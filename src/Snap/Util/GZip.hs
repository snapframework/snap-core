{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Snap.Util.GZip
( withCompression
, withCompression'
, noCompression ) where

import           Blaze.ByteString.Builder
import qualified Codec.Zlib.Enum as Z
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.Char8
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Char as Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Typeable
import           Prelude hiding (catch, takeWhile)


----------------------------------------------------------------------------
import           Snap.Core
import           Snap.Internal.Debug
import           Snap.Internal.Parsing
import           Snap.Iteratee
import qualified Snap.Iteratee as I


------------------------------------------------------------------------------
-- | Runs a 'Snap' web handler with compression if available.
--
-- If the client has indicated support for @gzip@ or @deflate@ in its
-- @Accept-Encoding@ header, and the @Content-Type@ in the response is one of
-- the following types:
--
--   * @application/x-javascript@
--
--   * @application/json@
--
--   * @text/css@
--
--   * @text/html@
--
--   * @text/javascript@
--
--   * @text/plain@
--
--   * @text/xml@
--
--   * @application/x-font-truetype@
--
-- Then the given handler's output stream will be compressed,
-- @Content-Encoding@ will be set in the output headers, and the
-- @Content-Length@ will be cleared if it was set. (We can't process the
-- stream in O(1) space if the length is known beforehand.)
--
-- The wrapped handler will be run to completion, and then the 'Response'
-- that's contained within the 'Snap' monad state will be passed to
-- 'finishWith' to prevent further processing.
--
withCompression :: MonadSnap m
                => m a   -- ^ the web handler to run
                -> m ()
withCompression = withCompression' compressibleMimeTypes


------------------------------------------------------------------------------
-- | The same as 'withCompression', with control over which MIME types to
-- compress.
withCompression' :: MonadSnap m
                 => Set ByteString
                    -- ^ set of compressible MIME types
                 -> m a
                    -- ^ the web handler to run
                 -> m ()
withCompression' mimeTable action = do
    _    <- action
    resp <- getResponse

    -- If a content-encoding is already set, do nothing. This prevents
    -- "withCompression $ withCompression m" from ruining your day.
    when (not $ isJust $ getHeader "Content-Encoding" resp) $ do
       let mbCt = fmap chop $ getHeader "Content-Type" resp

       debug $ "withCompression', content-type is " ++ show mbCt

       case mbCt of
         (Just ct) -> when (Set.member ct mimeTable) chkAcceptEncoding
         _         -> return $! ()


    getResponse >>= finishWith

  where
    chop = S.takeWhile (\c -> c /= ';' && not (Char.isSpace c))

    chkAcceptEncoding = do
        req <- getRequest
        debug $ "checking accept-encoding"
        let mbAcc = getHeader "Accept-Encoding" req
        debug $ "accept-encoding is " ++ show mbAcc
        let s = fromMaybe "" mbAcc

        types <- liftIO $ parseAcceptEncoding s

        chooseType types


    chooseType []              = return $! ()
    chooseType ("gzip":_)      = gzipCompression "gzip"
    chooseType ("deflate":_)   = compressCompression "deflate"
    chooseType ("x-gzip":_)    = gzipCompression "x-gzip"
    chooseType ("x-deflate":_) = compressCompression "x-deflate"
    chooseType (_:xs)          = chooseType xs


------------------------------------------------------------------------------
-- | Turn off compression by setting \"Content-Encoding: identity\" in the
-- response headers.
noCompression :: MonadSnap m => m ()
noCompression = modifyResponse $ setHeader "Content-Encoding" "identity"


------------------------------------------------------------------------------
-- private following
------------------------------------------------------------------------------


------------------------------------------------------------------------------
compressibleMimeTypes :: Set ByteString
compressibleMimeTypes = Set.fromList [ "application/x-font-truetype"
                                     , "application/x-javascript"
                                     , "application/json"
                                     , "text/css"
                                     , "text/html"
                                     , "text/javascript"
                                     , "text/plain"
                                     , "text/xml" ]




------------------------------------------------------------------------------
gzipCompression :: MonadSnap m => ByteString -> m ()
gzipCompression ce = modifyResponse f
  where
    f r = setHeader "Content-Encoding" ce    $
          setHeader "Vary" "Accept-Encoding" $
          clearContentLength                 $
          modifyResponseBody (gcompress (getBufferingMode r)) r


------------------------------------------------------------------------------
compressCompression :: MonadSnap m => ByteString -> m ()
compressCompression ce = modifyResponse f
  where
    f r = setHeader "Content-Encoding" ce    $
          setHeader "Vary" "Accept-Encoding" $
          clearContentLength                 $
          modifyResponseBody (ccompress (getBufferingMode r)) r


------------------------------------------------------------------------------
gcompress :: Bool               -- ^ buffer?
          -> forall a . Enumerator Builder IO a
          -> Enumerator Builder IO a
gcompress buffer e st = e $$ iFinal
  where
    i0     = returnI st
    iNoB   = mapFlush                =$ i0
    iZNoB  = Z.gzip                  =$ iNoB

    iB     = I.map fromByteString    =$ i0
    iZ     = Z.gzip                  =$ iB

    iFinal = enumBuilderToByteString =$ if buffer then iZ else iZNoB

    mapFlush :: Monad m => Enumeratee ByteString Builder m b
    mapFlush = I.map ((`mappend` flush) . fromByteString)


------------------------------------------------------------------------------
ccompress :: Bool               -- ^ buffer?
          -> forall a . Enumerator Builder IO a
          -> Enumerator Builder IO a
ccompress buffer e st = e $$ iFinal
  where
    i0     = returnI st
    iNoB   = mapFlush                         =$ i0
    iZNoB  = Z.compress 5 Z.defaultWindowBits =$ iNoB

    iB     = I.map fromByteString             =$ i0
    iZ     = Z.compress 5 Z.defaultWindowBits =$ iB

    iFinal = enumBuilderToByteString =$ if buffer then iZ else iZNoB

    mapFlush :: Monad m => Enumeratee ByteString Builder m b
    mapFlush = I.map ((`mappend` flush) . fromByteString)


------------------------------------------------------------------------------
-- We're not gonna bother with quality values; we'll do gzip or compress in
-- that order.
acceptParser :: Parser [ByteString]
acceptParser = do
    xs <- option [] $ (:[]) <$> encoding
    ys <- many (char ',' *> encoding)
    endOfInput
    return $! xs ++ ys
  where
    encoding = skipSpace *> c <* skipSpace

    c = do
        x <- coding
        option () qvalue
        return x

    qvalue = do
        skipSpace
        char ';'
        skipSpace
        char 'q'
        skipSpace
        char '='
        float
        return ()

    coding = string "*" <|> takeWhile isCodingChar

    isCodingChar ch = isDigit ch || isAlpha_ascii ch || ch == '-' || ch == '_'

    float = takeWhile isDigit >>
            option () (char '.' >> takeWhile isDigit >> pure ())


------------------------------------------------------------------------------
data BadAcceptEncodingException = BadAcceptEncodingException
   deriving (Typeable)


------------------------------------------------------------------------------
instance Show BadAcceptEncodingException where
    show BadAcceptEncodingException = "bad 'accept-encoding' header"


------------------------------------------------------------------------------
instance Exception BadAcceptEncodingException


------------------------------------------------------------------------------
parseAcceptEncoding :: ByteString -> IO [ByteString]
parseAcceptEncoding s =
    case r of
      Left _ -> throwIO BadAcceptEncodingException
      Right x -> return x
  where
    r = fullyParse s acceptParser

