{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

--------------------------------------------------------------------------------
-- | Helpers for running a 'Snap' web handler with compression.

module Snap.Util.GZip
  ( withCompression
  , withCompression'
  , noCompression
  , BadAcceptEncodingException
  , compressibleMimeTypes
  ) where

import           Control.Applicative              (Alternative ((<|>), many), Applicative ((*>), (<*), pure), (<$>))
import           Control.Exception                (Exception, throwIO)
import           Control.Monad                    (Functor (fmap), Monad ((>>), (>>=), return), MonadPlus (mplus), void, when)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Attoparsec.ByteString.Char8 (Parser, char, endOfInput, isAlpha_ascii, isDigit, skipSpace, string, takeWhile, takeWhile1)
import           Data.ByteString.Builder          (Builder)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as S (takeWhile)
import qualified Data.Char                        as Char (isSpace)
import           Data.Maybe                       (Maybe (Just, Nothing), fromMaybe, isJust, maybe)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set (fromList, member)
import           Data.Typeable                    (Typeable)
import           Prelude                          (Either (..), Eq (..), IO, Show (show), id, not, ($), ($!), (&&), (++), (||))
import           Snap.Core                        (MonadSnap, clearContentLength, finishWith, getHeader, getRequest, getResponse, modifyResponse, modifyResponseBody, setHeader)
import           Snap.Internal.Debug              (debug)
import           Snap.Internal.Parsing            (fullyParse)
import           System.IO.Streams                (OutputStream)
import qualified System.IO.Streams                as Streams (compressBuilder, gzipBuilder)

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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"/\" M.empty >> T.addHeader \"Accept-Encoding\" \"gzip,deflate\"
-- ghci> let h = 'Snap.Core.modifyResponse' ('Snap.Core.setContentType' \"text\/plain\") >> 'Snap.Core.writeBS' \"some text\"
-- ghci> T.runHandler r h
-- HTTP\/1.1 200 OK
-- content-type: text\/plain
-- server: Snap\/test
-- date: Fri, 08 Aug 2014 15:40:45 GMT
--
-- some text
-- ghci> T.runHandler r ('withCompression' h)
-- HTTP\/1.1 200 OK
-- content-type: text\/plain
-- vary: Accept-Encoding
-- content-encoding: gzip
-- server: Snap\/test
-- date: Fri, 08 Aug 2014 15:40:10 GMT
--
--
-- @
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

        chooseType Nothing types

    chooseType !m []               = maybe (return $! ()) id m
    chooseType !_ ("gzip":_)       = gzipCompression "gzip"
    chooseType !m ("deflate":xs)   =
        chooseType (m `mplus` Just (compressCompression "deflate")) xs

    chooseType !_ ("x-gzip":_)     = gzipCompression "x-gzip"
    chooseType !m ("x-deflate":xs) =
        chooseType (m `mplus` Just (compressCompression "x-deflate")) xs
    chooseType !m (_:xs)           = chooseType m xs


------------------------------------------------------------------------------
-- | Turn off compression by setting \"Content-Encoding: identity\" in the
-- response headers. 'withCompression' is a no-op when a content-encoding is
-- already set.
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
          modifyResponseBody gcompress r


------------------------------------------------------------------------------
compressCompression :: MonadSnap m => ByteString -> m ()
compressCompression ce = modifyResponse f
  where
    f r = setHeader "Content-Encoding" ce    $
          setHeader "Vary" "Accept-Encoding" $
          clearContentLength                 $
          modifyResponseBody ccompress r


------------------------------------------------------------------------------
gcompress :: (OutputStream Builder -> IO (OutputStream Builder))
          -> OutputStream Builder
          -> IO (OutputStream Builder)
gcompress body stream = Streams.gzipBuilder 5 stream >>= body


------------------------------------------------------------------------------
ccompress :: (OutputStream Builder -> IO (OutputStream Builder))
          -> OutputStream Builder
          -> IO (OutputStream Builder)
ccompress body stream = Streams.compressBuilder 5 stream >>= body


------------------------------------------------------------------------------
-- We're not gonna bother with quality values; we'll do gzip or compress in
-- that order.
acceptParser :: Parser [ByteString]
acceptParser = do
    xs <- ((:[]) <$> encoding) <|> (return $! [])
    ys <- many (char ',' *> encoding)
    endOfInput
    return $! xs ++ ys
  where
    encoding = skipSpace *> c <* skipSpace

    c = do
        x <- coding
        qvalue <|> (return $! ())
        return x

    qvalue = do
        skipSpace
        void $! char ';'
        skipSpace
        void $! char 'q'
        skipSpace
        void $! char '='
        float
        return $! ()

    coding = string "*" <|> takeWhile1 isCodingChar

    isCodingChar ch = isDigit ch || isAlpha_ascii ch || ch == '-' || ch == '_'

    float = takeWhile isDigit >>
            (char '.' >> takeWhile isDigit >> (pure $! ())) <|> (pure $! ())


------------------------------------------------------------------------------
-- | Thrown when the 'Accept-Encoding' request header has invalid format.
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
