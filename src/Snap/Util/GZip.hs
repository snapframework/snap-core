{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Snap.Util.GZip
( withCompression
, withCompression' ) where

import           Blaze.ByteString.Builder
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import           Control.Concurrent
import           Control.Applicative hiding (many)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.Char8 hiding (Done)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Char8 (ByteString)
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Typeable
import           Prelude hiding (catch, takeWhile)

----------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Internal.Parsing
import           Snap.Iteratee
import qualified Snap.Iteratee as I
import           Snap.Types


------------------------------------------------------------------------------
-- | Runs a 'Snap' web handler with compression if available.
--
-- If the client has indicated support for @gzip@ or @compress@ in its
-- @Accept-Encoding@ header, and the @Content-Type@ in the response is one of
-- the following types:
--
--   * @application/x-javascript@
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
       let mbCt = getHeader "Content-Type" resp

       debug $ "withCompression', content-type is " ++ show mbCt

       case mbCt of
         (Just ct) -> when (Set.member ct mimeTable) chkAcceptEncoding
         _         -> return $! ()


    getResponse >>= finishWith

  where
    chkAcceptEncoding = do
        req <- getRequest
        debug $ "checking accept-encoding"
        let mbAcc = getHeader "Accept-Encoding" req
        debug $ "accept-encoding is " ++ show mbAcc
        let s = fromMaybe "" mbAcc

        types <- liftIO $ parseAcceptEncoding s

        chooseType types


    chooseType []               = return $! ()
    chooseType ("gzip":_)       = gzipCompression "gzip"
    chooseType ("compress":_)   = compressCompression "compress"
    chooseType ("x-gzip":_)     = gzipCompression "x-gzip"
    chooseType ("x-compress":_) = compressCompression "x-compress"
    chooseType (_:xs)           = chooseType xs


------------------------------------------------------------------------------
-- private following
------------------------------------------------------------------------------


------------------------------------------------------------------------------
compressibleMimeTypes :: Set ByteString
compressibleMimeTypes = Set.fromList [ "application/x-font-truetype"
                                     , "application/x-javascript"
                                     , "text/css"
                                     , "text/html"
                                     , "text/javascript"
                                     , "text/plain"
                                     , "text/xml" ]




------------------------------------------------------------------------------
gzipCompression :: MonadSnap m => ByteString -> m ()
gzipCompression ce = modifyResponse f
  where
    f = setHeader "Content-Encoding" ce .
        setHeader "Vary" "Accept-Encoding" .
        clearContentLength .
        modifyResponseBody gcompress


------------------------------------------------------------------------------
compressCompression :: MonadSnap m => ByteString -> m ()
compressCompression ce = modifyResponse f
  where
    f = setHeader "Content-Encoding" ce .
        setHeader "Vary" "Accept-Encoding" .
        clearContentLength .
        modifyResponseBody ccompress


------------------------------------------------------------------------------
-- FIXME: use zlib-bindings
gcompress :: forall a . Enumerator Builder IO a
          -> Enumerator Builder IO  a
gcompress = compressEnumerator GZip.compress


------------------------------------------------------------------------------
ccompress :: forall a . Enumerator Builder IO a
          -> Enumerator Builder IO a
ccompress = compressEnumerator Zlib.compress


------------------------------------------------------------------------------
compressEnumerator :: forall a .
                      (L.ByteString -> L.ByteString)
                   -> Enumerator Builder IO a
                   -> Enumerator Builder IO a
compressEnumerator compFunc enum' origStep = do
    let iter = joinI $ I.map fromByteString origStep
    step <- lift $ runIteratee iter
    writeEnd <- liftIO $ newChan
    readEnd  <- liftIO $ newChan
    tid      <- liftIO $ forkIO $ threadProc readEnd writeEnd

    let enum = mapEnum fromByteString toByteString enum'
    let outEnum = enum (f readEnd writeEnd tid step)
    mapIter toByteString fromByteString outEnum

  where
    --------------------------------------------------------------------------
    streamFinished :: Stream ByteString -> Bool
    streamFinished EOF        = True
    streamFinished (Chunks _) = False


    --------------------------------------------------------------------------
    consumeSomeOutput :: Chan (Either SomeException (Stream ByteString))
                      -> Step ByteString IO a
                      -> Iteratee ByteString IO (Step ByteString IO a)
    consumeSomeOutput writeEnd step = do
        e <- lift $ isEmptyChan writeEnd
        if e
          then return step
          else do
            ech <- lift $ readChan writeEnd
            either throwError
                   (\ch -> do
                        step' <- checkDone (\k -> lift $ runIteratee $ k ch)
                                           step
                        consumeSomeOutput writeEnd step')
                   ech

    --------------------------------------------------------------------------
    consumeRest :: Chan (Either SomeException (Stream ByteString))
                -> Step ByteString IO a
                -> Iteratee ByteString IO a
    consumeRest writeEnd step = do
        ech <- lift $ readChan writeEnd
        either throwError
               (\ch -> do
                   step' <- checkDone (\k -> lift $ runIteratee $ k ch) step
                   if (streamFinished ch)
                      then returnI step'
                      else consumeRest writeEnd step')
               ech

    --------------------------------------------------------------------------
    f _ _ _ (Error e) = Error e
    f _ _ _ (Yield x _) = Yield x EOF
    f readEnd writeEnd tid st@(Continue k) = Continue $ \ch ->
        case ch of
          EOF -> do
            lift $ writeChan readEnd Nothing
            x <- consumeRest writeEnd st
            lift $ killThread tid
            return x

          (Chunks xs) -> do
            mapM_ (lift . writeChan readEnd . Just) xs
            step' <- consumeSomeOutput writeEnd (Continue k)
            returnI $ f readEnd writeEnd tid step'


    --------------------------------------------------------------------------
    threadProc :: Chan (Maybe ByteString)
               -> Chan (Either SomeException (Stream ByteString))
               -> IO ()
    threadProc readEnd writeEnd = do
        stream <- getChanContents readEnd

        let bs = L.fromChunks $ streamToChunks stream
        let output = L.toChunks $ compFunc bs

        runIt output `catch` \(e::SomeException) ->
            writeChan writeEnd $ Left e

      where
        runIt (x:xs) = do
            writeChan writeEnd (toChunk x) >> runIt xs

        runIt []     = do
            writeChan writeEnd $ Right EOF

    --------------------------------------------------------------------------
    streamToChunks []            = []
    streamToChunks (Nothing:_)   = []
    streamToChunks ((Just x):xs) = x:(streamToChunks xs)


    --------------------------------------------------------------------------
    toChunk = Right . Chunks . (:[])


------------------------------------------------------------------------------
-- We're not gonna bother with quality values; we'll do gzip or compress in
-- that order.
acceptParser :: Parser [ByteString]
acceptParser = do
    xs <- option [] $ (:[]) <$> encoding
    ys <- many (char ',' *> encoding)
    endOfInput
    return $ xs ++ ys
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

