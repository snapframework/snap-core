{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Snap.Util.GZip
( withCompression
, withCompression' ) where

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
import           Data.Iteratee.WrappedByteString
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Typeable
import           Prelude hiding (catch, takeWhile)

------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Internal.Parsing
import           Snap.Iteratee hiding (Enumerator)
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
-- @Content-Length@ will be cleared if it was set. (We can't process the stream
-- in O(1) space if the length is known beforehand.)
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
gcompress :: forall a . Enumerator a -> Enumerator a
gcompress = compressEnumerator GZip.compress


------------------------------------------------------------------------------
ccompress :: forall a . Enumerator a -> Enumerator a
ccompress = compressEnumerator Zlib.compress


------------------------------------------------------------------------------
compressEnumerator :: forall a .
                      (L.ByteString -> L.ByteString)
                   -> Enumerator a
                   -> Enumerator a
compressEnumerator compFunc enum iteratee = do
    writeEnd <- newChan
    readEnd  <- newChan
    tid      <- forkIO $ threadProc readEnd writeEnd

    enum (IterateeG $ f readEnd writeEnd tid iteratee)

  where
    --------------------------------------------------------------------------
    streamFinished :: Stream -> Bool
    streamFinished (EOF _)   = True
    streamFinished (Chunk _) = False


    --------------------------------------------------------------------------
    consumeSomeOutput :: Chan Stream
                      -> Iteratee IO a
                      -> IO (Iteratee IO a)
    consumeSomeOutput writeEnd iter = do
        e <- isEmptyChan writeEnd
        if e
          then return iter
          else do
            ch <- readChan writeEnd

            iter' <- liftM liftI $ runIter iter ch
            consumeSomeOutput writeEnd iter'


    --------------------------------------------------------------------------
    consumeRest :: Chan Stream
                -> Iteratee IO a
                -> IO (IterV IO a)
    consumeRest writeEnd iter = do
        ch <- readChan writeEnd

        iv <- runIter iter ch
        if (streamFinished ch)
           then return iv
           else consumeRest writeEnd $ liftI iv


    --------------------------------------------------------------------------
    f readEnd writeEnd tid i (EOF Nothing) = do
        writeChan readEnd Nothing
        x <- consumeRest writeEnd i
        killThread tid
        return x

    f _ _ tid _ (EOF (Just e)) = do
        killThread tid
        return $ Cont undefined (Just e)

    f readEnd writeEnd tid i (Chunk s') = do
        let s = unWrap s'
        writeChan readEnd $ Just s
        i' <- consumeSomeOutput writeEnd i
        return $ Cont (IterateeG $ f readEnd writeEnd tid i') Nothing


    --------------------------------------------------------------------------
    threadProc :: Chan (Maybe ByteString)
               -> Chan Stream
               -> IO ()
    threadProc readEnd writeEnd = do
        stream <- getChanContents readEnd

        let bs = L.fromChunks $ streamToChunks stream
        let output = L.toChunks $ compFunc bs

        runIt output `catch` \(e::SomeException) ->
            writeChan writeEnd $ EOF (Just $ Err $ show e)

      where
        runIt (x:xs) = do
            writeChan writeEnd (toChunk x) >> runIt xs

        runIt []     = do
            writeChan writeEnd $ EOF Nothing
    --------------------------------------------------------------------------
    streamToChunks []            = []
    streamToChunks (Nothing:_)   = []
    streamToChunks ((Just x):xs) = x:(streamToChunks xs)


    --------------------------------------------------------------------------
    toChunk = Chunk . WrapBS


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

