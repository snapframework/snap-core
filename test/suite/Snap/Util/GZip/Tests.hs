{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Util.GZip.Tests
  ( tests ) where

import           Blaze.ByteString.Builder
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import           Control.Exception hiding (assert)
import           Control.Monad (liftM)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Digest.Pure.MD5
import           Data.IORef
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Serialize (encode)
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic hiding (run)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import           Snap.Types
import           Snap.Internal.Http.Types
import           Snap.Iteratee
import           Snap.Test.Common ()
import           Snap.Util.GZip


stream2stream
  :: Iteratee Builder IO L.ByteString
stream2stream = liftM (toLazyByteString . mconcat) consume

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testIdentity1
        , testIdentity1_charset
        , testIdentity2
        , testIdentity3
        , testIdentity4
        , testIdentity5
        , testNoHeaders
        , testNoAcceptEncoding
        , testNopWhenContentEncodingSet
        , testCompositionDoesn'tExplode
        , testGzipLotsaChunks
        , testNoCompression
        , testBadHeaders ]


------------------------------------------------------------------------------
expectException :: IO a -> PropertyM IO ()
expectException m = do
    e <- liftQ $ try m
    case e of
      Left (z::SomeException)  -> (show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"


------------------------------------------------------------------------------
liftQ :: forall a m . (Monad m) => m a -> PropertyM m a
liftQ = QC.run


------------------------------------------------------------------------------
gzipHdrs, xGzipHdrs, badHdrs, compressHdrs, xCompressHdrs, emptyHdrs :: Headers
emptyHdrs = Map.empty
gzipHdrs = setHeader "Accept-Encoding" "froz,gzip, x-gzip" emptyHdrs
xGzipHdrs = setHeader "Accept-Encoding" "x-gzip;q=1.0" emptyHdrs
badHdrs = setHeader "Accept-Encoding" "*&%^&^$%&%&*^\023" emptyHdrs
compressHdrs = setHeader "Accept-Encoding" "compress" emptyHdrs
xCompressHdrs = setHeader "Accept-Encoding" "x-compress" emptyHdrs



------------------------------------------------------------------------------
mkNoHeaders :: IO Request
mkNoHeaders = do
    enum <- newIORef $ SomeEnumerator returnI

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False emptyHdrs
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty


mkGzipRq :: IO Request
mkGzipRq = do
    enum <- newIORef $ SomeEnumerator returnI

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False gzipHdrs
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty

mkXGzipRq :: IO Request
mkXGzipRq = do
    enum <- newIORef $ SomeEnumerator returnI

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False xGzipHdrs
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty



------------------------------------------------------------------------------
mkCompressRq :: IO Request
mkCompressRq = do
    enum <- newIORef $ SomeEnumerator returnI

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False compressHdrs
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty

mkXCompressRq :: IO Request
mkXCompressRq = do
    enum <- newIORef $ SomeEnumerator returnI

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False xCompressHdrs
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty



------------------------------------------------------------------------------
mkBadRq :: IO Request
mkBadRq = do
    enum <- newIORef $ SomeEnumerator returnI

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False badHdrs
                  enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty

------------------------------------------------------------------------------
seqSnap :: Snap a -> Snap a
seqSnap m = do
    !x <- m
    return $! x `seq` x


------------------------------------------------------------------------------
goGeneric :: IO Request -> Snap a -> IO (Request, Response)
goGeneric mkRq m = do
    rq <- mkRq
    run_ $! runSnap (seqSnap m) d d rq
  where
    d = (const $ return ())


goGZip, goCompress, goXGZip     :: Snap a -> IO (Request,Response)
goNoHeaders, goXCompress, goBad :: Snap a -> IO (Request,Response)

goGZip      = goGeneric mkGzipRq
goCompress  = goGeneric mkCompressRq
goXGZip     = goGeneric mkXGzipRq
goXCompress = goGeneric mkXCompressRq
goBad       = goGeneric mkBadRq
goNoHeaders = goGeneric mkNoHeaders

------------------------------------------------------------------------------
noContentType :: L.ByteString -> Snap ()
noContentType s = modifyResponse $ setResponseBody $
                  enumBuilder $ fromLazyByteString s

------------------------------------------------------------------------------
withContentType :: ByteString -> L.ByteString -> Snap ()
withContentType ct body =
    modifyResponse $
    setResponseBody (enumBuilder $ fromLazyByteString body) .
    setContentType ct

------------------------------------------------------------------------------
textPlain :: L.ByteString -> Snap ()
textPlain = withContentType "text/plain"


------------------------------------------------------------------------------
binary :: L.ByteString -> Snap ()
binary = withContentType "application/octet-stream"


------------------------------------------------------------------------------
testNoHeaders :: Test
testNoHeaders = testProperty "gzip/noheaders" $
                monadicIO $
                forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        -- if there's no content-type, withCompression should be a no-op
        (!_,!rsp) <- liftQ $ goNoHeaders (seqSnap $ withCompression
                                                  $ noContentType s)
        assert $ getHeader "Content-Encoding" rsp == Nothing
        assert $ getHeader "Vary" rsp == Nothing
        let body = rspBodyToEnum $ rspBody rsp

        c <- liftQ $
             runIteratee stream2stream >>= run_ . body

        assert $ s == c


------------------------------------------------------------------------------
testNoAcceptEncoding :: Test
testNoAcceptEncoding = testProperty "gzip/noAcceptEncoding" $
                       monadicIO $
                       forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        -- if there's no accept-encoding, withCompression should be a no-op
        (!_,!rsp) <- liftQ $ goNoHeaders (seqSnap $ withCompression
                                                  $ textPlain s)
        assert $ getHeader "Content-Encoding" rsp == Nothing
        assert $ getHeader "Vary" rsp == Nothing
        let body = rspBodyToEnum $ rspBody rsp

        c <- liftQ $
             runIteratee stream2stream >>= run_ . body

        assert $ s == c


------------------------------------------------------------------------------
testIdentity1 :: Test
testIdentity1 = testProperty "gzip/identity1" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp) <- liftQ $ goGZip (seqSnap $ withCompression $ textPlain s)
        assert $ getHeader "Content-Encoding" rsp == Just "gzip"
        assert $ getHeader "Vary" rsp == Just "Accept-Encoding"
        let body = rspBodyToEnum $ rspBody rsp

        c <- liftQ $
             runIteratee stream2stream >>= run_ . body

        let s1 = GZip.decompress c
        assert $ s == s1


------------------------------------------------------------------------------
testIdentity1_charset :: Test
testIdentity1_charset = testProperty "gzip/identity1_charset" $
                        monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp) <- liftQ $
                     goGZip (seqSnap $ withCompression $
                             withContentType "text/plain; charset=utf-8" s)
        assert $ getHeader "Content-Encoding" rsp == Just "gzip"
        assert $ getHeader "Vary" rsp == Just "Accept-Encoding"
        let body = rspBodyToEnum $ rspBody rsp

        c <- liftQ $
             runIteratee stream2stream >>= run_ . body

        let s1 = GZip.decompress c
        assert $ s == s1


------------------------------------------------------------------------------
testIdentity2 :: Test
testIdentity2 = testProperty "gzip/identity2" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp) <- liftQ $ goCompress (seqSnap $ withCompression $ textPlain s)

        assert $ getHeader "Content-Encoding" rsp == Just "compress"
        assert $ getHeader "Vary" rsp == Just "Accept-Encoding"
        let body = rspBodyToEnum $ rspBody rsp

        c <- liftQ $
              runIteratee stream2stream >>= run_ . body

        let s' = Zlib.decompress c
        assert $ s == s'


------------------------------------------------------------------------------
testIdentity3 :: Test
testIdentity3 = testProperty "gzip/identity3" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp3) <- liftQ $ goGZip (seqSnap $ withCompression $ binary s)
        let body3 = rspBodyToEnum $ rspBody rsp3

        s3 <- liftQ $
              runIteratee stream2stream >>= run_ . body3

        assert $ s == s3


------------------------------------------------------------------------------
testIdentity4 :: Test
testIdentity4 = testProperty "gzip/identity4" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp) <- liftQ $ goXGZip (seqSnap $ withCompression $ textPlain s)
        assert $ getHeader "Content-Encoding" rsp == Just "x-gzip"
        let body = rspBodyToEnum $ rspBody rsp

        c <- liftQ $
             runIteratee stream2stream >>= run_ . body

        let s1 = GZip.decompress c
        assert $ s == s1


------------------------------------------------------------------------------
testIdentity5 :: Test
testIdentity5 = testProperty "gzip/identity5" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp2) <- liftQ $ goXCompress (seqSnap $ withCompression $ textPlain s)

        assert $ getHeader "Content-Encoding" rsp2 == Just "x-compress"
        let body2 = rspBodyToEnum $ rspBody rsp2

        c2 <- liftQ $
              runIteratee stream2stream >>= run_ . body2

        let s2 = Zlib.decompress c2
        assert $ s == s2


------------------------------------------------------------------------------
testBadHeaders :: Test
testBadHeaders = testProperty "gzip/bad headers" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = expectException $ do
        (!_,!rsp) <- goBad (seqSnap $ withCompression $ textPlain s)
        let body = rspBodyToEnum $ rspBody rsp

        runIteratee stream2stream >>= run_ . body


------------------------------------------------------------------------------
testNopWhenContentEncodingSet :: Test
testNopWhenContentEncodingSet =
    testProperty "gzip/testNopWhenContentEncodingSet" $
                 monadicIO $
                 forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp) <- liftQ $ goGZip $ f s
        assert $ getHeader "Content-Encoding" rsp == Just "identity"

    f !s = seqSnap $ withCompression $ do
            modifyResponse $ setHeader "Content-Encoding" "identity"
            textPlain s


------------------------------------------------------------------------------
testCompositionDoesn'tExplode :: Test
testCompositionDoesn'tExplode =
    testProperty "gzip/testCompositionDoesn'tExplode" $
                 monadicIO $
                 forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp) <- liftQ $ goGZip (seqSnap $
                                     withCompression $
                                     withCompression $
                                     withCompression $ textPlain s)

        assert $ getHeader "Content-Encoding" rsp == Just "gzip"

        let body = rspBodyToEnum $ rspBody rsp

        c <- liftQ $
             runIteratee stream2stream >>= run_ . body

        let s1 = GZip.decompress c
        assert $ s == s1


------------------------------------------------------------------------------
testGzipLotsaChunks :: Test
testGzipLotsaChunks = testCase "gzip/lotsOfChunks" prop
  where
    prop = do
        let s = L.take 120000 $ L.fromChunks $ frobnicate "dshflkahdflkdhsaflk"
        (!_,!rsp) <- goGZip (seqSnap $ withCompression $ textPlain s)
        let body = rspBodyToEnum $ rspBody rsp

        c <- runIteratee stream2stream >>= run_ . body

        let s1 = GZip.decompress c
        H.assertBool "streams equal" $ s == s1

    
    -- in order to get incompressible text (so that we can test whether the
    -- gzip thread is streaming properly!) we'll iteratively md5 the source
    -- string
    frobnicate s = let s' = encode $ md5 $ L.fromChunks [s]
                   in (s:frobnicate s')


------------------------------------------------------------------------------
testNoCompression :: Test
testNoCompression = testProperty "gzip/noCompression" $
                    monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp) <- liftQ $ goGZip (seqSnap $ withCompression $
                                     (noCompression >> textPlain s))
        assert $ getHeader "Content-Encoding" rsp == Just "identity"
        let body = rspBodyToEnum $ rspBody rsp

        s1 <- liftQ $
             runIteratee stream2stream >>= run_ . body

        assert $ s == s1




