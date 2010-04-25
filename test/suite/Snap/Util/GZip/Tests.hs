{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Util.GZip.Tests
  ( tests ) where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import           Control.Exception hiding (assert)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.IORef
import           Data.Iteratee
import qualified Data.Map as Map
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic hiding (run)

import           Snap.Types
import           Snap.Internal.Http.Types
import           Snap.Iteratee
import           Snap.Test.Common ()
import           Snap.Util.GZip


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testIdentity1
        , testIdentity2
        , testIdentity3
        , testCompositionDoesn'tExplode
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
gzipHdrs, badHdrs, compressHdrs, emptyHdrs :: Headers
emptyHdrs = Map.empty
gzipHdrs = setHeader "Accept-Encoding" "froz,gzip" emptyHdrs
badHdrs = setHeader "Accept-Encoding" "*&%^&^$%&%&*^\023" emptyHdrs
compressHdrs = setHeader "Accept-Encoding" "compress" emptyHdrs


------------------------------------------------------------------------------
mkGzipRq :: IO Request
mkGzipRq = do
    enum <- newIORef $ SomeEnumerator return

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False gzipHdrs
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty


------------------------------------------------------------------------------
mkCompressRq :: IO Request
mkCompressRq = do
    enum <- newIORef $ SomeEnumerator return

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False compressHdrs
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty


------------------------------------------------------------------------------
mkBadRq :: IO Request
mkBadRq = do
    enum <- newIORef $ SomeEnumerator return

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False badHdrs
                  enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty

------------------------------------------------------------------------------
goGZip, goCompress, goBad :: Snap a -> IO (Request,Response)
goGZip m = do
    gzipRq <- mkGzipRq
    run $ runSnap m gzipRq

goCompress m = do
    compressRq <- mkCompressRq
    run $ runSnap m compressRq

goBad m = do
    badRq <- mkBadRq
    run $ runSnap m badRq

------------------------------------------------------------------------------
textPlain :: L.ByteString -> Snap ()
textPlain s = modifyResponse $
              setResponseBody (enumLBS s) .
              setContentType "text/plain"


------------------------------------------------------------------------------
binary :: L.ByteString -> Snap ()
binary s = modifyResponse $
           setResponseBody (enumLBS s) .
           setContentType "application/octet-stream"


------------------------------------------------------------------------------
testIdentity1 :: Test
testIdentity1 = testProperty "identity1" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (_,rsp) <- liftQ $ goGZip (withCompression $ textPlain s)
        let body = rspBody rsp

        c <- liftQ $
             body stream2stream >>= run >>= return . fromWrap

        let s1 = GZip.decompress c
        assert $ s == s1

testCompositionDoesn'tExplode :: Test
testCompositionDoesn'tExplode =
    testProperty "testCompositionDoesn'tExplode" $
                 monadicIO $
                 forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (_,rsp) <- liftQ $ goGZip (withCompression $ withCompression $ textPlain s)
        let body = rspBody rsp

        c <- liftQ $
             body stream2stream >>= run >>= return . fromWrap

        let s1 = GZip.decompress c
        assert $ s == s1



testIdentity2 :: Test
testIdentity2 = testProperty "identity2" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (_,rsp2) <- liftQ $ goCompress (withCompression $ textPlain s)
        let body2 = rspBody rsp2

        c2 <- liftQ $
              body2 stream2stream >>= run >>= return . fromWrap

        let s2 = Zlib.decompress c2
        assert $ s == s2


testIdentity3 :: Test
testIdentity3 = testProperty "identity3" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (_,rsp3) <- liftQ $ goGZip (withCompression $ binary s)
        let body3 = rspBody rsp3

        s3 <- liftQ $
              body3 stream2stream >>= run >>= return . fromWrap

        assert $ s == s3



------------------------------------------------------------------------------
testBadHeaders :: Test
testBadHeaders = testProperty "bad headers" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = expectException $ do
        (_,rsp) <- goBad (withCompression $ textPlain s)
        let body = rspBody rsp

        body stream2stream >>= run >>= return . fromWrap
