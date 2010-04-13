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
gzipHdrs = setHeader "Accept-Encoding" "gzip" emptyHdrs
badHdrs = setHeader "Accept-Encoding" "*&%^&^$%&%&*^\023" emptyHdrs
compressHdrs = setHeader "Accept-Encoding" "compress" emptyHdrs


------------------------------------------------------------------------------
gzipRq :: Request
gzipRq = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False gzipHdrs
                 return Nothing GET (1,1) [] "/" "/" "/" "" Map.empty


------------------------------------------------------------------------------
compressRq :: Request
compressRq = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False compressHdrs
                     return Nothing GET (1,1) [] "/" "/" "/" "" Map.empty


------------------------------------------------------------------------------
badRq :: Request
badRq = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False badHdrs
                     return Nothing GET (1,1) [] "/" "/" "/" "" Map.empty

------------------------------------------------------------------------------
goGZip, goCompress, goBad :: Snap a -> IO (Request,Response)
goGZip m = run $ runSnap m gzipRq
goCompress m = run $ runSnap m compressRq
goBad m = run $ runSnap m badRq

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
