{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Iteratee.Tests
  ( tests ) where

import qualified Control.Exception as E
import           Control.Exception hiding (try, assert)
import           Control.Monad.Identity
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Test.Framework 
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic hiding (run)

import           Snap.Iteratee
import           Snap.Test.Common ()

liftQ :: forall a m . (Monad m) => m a -> PropertyM m a
liftQ = QC.run

tests :: [Test]
tests = [ testEnumBS
        , testEnumLBS
        , testBuffer
        , testTakeExactly1
        , testTakeExactly2
        , testTakeExactly3
        ]

testEnumBS :: Test
testEnumBS = testProperty "enumBS" prop
  where
    prop :: S.ByteString -> Bool
    prop s = (S.concat $ L.toChunks $ fromWrap $ runIdentity (run iter)) == s
      where
        iter = runIdentity $ enumBS s stream2stream

testEnumLBS :: Test
testEnumLBS = testProperty "enumLBS" prop
  where
    prop :: L.ByteString -> Bool
    prop s = fromWrap (runIdentity (run iter)) == s
      where
        iter = runIdentity $ enumLBS s stream2stream


testBuffer :: Test
testBuffer = testProperty "testBuffer" prop
  where
    prop s = s /= L.empty ==> fromWrap (runIdentity (run iter)) == s'
      where
        s' = L.take 20000 $ L.cycle s
        iter = runIdentity $ enumLBS s' (bufferIteratee stream2stream)

--enumBS "abcdefg" (joinI (takeExactly 8 stream2stream)) >>= run >>= return . fromWrap


testTakeExactly1 :: Test
testTakeExactly1 = testProperty "short stream" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        e <- liftQ $ E.try $ doIter >>= run >>= return . fromWrap
        case e of
          Left (_::SomeException) -> return ()
          Right _ -> fail "expected exception"

      where
        doIter = enumLBS s (joinI (takeExactly (n+1) stream2stream))

        n = fromIntegral $ L.length s


testTakeExactly2 :: Test
testTakeExactly2 = testProperty "exact stream" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        e <- liftQ $ doIter >>= run >>= return . fromWrap
        assert $ e == s

      where
        doIter = enumLBS s (joinI (takeExactly n stream2stream))

        n = fromIntegral $ L.length s


testTakeExactly3 :: Test
testTakeExactly3 = testProperty "long stream" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        e <- liftQ $ doIter >>= run >>= return . fromWrap
        assert $ e == L.take (fromIntegral n) s

      where
        doIter = enumLBS s (joinI (takeExactly n stream2stream))

        n = fromIntegral $ L.length s
