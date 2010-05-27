{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Iteratee.Tests
  ( tests ) where

import qualified Control.Exception as E
import           Control.Exception hiding (try, assert)
import           Control.Monad
import           Control.Monad.Identity
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Monoid
import           Data.Iteratee.WrappedByteString
import           Data.Word
import           Prelude hiding (drop, take)
import           Test.Framework 
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic hiding (run)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H
import           System.IO.Unsafe

import           Snap.Iteratee
import           Snap.Test.Common ()

liftQ :: forall a m . (Monad m) => m a -> PropertyM m a
liftQ = QC.run

expectException :: IO a -> PropertyM IO ()
expectException m = do
    e <- liftQ $ E.try m
    case e of
      Left (z::SomeException)  -> (show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"


tests :: [Test]
tests = [ testEnumBS
        , testEnumLBS
        , testBuffer
        , testBuffer2
        , testBuffer3
        , testBuffer4
        , testUnsafeBuffer
        , testUnsafeBuffer2
        , testUnsafeBuffer3
        , testUnsafeBuffer4
        , testUnsafeBuffer5
        , testTakeExactly1
        , testTakeExactly2
        , testTakeExactly3
        , testTakeNoMoreThan1
        , testTakeNoMoreThan2
        , testTakeNoMoreThan3
        , testCountBytes
        , testCountBytes2
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
        i = runIdentity $ bufferIteratee stream2stream
        iter = runIdentity $ enumLBS s' i


testBuffer2 :: Test
testBuffer2 = testCase "testBuffer2" prop
  where
    prop = do
        i <- bufferIteratee $ drop 4 >> stream2stream

        s <- enumLBS "abcdefgh" i >>= run >>= return . fromWrap
        H.assertEqual "s == 'efgh'" "efgh" s


testBuffer3 :: Test
testBuffer3 = testProperty "testBuffer3" prop
  where
    prop s = s /= L.empty ==> fromWrap (runIdentity (run iter)) == (L.take 19999 s')
      where
        s' = L.take 20000 $ L.cycle s
        ss = joinI $ take 19999 stream2stream
        i = runIdentity $ bufferIteratee (ss >>= \x -> drop 1 >> return x)
        iter = runIdentity $ enumLBS s' i

testBuffer4 :: Test
testBuffer4 = testProperty "testBuffer4" $
              monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        i <- liftQ $ bufferIteratee (stream2stream >> throwErr (Err "foo"))
        i' <- liftQ $ enumLBS s i
        expectException $ run i'

        j <- liftQ $ bufferIteratee (throwErr (Err "foo") >> stream2stream)
        j' <- liftQ $ enumLBS s j
        expectException $ run j'
        
        k <- liftQ $ enumErr "foo" j
        expectException $ run k


copyingStream2stream :: Iteratee IO (WrappedByteString Word8)
copyingStream2stream = IterateeG (step mempty)
  where
  step acc (Chunk (WrapBS ls))
    | S.null ls = return $ Cont (IterateeG (step acc)) Nothing
    | otherwise = do
          let !ls' = S.copy ls
          let !bs' = WrapBS $! ls'
          return $ Cont (IterateeG (step (acc `mappend` bs')))
                        Nothing

  step acc str        = return $ Done acc str


bufferAndRun :: Iteratee IO a -> L.ByteString -> IO a
bufferAndRun ii s = do
    (i,_) <- unsafeBufferIteratee ii
    iter  <- enumLBS s i
    run iter


testUnsafeBuffer :: Test
testUnsafeBuffer = testProperty "testUnsafeBuffer" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        pre $ s /= L.empty
        x <- liftQ $ bufferAndRun copyingStream2stream s'
        assert $ fromWrap x == s'

      where
        s' = L.take 20000 $ L.cycle s


testUnsafeBuffer2 :: Test
testUnsafeBuffer2 = testCase "testUnsafeBuffer2" prop
  where
    prop = do
        (i,_) <- unsafeBufferIteratee $ drop 4 >> copyingStream2stream

        s <- enumLBS "abcdefgh" i >>= run >>= return . fromWrap
        H.assertEqual "s == 'efgh'" "efgh" s


testUnsafeBuffer3 :: Test
testUnsafeBuffer3 = testProperty "testUnsafeBuffer3" $
                    monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        pre $ s /= L.empty
        x <- liftQ $ bufferAndRun (ss >>= \x -> drop 1 >> return x) s'

        assert $ fromWrap x == (L.take 19999 s')
      where
        s' = L.take 20000 $ L.cycle s
        ss = joinI $ take 19999 copyingStream2stream


testUnsafeBuffer4 :: Test
testUnsafeBuffer4 = testProperty "testUnsafeBuffer4" $
                    monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        (i,_) <- liftQ $
                 unsafeBufferIteratee (copyingStream2stream >> throwErr (Err "foo"))
        i' <- liftQ $ enumLBS s i
        expectException $ run i'

        (j,_) <- liftQ $
                 unsafeBufferIteratee (throwErr (Err "foo") >> copyingStream2stream)
        j' <- liftQ $ enumLBS s j
        expectException $ run j'
        
        k <- liftQ $ enumErr "foo" j
        expectException $ run k


testUnsafeBuffer5 :: Test
testUnsafeBuffer5 = testProperty "testUnsafeBuffer5" $
                    monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        pre $ s /= L.empty
        x <- liftQ $ bufferAndRun copyingStream2stream s
        assert $ fromWrap x == s


testTakeExactly1 :: Test
testTakeExactly1 = testProperty "short stream" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        expectException $ doIter >>= run >>= return . fromWrap

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


testTakeNoMoreThan1 :: Test
testTakeNoMoreThan1 = testProperty "takeNoMore: short stream" $
                      monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        s' <- liftQ $ doIter >>= run >>= return . fromWrap

        assert $ s == s'

      where
        doIter = enumLBS s (joinI (takeNoMoreThan (n+1) stream2stream))

        n = fromIntegral $ L.length s


testTakeNoMoreThan2 :: Test
testTakeNoMoreThan2 = testProperty "takeNoMore: exact stream" $
                      monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        e <- liftQ $ doIter >>= run >>= return . fromWrap
        assert $ e == s

      where
        doIter = enumLBS s (joinI (takeNoMoreThan n stream2stream))

        n = fromIntegral $ L.length s


testTakeNoMoreThan3 :: Test
testTakeNoMoreThan3 = testProperty "takeNoMoreLong" $
                      monadicIO $ forAllM arbitrary prop
  where
    prop :: (Int,L.ByteString) -> PropertyM IO ()
    prop (m,s) = do
        v <- liftQ $ enumLBS "" (joinI (takeNoMoreThan 0 stream2stream)) >>= run
        assert $ fromWrap v == ""

        if (L.null s || m == 0)
           then liftQ $ do
                     !v <- doIter >>= run
                     return ()
           else expectException $ doIter >>= run >>= return . fromWrap

        
      where
        doIter = enumLBS s (joinI (takeNoMoreThan (n-abs m) stream2stream))
        n = fromIntegral $ L.length s


testCountBytes :: Test
testCountBytes = testProperty "count bytes" $
                 monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,n1) <- f (countBytes (return ()))
        (!_,n2) <- f (countBytes stream2stream)

        assert $ n1 == 0
        assert $ n2 == n

        expectException $ g erriter
        expectException $ enumEof erriter >>= run
        

     where
       erriter = countBytes $ throwErr $ Err "foo"
       g iter = enumLBS s iter >>= run
       f = liftQ . g
       n = fromEnum $ L.length s


testCountBytes2 :: Test
testCountBytes2 = testProperty "count bytes" $
                  monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        pre $ L.length s > 4
        n1 <- f iter

        assert $ n1 == 4

     where
       f i = liftQ $ enumLBS s i >>= run
       iter = do
           (!_,m) <- countBytes $ drop 4
           stream2stream
           return m
