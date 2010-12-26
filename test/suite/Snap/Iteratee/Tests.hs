{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Iteratee.Tests
  ( tests ) where

import qualified Control.Exception as E
import           Control.Exception hiding (try, assert)
import           Control.Monad
import           Control.Monad.Identity
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Int
import           Data.Maybe
import           Prelude hiding (head, drop, take)
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic hiding (run)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import           Snap.Iteratee
import           Snap.Test.Common ()

import Snap.Internal.Iteratee.Debug

liftQ :: forall a m . (Monad m) => m a -> PropertyM m a
liftQ = QC.run


throwErr :: String -> Iteratee a IO b
throwErr = throwError . AssertionFailed

expectException :: IO a -> PropertyM IO ()
expectException m = do
    e <- liftQ $ E.try m
    case e of
      Left (z::SomeException)  -> (show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"


tests :: [Test]
tests = [ testEnumBS
        , testEnumLBS
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
testEnumBS = testProperty "iteratee/enumBS" prop
  where
    prop :: S.ByteString -> Bool
    prop s = (S.concat $ runIdentity (run_ iter)) == s
      where
        iter = runIdentity $ liftM (enumBS s) $ runIteratee consume

testEnumLBS :: Test
testEnumLBS = testProperty "iteratee/enumLBS" prop
  where
    prop :: L.ByteString -> Bool
    prop s = L.fromChunks (runIdentity (run_ iter)) == s
      where
        iter = runIdentity $ liftM (enumLBS s) $ runIteratee consume


copyingConsume :: Iteratee ByteString IO L.ByteString
copyingConsume = f []
  where
    f l = do
        mbX <- head
        maybe (return $ L.fromChunks $ reverse l)
              (\x -> let !z = S.copy x
                     in z `seq` f (z : l))
              mbX


bufferAndRun :: Iteratee ByteString IO a -> L.ByteString -> IO a
bufferAndRun ii s = do
    i <- unsafeBufferIteratee ii >>= runIteratee
    run_ $ enumLBS s i


testUnsafeBuffer :: Test
testUnsafeBuffer = testProperty "iteratee/testUnsafeBuffer" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        pre $ s /= L.empty
        x <- liftQ $ bufferAndRun copyingConsume s'
        assert $ x == s'

      where
        s' = L.take 20000 $ L.cycle s


testUnsafeBuffer2 :: Test
testUnsafeBuffer2 = testCase "iteratee/testUnsafeBuffer2" prop
  where
    prop = do
        i <- unsafeBufferIteratee (drop 4 >> copyingConsume) >>= runIteratee
        s <- run_ $ enumLBS "abcdefgh" i
        H.assertEqual "s == 'efgh'" "efgh" s


testUnsafeBuffer3 :: Test
testUnsafeBuffer3 = testProperty "iteratee/testUnsafeBuffer3" $
                    monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        pre $ s /= L.empty

        ss <- liftQ $ runIteratee copyingConsume >>=
                      return . joinI . take 19999

        x <- liftQ $ bufferAndRun (ss >>= \x -> drop 1 >> return x) s'

        let y = L.take 19999 s'
        if x /= y
         then liftQ $ do
             putStrLn $ "FAILED!!!!!"
             putStrLn $ "length x = " ++ show (L.length x)
             putStrLn $ "length y = " ++ show (L.length y)
             diff x y
             putStrLn $ "input was " ++ show s
         else return ()

        assert $ x == y
      where
        s' = L.take 20000 $ L.cycle s


    diff a b = d a b (0::Int)
    d a b n = do
        case ma of
          Nothing -> if mb /= Nothing
                       then do
                         let Just (y,_) = mb
                         putStrLn $ "differ at byte " ++ show n
                         putStrLn $ "a=Nothing, b=" ++ show y
                       else return ()

          Just (x,rest1) ->
            if isNothing mb
              then do
                putStrLn $ "differ at byte " ++ show n
                putStrLn $ "a=" ++ show x ++ ", b=Nothing"
              else do
                let Just (y,rest2) = mb
                if x /= y
                   then do
                     putStrLn $ "differ at byte " ++ show n
                     putStrLn $ "a=" ++ show x ++ ", b=" ++ show y
                     let r1 = L.take 6 rest1
                     let r2 = L.take 6 rest2
                     putStrLn $ "next few bytes a = " ++ show r1
                     putStrLn $ "next few bytes b = " ++ show r2
                   else
                     d rest1 rest2 (n+1)
      where
        ma = L.uncons a
        mb = L.uncons b



tub3Prop s = do
    ss <- runIteratee copyingConsume >>=
          return . joinI . take 19999

    let it = (ss >>= \x -> drop 1 >> return x)
    x <- bufferAndRun (iterateeDebugWrapper "foo" it) s'

    let a = x
    let b = L.take 19999 s'
    let boo = (a == b)
    putStrLn $ "equal? " ++ show boo
    diff a b

  where
    diff a b = d a b (0::Int)
    d a b n = do
        case ma of
          Nothing -> if mb /= Nothing
                       then do
                         let Just (y,_) = mb
                         putStrLn $ "differ at byte " ++ show n
                         putStrLn $ "a=Nothing, b=" ++ show y
                       else return ()

          Just (x,rest1) ->
            if isNothing mb
              then do
                putStrLn $ "differ at byte " ++ show n
                putStrLn $ "a=" ++ show x ++ ", b=Nothing"
              else do
                let Just (y,rest2) = mb
                if x /= y
                   then do
                     putStrLn $ "differ at byte " ++ show n
                     putStrLn $ "a=" ++ show x ++ ", b=" ++ show y
                   else
                     d rest1 rest2 (n+1)
      where
        ma = L.uncons a
        mb = L.uncons b

    s' = L.take 20000 $ L.cycle s




testUnsafeBuffer4 :: Test
testUnsafeBuffer4 = testProperty "iteratee/testUnsafeBuffer4" $
                    monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        i  <- liftQ (unsafeBufferIteratee (copyingConsume >> throwErr "foo") >>=
                     runIteratee)
        expectException $ run_ $ enumLBS s i

        j  <- liftQ $ unsafeBufferIteratee (throwErr "foo" >> copyingConsume)
                      >>= runIteratee
        expectException $ run_ $ enumLBS s j


testUnsafeBuffer5 :: Test
testUnsafeBuffer5 = testProperty "iteratee/testUnsafeBuffer5" $
                    monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        pre $ s /= L.empty
        x <- liftQ $ bufferAndRun copyingConsume s
        assert $ x == s


testTakeExactly1 :: Test
testTakeExactly1 = testProperty "iteratee/takeExactly1" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        expectException $ doIter >>= run_

      where
        doIter = runIteratee consume >>=
                 runIteratee . joinI . takeExactly (n+1) >>=
                 return . enumLBS s

        n = fromIntegral $ L.length s


testTakeExactly2 :: Test
testTakeExactly2 = testProperty "iteratee/takeExactly2" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        e <- liftQ $ doIter >>= run_
        assert $ L.fromChunks e == s

      where
        doIter = runIteratee consume >>=
                 runIteratee . joinI . takeExactly n >>=
                 return . enumLBS s

        n = fromIntegral $ L.length s


testTakeExactly3 :: Test
testTakeExactly3 = testProperty "iteratee/takeExactly3" $
                   monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        e <- liftQ $ doIter >>= run_
        assert $ L.fromChunks e == L.take (fromIntegral n) s

      where
        doIter = runIteratee consume >>=
                 runIteratee . joinI . takeExactly n >>=
                 return . enumLBS s

        n = fromIntegral $ L.length s


testTakeNoMoreThan1 :: Test
testTakeNoMoreThan1 = testProperty "iteratee/takeNoMoreThan1" $
                      monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        s' <- liftQ $ doIter >>= run_

        assert $ s == L.fromChunks s'

      where
        doIter = do
            step <- runIteratee consume >>=
                    runIteratee . joinI . takeNoMoreThan (n+1)
            return $ enumLBS s step

        n = fromIntegral $ L.length s


testTakeNoMoreThan2 :: Test
testTakeNoMoreThan2 = testProperty "iteratee/takeNoMoreThan2" $
                      monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        e <- liftQ $ doIter >>= run_
        assert $ L.fromChunks e == s

      where
        doIter = do
            step  <- runIteratee consume
            step' <- runIteratee (joinI (takeNoMoreThan n step))
            return $ enumLBS (L.concat ["", s]) step'


        n = fromIntegral $ L.length s


testTakeNoMoreThan3 :: Test
testTakeNoMoreThan3 = testProperty "iteratee/takeNoMoreThan3" $
                      monadicIO $ forAllM arbitrary prop
  where
    prop :: (Int64,L.ByteString) -> PropertyM IO ()
    prop (m,s) = do
        step1 <- liftQ $ runIteratee consume
        step2 <- liftQ $ runIteratee $ joinI (takeNoMoreThan 0 step1)
        v <- liftQ $ run_ $ enumLBS "" step2
        assert $ S.concat v == ""

        if (L.null s || m == 0)
           then liftQ $ do
                     !_ <- doIter >>= run_
                     return ()
           else expectException $ doIter >>= run_ >>= return


      where
        doIter = do
            step <- runIteratee consume
            let i = joinI $ takeNoMoreThan (n-abs m) step
            step' <- runIteratee i
            return $ enumLBS s step'

        n = L.length s


testCountBytes :: Test
testCountBytes = testProperty "iteratee/countBytes" $
                 monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,n1) <- liftQ (runIteratee (countBytes (return ())) >>= f)
        (!_,n2) <- liftQ (runIteratee (countBytes consume) >>= f)

        assert $ n1 == 0
        assert $ n2 == n

        expectException $ (runIteratee erriter >>= f)
        expectException $ (runIteratee erriter >>= run_ . enumEOF)


     where
       erriter = countBytes $ throwError $ AssertionFailed "foo"
       f iter = run_ $ enumLBS s iter
       n = L.length s


testCountBytes2 :: Test
testCountBytes2 = testProperty "iteratee/countBytes2" $
                  monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        pre $ L.length s > 4

        step    <- liftQ $ runIteratee iter
        (n1,s') <- f step

        assert $ n1 == 4
        assert $ s' == L.drop 4 s

     where
       f i = liftQ $ run_ $ enumLBS s i
       iter = do
           (!_,m) <- countBytes $ drop' 4
           x <- liftM L.fromChunks consume
           return (m,x)
