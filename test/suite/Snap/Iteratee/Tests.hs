{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Iteratee.Tests
  ( tests ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import           Control.Exception hiding (try, assert, throw, catch)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Identity
import           Control.Monad.Trans
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Int
import           Data.Maybe
import           Prelude hiding (catch, head, drop, take)
import           System.Timeout
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic hiding (run)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import           Snap.Iteratee
import           Snap.Internal.Iteratee.BoyerMooreHorspool
import           Snap.Test.Common

import Snap.Internal.Iteratee.Debug

throwErr :: String -> Iteratee a IO b
throwErr = throwError . AssertionFailed


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
        , testKillIfTooSlow1
        , testKillIfTooSlow2
        , testBMH
        , testBMHTrivials
        , testCatchIO
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


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
testBMHTrivials :: Test
testBMHTrivials = testCase "iteratee/BoyerMooreHorspoolTrivial" prop
  where
    prop = do
        coverShowInstance $ Match ""
        coverShowInstance $ NoMatch ""


------------------------------------------------------------------------------
testBMH :: Test
testBMH = testProperty "iteratee/BoyerMooreHorspool" $
          monadicIO $ forAllM arbitrary prop
  where
    prop :: (ByteString, [ByteString]) -> PropertyM IO ()
    prop (needle', haystack') = do
        let needle = B16.encode needle'
        let haystack = Prelude.map B16.encode haystack'

        let lneedle = L.fromChunks [needle]
        let lhaystack = L.fromChunks haystack

        pre ((not $ S.null needle) &&
             (not $ L.null lhaystack) &&
             (not $ S.isInfixOf needle (S.concat haystack)))

        -- put the needle at the beginning, at the end, and somewhere in the
        -- middle

        lhay <- insertNeedle lneedle lhaystack
        let stream = L.concat [lneedle, lhay]

        -- there should be exactly three Matches
        let iter = enumLBS stream $$ joinI (bmhEnumeratee needle $$ consume)
        outp <- QC.run $ run_ iter

        let nMatches = length $ filter isMatch outp

        when (nMatches /= 3) $ QC.run $ do
            putStrLn "got wrong number of matches!!"
            putStrLn "needle:\n"
            putStrLn $ show lneedle
            putStrLn "\nhaystack:\n"
            mapM_ (putStrLn . show) (L.toChunks stream)
            putStrLn "\noutput stream:"
            mapM_ (putStrLn . show) outp
            putStrLn ""

        assert $ nMatches == 3


    isMatch (Match _) = True
    isMatch _         = False

    insertNeedle lneedle lhaystack = do
        idxL  <- pick $ choose (0, lenL-1)
        idxN  <- pick $ choose (0, lenN-1)
        idxN2 <- pick $ choose (0, lenN-1)
        let (l1, l2) = L.splitAt (toEnum idxL) lhaystack
        let (n1, n2) = L.splitAt (toEnum idxN) lneedle
        let (n3, n4) = L.splitAt (toEnum idxN2) lneedle

        return $ L.concat [ l1, n1, n2, l2, n3, n4 ]

      where
        lenN = fromEnum $ L.length lneedle
        lenL = fromEnum $ L.length lhaystack

------------------------------------------------------------------------------
testKillIfTooSlow1 :: Test
testKillIfTooSlow1 = testCase "iteratee/killIfTooSlow1" $ do
    let iter = killIfTooSlow (return ()) 1000 4 consume
    m <- timeout (10*seconds) (expectExceptionH $ run_ $ tooSlowEnum 10 $$ iter)
    maybe (fail "timed out without dying")
          (const $ return ())
          m


------------------------------------------------------------------------------
testKillIfTooSlow2 :: Test
testKillIfTooSlow2 = testCase "iteratee/killIfTooSlow2" $ do
    -- 10 bytes per second, minimum run 2 seconds
    let iter = killIfTooSlow (return ()) 10 2 consume
    m <- liftM S.concat $ run_ $ tooSlowEnum 3 $$ iter
    H.assertEqual "testKillIfTooSlow2" (S.replicate 300 'f') m



------------------------------------------------------------------------------
testCatchIO :: Test
testCatchIO = testCase "iteratee/monadCatchIO" $ do
    e <- run_ $ enumList 1 ["1", "2", "3", "4", "5"] $$ iter 0
    H.assertBool "handled exception" $ isJust e

  where
    iter !i = (continue $ k (i::Int)) `catch` h

    k _ EOF = return Nothing
    k i _   = if i >= 2
                then throw $ ErrorCall "should not escape!"
                else iter (i+1)

    h :: SomeException -> Iteratee ByteString IO (Maybe String)
    h e = return $ Just $ show e

------------------------------------------------------------------------------
tooSlowEnum :: Int -> Enumerator ByteString IO a
tooSlowEnum ntimes (Continue k) = do
    if ntimes <= 0
      then k EOF
      else do
        step <- lift $ runIteratee $ k $ Chunks [S.replicate 100 'f']
        liftIO $ waitabit
        tooSlowEnum (ntimes-1) step
tooSlowEnum _ z = returnI z


------------------------------------------------------------------------------
waitabit :: IO ()
waitabit = threadDelay $ 2*seconds

------------------------------------------------------------------------------
seconds :: Int
seconds = (10::Int) ^ (6::Int)
