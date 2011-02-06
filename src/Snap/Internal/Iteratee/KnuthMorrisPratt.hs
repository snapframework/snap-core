{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Internal.Iteratee.KnuthMorrisPratt
  ( kmpEnumeratee
  , MatchInfo(..) )
  where

import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Unsafe as S
import           Data.Enumerator hiding (head)
import qualified Data.Enumerator.List as EL
import qualified Data.Vector           as V
import           Data.Vector           (Vector)
import qualified Data.Vector.Mutable   as MV
import           Prelude               hiding (head)

------------------------------------------------------------------------------
data MatchInfo = Match !ByteString
               | NoMatch !ByteString
  deriving (Show)

------------------------------------------------------------------------------
-- FIXME: s/MonadIO/Monad/
kmpEnumeratee :: (MonadIO m) =>
                 ByteString                         -- ^ needle
              -> Enumeratee ByteString MatchInfo m a
kmpEnumeratee needle = checkDone (iter "" 0)
  where
    needleLen = S.length needle
    table     = buildKmpTable needle

    --------------------------------------------------------------------------
    iter :: (MonadIO m) =>
            ByteString
         -- ^ num bytes left over from previous match
         -> Int  -- ^ needle index
         -> (Stream MatchInfo -> Iteratee MatchInfo m a)
         -- ^ iteratee continuation
         -> Iteratee ByteString m (Step MatchInfo m a)
    iter !leftOver !needleIndex !k = do
        EL.head >>= maybe (finish leftOver k)
                          (processChunk leftOver needleIndex k)

    --------------------------------------------------------------------------
    finish :: (MonadIO m) =>
              ByteString
           -> (Stream MatchInfo -> Iteratee MatchInfo m a)
           -> Iteratee ByteString m (Step MatchInfo m a)
    finish !leftOver !k
        | S.null leftOver = lift $ runIteratee $ k EOF
        | otherwise = do
            step <- lift $ runIteratee $ k $ Chunks [NoMatch leftOver]
            checkDone (\k' -> lift $ runIteratee $ k' EOF) step

    --------------------------------------------------------------------------
    processChunk :: (MonadIO m) =>
                    ByteString
                 -> Int
                 -> (Stream MatchInfo -> Iteratee MatchInfo m a)
                 -> ByteString
                 -> Iteratee ByteString m (Step MatchInfo m a)
    processChunk !leftOver !needleIndex !k !input = go 0 needleIndex

      where
        !inputLen    = S.length input
        !leftOverLen = S.length leftOver
        !totalLen    = inputLen + leftOverLen

        -- m = start of match in leftOver + index
        -- i = needle index
        go !m !i
           | (m+i >= totalLen) = finalize m i
           | (S.unsafeIndex needle i == S.unsafeIndex input ii) =
                 if i == needleLen - 1
                   then yieldMatch m
                   else go m (i+1)
           | otherwise = go m' i'

          where
            ii = i + m - leftOverLen
            ti = V.unsafeIndex table i
            m' = m + i - ti
            i' = max 0 ti

           
        ----------------------------------------------------------------------
        -- here we've reached the end of the input chunk. A couple of things
        -- we know:
        --
        -- * the input from [0..m) doesn't match the needle and we should
        --   yield it to the inner iteratee. However if m == 0 then the whole
        --   input string was a match and we need to feed our previous
        --   leftovers forward plus our entire input string.
        --
        -- * the input from [m..ilen) is a partial match that we need to feed
        --   forward
        finalize m i
            | m == 0 = iter (S.append leftOver input) i k

            | m < leftOverLen = do
                -- here part of the leftover is the no match and we carry the
                -- rest forward along with the input
                let (nomatch, restLeftOver) = S.splitAt m leftOver
                let rest = S.append restLeftOver input
                let chunk = Chunks [NoMatch nomatch]
                step <- lift $ runIteratee $ k chunk
                checkDone (iter rest i) step

            | otherwise = do
                -- the whole leftOver part was garbage.
                let m' = m - leftOverLen
                let (nomatchInput, rest) = S.splitAt m' input
                let nomatch = S.append leftOver nomatchInput
                let chunk = Chunks [NoMatch nomatch]
                step <- lift $ runIteratee $ k chunk
                checkDone (iter rest i) step
            

        ----------------------------------------------------------------------
        -- we got a match! We need to yield [0..m) to the inner iteratee as a
        -- nomatch, then yield the needle, then go back to processing the rest
        -- of the input from scratch. Same caveats re: m==0 apply here.
        yieldMatch m 
            | m == 0 = do
                 -- we have no garbage and just advance by the size of the needle
                 step <- lift $ runIteratee $ k $ Chunks [Match needle]
                 -- we also can be sure here that the needle crosses the
                 -- leftOver..input boundary (otherwise we would have yielded
                 -- it earlier)
                 let m' = needleLen - leftOverLen
                 let rest = S.drop m' input
                 checkDone (\k' -> processChunk "" 0 k' rest) step

            | otherwise = do
                 let (garbage,rest) =
                         if m < leftOverLen
                           then let (a,b) = S.splitAt m leftOver
                                in (a, S.drop needleLen $ S.append b input)
                           else let m'    = m - leftOverLen
                                    (a,b) = S.splitAt m' input
                                in (S.append leftOver a, S.drop needleLen b)
                 
                 step <- lift $ runIteratee $ k $ Chunks [NoMatch garbage]
                 flip checkDone step $ \k' -> do
                     step' <- lift $ runIteratee $ k' $ Chunks [Match needle]
                     flip checkDone step' $ \k'' -> processChunk "" 0 k'' rest


------------------------------------------------------------------------------
buildKmpTable :: ByteString -> Vector Int
buildKmpTable needle = V.create $ do
    t <- MV.new (max 2 needleLen)
    MV.write t 0 (-1)
    MV.write t 1 0
    f 2 0 t

  where
    needleLen = S.length needle

    f !pos !cnd t =
        -- are we finished? return the vector.
        if pos >= needleLen
          then return t
          else do
            let wPos1 = S.unsafeIndex needle (pos-1)
            let wCnd  = S.unsafeIndex needle cnd

            if wPos1 == wCnd
              then do
                -- first case: the substring continues
                let cnd' = cnd+1
                MV.write t pos cnd'
                f (pos+1) cnd' t

              else if cnd > 0
                     then do
                       -- second case: it doesn't, but we can fall back
                       cnd' <- MV.read t cnd
                       f pos cnd' t
                     else do
                       -- we have run out of candidates.
                       MV.write t pos 0
                       f (pos+1) cnd t


{-
testIt :: ByteString -> [ByteString] -> IO [MatchInfo]
testIt needle haystack = do
    consumeStep <- runIteratee consume
    eteeStep    <- runIteratee $ etee consumeStep
    -- iter :: Iteratee ByteString m (Step MatchInfo m [MatchInfo])
    let iter = enumList 1 haystack eteeStep
    finalInnerStep <- run_ iter
    run_ $ returnI finalInnerStep

  where
    etee = kmpEnumeratee needle

-}
