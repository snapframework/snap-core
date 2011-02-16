{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Internal.Iteratee.BoyerMooreHorspool
  ( bmhEnumeratee
  , MatchInfo(..) )
  where

import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Unsafe as S
import           Data.Enumerator hiding (head, filter, map)
import qualified Data.Enumerator.List as EL
import           Data.Int
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed.Mutable  as MV
import           Prelude               hiding (head, last)
import           Snap.Iteratee hiding (map)


{-# INLINE debug #-}
debug :: MonadIO m => String -> m ()
--debug s = liftIO $ putStrLn s
debug _ = return ()

------------------------------------------------------------------------------
data MatchInfo = Match !ByteString
               | NoMatch !ByteString
  deriving (Show)


-- We return strict bytestring because we always expect a chunk to be bigger
-- than the needle
lookahead :: (MonadIO m) =>
             Int
          -> Iteratee ByteString m (Either ByteString ByteString)
lookahead n = go id n
  where
    go !dlist !k = do
        EL.head >>= maybe
                        (do
                            let !ls = S.concat $ dlist []
                            debug $ "lookahead " ++ show n
                             ++ " failing, returning " ++ show ls

                            return $ Left ls)
                        (\x -> do
                             let !l  = S.length x
                             let !r  = k - l
                             let !d' = dlist . (x:)

                             if r <= 0
                               then do
                                   let !ls = S.concat $ d' []
                                   debug $ "lookahead " ++ show n
                                    ++ " successfully returning "
                                    ++ show ls
                                   return $ Right $ ls
                               else go d' r)
{-# INLINE lookahead #-}

matches :: ByteString     -- ^ needle
        -> Int            -- ^ needle start
        -> Int            -- ^ needle end (inclusive)
        -> ByteString     -- ^ haystack
        -> Int            -- ^ haystack start
        -> Int            -- ^ haystack end (inclusive)
        -> Bool
matches !needle !nstart !nend' !haystack !hstart !hend' =
    go nend' hend'
  where
    go !nend !hend =
        if nend < nstart || hend < hstart
          then True
          else let !nc = S.index needle nend -- FIXME: use unsafeIndex
                   !hc = S.index haystack hend
               in if nc /= hc
                    then False
                    else go (nend-1) (hend-1)
{-# INLINE matches #-}


bmhEnumeratee :: (MonadIO m) =>
                 ByteString
              -> Step MatchInfo m a
              -> Iteratee ByteString m (Step MatchInfo m a)
bmhEnumeratee needle _step = do
    debug $ "boyermoore: needle=" ++ show needle
    cDone _step iter
  where
    {-# INLINE cDone #-}
    cDone (Continue k) f = f k
    cDone step _ = yield step (Chunks [])


    iter !k = {-# SCC "bmh/iter" #-} do
        lookahead nlen >>= either (finishAndEOF k . (:[]))
                                  (startSearch k)

    finishAndEOF k xs = {-# SCC "finishAndEOF" #-} do
        debug $ "finishAndEOF, returning NoMatch for " ++ show xs
        step <- lift $ runIteratee $ k $
                Chunks (map NoMatch $ filter (not . S.null) xs)
        cDone step (\k' -> lift $ runIteratee $ k' EOF)


    startSearch !k !haystack = {-# SCC "startSearch" #-} do
        debug $ "startsearch: " ++ show haystack
        if S.null haystack
           then lookahead nlen >>=
                either (\s -> finishAndEOF k [s])
                       (startSearch k)
           else go 0
      where
        !hlen = S.length haystack

        go !hidx
          | hend >= hlen = crossBound hidx
          | otherwise = {-# SCC "go" #-} do
              let match = matches needle 0 last haystack hidx hend
              debug $ "go " ++ show hidx ++ ", hend=" ++ show hend
                        ++ ", match was " ++ show match
              if match
                then {-# SCC "go/match" #-} do
                  let !nomatch = S.take hidx haystack
                  let !aftermatch = S.drop (hend+1) haystack

                  step <- if not $ S.null nomatch
                            then lift $ runIteratee $ k $ Chunks [NoMatch nomatch]
                            else return $ Continue k

                  cDone step $ \k' -> do
                      step' <- lift $ runIteratee $ k' $ Chunks [Match needle]
                      cDone step' $ \k'' -> startSearch k'' aftermatch
                else {-# SCC "go/nomatch" #-} do
                  -- skip ahead
                  let c = S.index haystack hend
                  let !skip = V.unsafeIndex table $ fromEnum c
                  go (hidx + skip)
          where
            !hend = hidx + nlen - 1
                                        
        crossBound !hidx = {-# SCC "crossBound" #-} do
            let !leftLen = hlen - hidx
            let !needMore = nlen - leftLen
            debug $ "crossbound " ++ show hidx ++ ", leftlen=" ++ show leftLen
                      ++ ", needmore=" ++ show needMore                            
            lookahead needMore >>=
             either
              (\s -> finishAndEOF k [haystack, s])
              (\nextHaystack -> do
                   let match1 = matches needle leftLen last
                                        nextHaystack 0 (needMore-1)
                   let match2 = matches needle 0 (leftLen-1)
                                        haystack hidx (hlen-1)

                   debug $ "crossbound match1=" ++ show match1
                             ++ " match2=" ++ show match2

                   if match1 && match2
                     then {-# SCC "crossBound/match" #-} do
                       let !nomatch = S.take hidx haystack
                       let !aftermatch = S.drop needMore nextHaystack

                       -- FIXME: merge this code w/ above
                       step <- if not $ S.null nomatch
                                 then lift $ runIteratee $ k $
                                      Chunks [NoMatch nomatch]
                                 else return $ Continue k

                       debug $ "matching"
                       cDone step $ \k' -> do
                           step' <- lift $ runIteratee $ k' $
                                    Chunks [Match needle]
                           cDone step' $ \k'' ->
                               startSearch k'' aftermatch

                     else {-# SCC "crossBound/nomatch" #-} do
                       let c = S.index nextHaystack $ needMore-1
                       let p = V.unsafeIndex table (fromEnum c)

                       debug $ "p was " ++ show p ++ ", ll=" ++ show leftLen
                       if p < leftLen
                         then do
                           let (!nomatch, !crumb) = S.splitAt (hidx + p)
                                                              haystack
                           let !rest = S.append crumb nextHaystack
                           step <- lift $ runIteratee $ k $
                                   Chunks $ map NoMatch $
                                   filter (not . S.null) [nomatch]

                           cDone step $ flip startSearch rest

                         else do
                           let sidx = p - leftLen
                           let (!crumb, !rest) = S.splitAt sidx nextHaystack
                           step <- lift $ runIteratee $ k $
                                   Chunks $ map NoMatch $
                                   filter (not . S.null) [haystack, crumb]

                           cDone step $ flip startSearch rest
              )



    !nlen = S.length needle

    !last = nlen - 1

    !table = V.create $ do
        t <- MV.replicate 256 nlen
        go t

      where
        go !t = go' 0
          where
            go' !i | i >= last  = return t
                   | otherwise = do
                let c = fromEnum $ S.unsafeIndex needle i
                MV.unsafeWrite t c (last - i)
                go' $! i+1


testIt :: ByteString -> [ByteString] -> IO [MatchInfo]
testIt needle haystack = do
    consumeStep <- runIteratee EL.consume
    eteeStep    <- runIteratee $ etee consumeStep
    -- iter :: Iteratee ByteString m (Step MatchInfo m [MatchInfo])
    let iter = enumList 1 haystack eteeStep
    finalInnerStep <- run_ iter
    run_ $ returnI finalInnerStep

  where
    etee = bmhEnumeratee needle
