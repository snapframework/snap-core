{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------------
-- | Snap Framework type aliases and utilities for iteratees. Note that as a
-- convenience, this module also exports everything from @Data.Enumerator@ in
-- the @enumerator@ library.

module Snap.Iteratee
  (
    -- * Enumerators
    enumBS
  , enumLBS
  , enumBuilder
  , enumFile
  , enumFilePartial
  , InvalidRangeException


    -- * Iteratee utilities
  , joinI'
  , countBytes
  , drop'
  , mkIterateeBuffer
  , unsafeBufferIterateeWithBuffer
  , unsafeBufferIteratee
  , take
  , drop
  , takeExactly
  , takeNoMoreThan
  , skipToEof
  , mapEnum
  , mapIter
  , enumBuilderToByteString
  , unsafeEnumBuilderToByteString
  , enumByteStringToBuilder
  , killIfTooSlow

  , TooManyBytesReadException
  , ShortWriteException
  , RateTooSlowException

    -- * Re-export types and functions from @Data.Enumerator@
  , Stream (..)
  , Step (..)
  , Iteratee (..)
  , Enumerator
  , Enumeratee

    -- ** Primitives
    -- *** Combinators
    -- | These are common patterns which occur whenever iteratees are
    -- being defined.
  , returnI
  , yield
  , continue
  , throwError
  , catchError
  , liftI
  , (>>==)
  , (==<<)
  , ($$)
  , (>==>)
  , (<==<)
  , ($=)
  , (=$)

    -- *** Iteratees
  , run
  , run_
  , consume
  , Data.Enumerator.isEOF
  , liftTrans
  , liftFoldL
  , liftFoldL'
  , liftFoldM
  , printChunks
  , head
  , peek

    -- *** Enumerators
  , enumEOF
  , enumList
  , concatEnums
    -- *** Enumeratees
  , checkDone
  , Data.Enumerator.List.map
  , Data.Enumerator.sequence
  , joinI


{-
    -- ** Iteratee utilities
  , drop'

-}
  ) where

------------------------------------------------------------------------------

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Enumerator
import           Control.DeepSeq
import           Control.Exception.Control
import           Control.Monad
import           Control.Monad.IO.Control
import           Control.Monad.Trans (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Control
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Enumerator hiding (consume, drop, head)
import qualified Data.Enumerator as I
import           Data.Enumerator.Binary (enumHandle)
import           Data.Enumerator.List hiding (take, drop)
import qualified Data.Enumerator.List as IL
import qualified Data.List as List
import           Data.Monoid (mappend )
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Typeable
import           Foreign hiding (peek)
import           Foreign.C.Types
import           GHC.ForeignPtr
import           Prelude hiding (catch, drop, head, take)
import           System.IO

#ifndef PORTABLE
import           System.IO.Posix.MMap
import           System.PosixCompat.Files
import           System.PosixCompat.Types
#endif

------------------------------------------------------------------------------
instance (Functor m, MonadControlIO m) => MonadControlIO (Iteratee s m) where
  liftControlIO = liftLiftControlBase liftControlIO

instance MonadTransControl (Iteratee s) where
-- liftControl :: Monad m => (Run (Iteratee s) -> m α) -> Iteratee s m α
-- type Run t = forall n o β. (Monad n, Monad o, Monad ((Iteratee s) o)) =>
--              (Iteratee s) n β -> n ((Iteratee s) o β)
  liftControl f = Iteratee $ undefined --return (f run) -- undefined
    where run i = undefined -- do
                     {- step <- runIteratee i-}
                     {- return undefined-}

------------------------------------------------------------------------------
-- | Get the length of a bytestring Stream
streamLength :: Stream ByteString -> Int
streamLength (Chunks xs) = List.foldl' (\c s -> c + S.length s) 0 xs
streamLength EOF         = 0


------------------------------------------------------------------------------
-- | Enumerates a Builder.
enumBuilder :: (Monad m) => Builder -> Enumerator Builder m a
enumBuilder = enumList 1 . (:[])
{-# INLINE enumBuilder #-}


------------------------------------------------------------------------------
-- | Enumerates a strict bytestring.
enumBS :: (Monad m) => ByteString -> Enumerator ByteString m a
enumBS = enumList 1 . (:[])
{-# INLINE enumBS #-}


------------------------------------------------------------------------------
-- | Enumerates a lazy bytestring.

enumLBS :: (Monad m) => L.ByteString -> Enumerator ByteString m a
enumLBS bs = enumList 1 (L.toChunks bs)
{-# INLINE enumLBS #-}


------------------------------------------------------------------------------
skipToEof :: (Monad m) => Iteratee a m ()
skipToEof = continue k
  where
    k EOF = yield () EOF
    k _   = skipToEof


------------------------------------------------------------------------------
-- | Wraps an 'Iteratee', counting the number of bytes consumed by it.
countBytes :: (Monad m) => forall a .
              Iteratee ByteString m a
           -> Iteratee ByteString m (a, Int64)
countBytes i = Iteratee $ do
    step <- runIteratee i
    case step of
      (Continue k) -> return (Continue $ go 0 k)
      (Yield x s)  -> return $ Yield (x,0) s
      (Error e)    -> return $ Error e

  where
    go !n k str = Iteratee $ do
        let len = toEnum $ streamLength str
        step <- runIteratee (k str)
        case step of
          (Continue k') -> return (Continue $ go (n + len) k')
          (Yield x s)   -> let len' = n + len - (toEnum $ streamLength s)
                           in return (Yield (x, len') s)
          (Error e)     -> return (Error e)


------------------------------------------------------------------------------
bUFSIZ :: Int
bUFSIZ = 8192


------------------------------------------------------------------------------
-- | Creates a buffer to be passed into 'unsafeBufferIterateeWithBuffer'.
mkIterateeBuffer :: IO (ForeignPtr CChar)
mkIterateeBuffer = mallocPlainForeignPtrBytes bUFSIZ


------------------------------------------------------------------------------
-- | Buffers an iteratee, \"unsafely\". Here we use a fixed binary buffer
-- which we'll re-use, meaning that if you hold on to any of the bytestring
-- data passed into your iteratee (instead of, let's say, shoving it right out
-- a socket) it'll get changed out from underneath you, breaking referential
-- transparency. Use with caution!
unsafeBufferIteratee :: Iteratee ByteString IO a
                     -> IO (Iteratee ByteString IO a)
unsafeBufferIteratee step = do
    buf <- mkIterateeBuffer
    return $ unsafeBufferIterateeWithBuffer buf step


------------------------------------------------------------------------------
-- | Buffers an iteratee, \"unsafely\". Here we use a fixed binary buffer
-- which we'll re-use, meaning that if you hold on to any of the bytestring
-- data passed into your iteratee (instead of, let's say, shoving it right out
-- a socket) it'll get changed out from underneath you, breaking referential
-- transparency. Use with caution!
--
-- This version accepts a buffer created by 'mkIterateeBuffer'.
--
unsafeBufferIterateeWithBuffer :: ForeignPtr CChar
                               -> Iteratee ByteString IO a
                               -> Iteratee ByteString IO a
unsafeBufferIterateeWithBuffer buf iter = Iteratee $ do
    step <- runIteratee iter
    start step

  where
    --------------------------------------------------------------------------
    start :: Step ByteString IO a -> IO (Step ByteString IO a)
    start (Continue k) = return $ Continue $ go 0 k
    start s@_          = return s


    --------------------------------------------------------------------------
    sendBuf :: Int
            -> (Stream ByteString -> Iteratee ByteString IO a)
            -> IO (Step ByteString IO a)
    sendBuf n k =
      {-# SCC "unsafeBufferIteratee/sendBuf" #-} do
        assert (n > 0)       (return ())
        assert (n <= bUFSIZ) (return ())

        withForeignPtr buf $ \ptr -> do
            !s <- S.unsafePackCStringLen (ptr, n)
            runIteratee $ k $ Chunks [s]


    --------------------------------------------------------------------------
    copy EOF         = EOF
    copy (Chunks xs) = zs `deepseq` Chunks ys
      where
        !ys  = Prelude.map S.copy xs
        !zs  = Prelude.map (`seq` ()) ys

    --------------------------------------------------------------------------
    go :: Int
       -> (Stream ByteString -> Iteratee ByteString IO a)
       -> (Stream ByteString -> Iteratee ByteString IO a)
    go !n !k EOF = Iteratee $ do
        if n == 0
          then runIteratee $ k EOF
          else do
              assert (n > 0)       (return ())
              assert (n <= bUFSIZ) (return ())

              step  <- sendBuf n k
              step2 <- runIteratee $ enumEOF step
              return $ copyStep step2


    go !n !k (Chunks xs) = Iteratee $ do
        assert (n >= 0)      (return ())
        assert (n <= bUFSIZ) (return ())

        let s = S.concat xs
        let m = S.length s
        if m+n >= bUFSIZ
          then overflow    n k s m
          else copyAndCont n k s m


    --------------------------------------------------------------------------
    copyStep (Yield x r) = let !z = copy r in Yield x z
    copyStep x           = x


    --------------------------------------------------------------------------
    copyAndCont :: Int
                -> (Stream ByteString -> Iteratee ByteString IO a)
                -> ByteString
                -> Int
                -> IO (Step ByteString IO a)
    copyAndCont !n k !s !m =
      {-# SCC "unsafeBufferIteratee/copyAndCont" #-} do
        assert (n >= 0) (return ())
        assert (n+m < bUFSIZ) (return ())
        S.unsafeUseAsCStringLen s $ \(p,sz) -> do
          assert (m == sz) (return ())
          withForeignPtr buf $ \bufp -> do
            let b' = plusPtr bufp n
            copyBytes b' p sz

        return $ Continue $ go (n+m) k



    --------------------------------------------------------------------------
    overflow :: Int
             -> (Stream ByteString -> Iteratee ByteString IO a)
             -> ByteString
             -> Int
             -> IO (Step ByteString IO a)
    overflow !n k !s !m =
      {-# SCC "unsafeBufferIteratee/overflow" #-} do
        assert (n+m >= bUFSIZ) (return ())
        assert (n < bUFSIZ)    (return ())

        let rest    = bUFSIZ - n
        let m2      = m - rest

        let (s1,s2) = S.splitAt rest s

        S.unsafeUseAsCStringLen s1 $ \(p,_) ->
          withForeignPtr buf $ \bufp -> do
            let b' = plusPtr bufp n
            copyBytes b' p rest

            iv <- sendBuf bUFSIZ k
            case iv of
              (Yield x r)   -> let !z = copy r
                               in return $ Yield x $ (z `mappend` Chunks [s2])
              (Error e)     -> return $ Error e
              (Continue k') -> do
                  -- check the size of the remainder; if it's bigger than the
                  -- buffer size then just send it
                  if m2 >= bUFSIZ
                    then do
                        step <- runIteratee $ k' $ Chunks [s2]
                        case step of
                          (Yield x r)    -> let !z = copy r
                                            in return $! Yield x z
                          (Error e)      -> return $ Error e
                          (Continue k'') -> return $ Continue $ go 0 k''

                    else copyAndCont 0 k' s2 m2


------------------------------------------------------------------------------
-- | Skip n elements of the stream, if there are that many
drop :: (Monad m) => Int -> Iteratee ByteString m ()
drop k = drop' (toEnum k)

------------------------------------------------------------------------------
-- | Skip n elements of the stream, if there are that many
drop' :: (Monad m) => Int64 -> Iteratee ByteString m ()
drop' 0  = return ()
drop' !n = continue k

  where
    k EOF         = return ()
    k (Chunks xs) = chunks n xs

    chunks !m []     = drop' m
    chunks !m (x:xs) = do
        let strlen = toEnum $ S.length x
        if strlen <= m
          then chunks (m-strlen) xs
          else yield () $ Chunks ((S.drop (fromEnum m) x):xs)


------------------------------------------------------------------------------
data ShortWriteException = ShortWriteException deriving (Typeable)
data RateTooSlowException = RateTooSlowException deriving (Typeable)
data TooManyBytesReadException = TooManyBytesReadException deriving (Typeable)

instance Show ShortWriteException where
    show ShortWriteException = "Short write"

instance Show RateTooSlowException where
    show RateTooSlowException = "Input rate too slow"

instance Show TooManyBytesReadException where
    show TooManyBytesReadException = "Too many bytes read"

instance Exception ShortWriteException
instance Exception RateTooSlowException
instance Exception TooManyBytesReadException


------------------------------------------------------------------------------
take :: (Monad m) => Int -> Enumeratee ByteString ByteString m a
take k = take' (toEnum k)


------------------------------------------------------------------------------
take' :: (Monad m) => Int64 -> Enumeratee ByteString ByteString m a
take' _ y@(Yield _ _   ) = return y
take' _   (Error e     ) = throwError e
take' !n st@(Continue k) = do
    if n == 0
      then lift $ runIteratee $ k EOF
      else do
        mbX <- head
        maybe (lift $ runIteratee $ k EOF)
              check
              mbX

  where
    check x | S.null x    = take' n st
            | strlen <= n = do
                  newStep <- lift $ runIteratee $ k $ Chunks [x]
                  take' (n-strlen) newStep
            | otherwise = do
                  step1 <- lift $ runIteratee $ k $ Chunks [s1]
                  step2 <- lift $ runIteratee $ enumEOF step1

                  case step2 of
                    (Yield v _)    -> yield (Yield v EOF) (Chunks [s2])
                    (Error e)      -> throwError e
                    (Continue _)   -> error "divergent iteratee"
      where
        strlen  = toEnum $ S.length x
        (s1,s2) = S.splitAt (fromEnum n) x


------------------------------------------------------------------------------
-- | Reads n bytes from a stream and applies the given iteratee to the stream
-- of the read elements. Reads exactly n bytes, and if the stream is short
-- propagates an error.
takeExactly :: (Monad m)
            => Int64
            -> Enumeratee ByteString ByteString m a
takeExactly !n  y@(Yield _ _ ) = drop' n >> return y
takeExactly _     (Error e   ) = throwError e
takeExactly !n st@(Continue !k) = do
    if n == 0
      then lift $ runIteratee $ k EOF
      else do
        mbX <- head
        maybe (throwError ShortWriteException)
              check
              mbX

  where
    check !x | S.null x   = takeExactly n st
             | strlen < n = do
                   newStep <- lift $ runIteratee $ k $ Chunks [x]
                   takeExactly (n-strlen) newStep
             | otherwise = do
                   let (s1,s2) = S.splitAt (fromEnum n) x
                   !step1 <- lift $ runIteratee $ k $ Chunks [s1]
                   !step2 <- lift $ runIteratee $ enumEOF step1

                   case step2 of
                     (Continue _) -> error "divergent iteratee"
                     (Error e)    -> throwError e
                     (Yield v _)  -> yield (Yield v EOF) (Chunks [s2])

      where
        !strlen  = toEnum $ S.length x


------------------------------------------------------------------------------
takeNoMoreThan :: (Monad m) =>
                  Int64 -> Enumeratee ByteString ByteString m a
takeNoMoreThan _   y@(Yield _ _)  = return y
takeNoMoreThan _     (Error e  )  = throwError e
takeNoMoreThan !n st@(Continue k) = do
    mbX <- head
    maybe (lift $ runIteratee $ k EOF)
          check
          mbX

  where
    check x | S.null x    = takeNoMoreThan n st
            | strlen <= n = do
                  newStep <- lift $ runIteratee $ k $ Chunks [x]
                  takeNoMoreThan (n-strlen) newStep
            | otherwise = do
                  step1 <- lift $ runIteratee $ k $ Chunks [s1]
                  case step1 of
                    (Yield v rest) -> yield (Yield v EOF)
                                            (rest `mappend` Chunks [s2])
                    (Error e)      -> throwError e
                    (Continue _)   -> throwError TooManyBytesReadException
      where
        strlen  = toEnum $ S.length x
        (s1,s2) = S.splitAt (fromEnum n) x


------------------------------------------------------------------------------
{-# INLINE _enumFile #-}
_enumFile :: FilePath
          -> Enumerator ByteString IO a
_enumFile fp iter = do
    h  <- liftIO $ openBinaryFile fp ReadMode
    enumHandle 32678 h iter `finally` (liftIO $ hClose h)


------------------------------------------------------------------------------
data InvalidRangeException = InvalidRangeException
   deriving (Typeable)


------------------------------------------------------------------------------
instance Show InvalidRangeException where
    show InvalidRangeException = "Invalid range"


------------------------------------------------------------------------------
instance Exception InvalidRangeException


------------------------------------------------------------------------------
{-# INLINE _enumFilePartial #-}
_enumFilePartial :: FilePath
                 -> (Int64,Int64)
                 -> Enumerator ByteString IO a
_enumFilePartial fp (start,end) iter = do
    let len = end - start

    bracket (liftIO $ openBinaryFile fp ReadMode)
            (liftIO . hClose)
            (\h -> do
                 unless (start == 0) $ liftIO $
                        hSeek h AbsoluteSeek $ toInteger start
                 step <- lift $ runIteratee $ joinI $ takeExactly len iter
                 enumHandle 32678 h step)


------------------------------------------------------------------------------
enumFile :: FilePath -> Enumerator ByteString IO a
enumFilePartial :: FilePath
                -> (Int64,Int64)
                -> Enumerator ByteString IO a


#ifdef PORTABLE

enumFile = _enumFile
enumFilePartial fp rng@(start,end) iter = do
    when (end < start) $ throwError InvalidRangeException
    _enumFilePartial fp rng iter

#else

-- 40MB limit
maxMMapFileSize :: FileOffset
maxMMapFileSize = 41943040


------------------------------------------------------------------------------
tooBigForMMap :: FilePath -> IO Bool
tooBigForMMap fp = do
    stat <- getFileStatus fp
    return $ fileSize stat > maxMMapFileSize


------------------------------------------------------------------------------
enumFile _  (Error e)    = throwError e
enumFile _  (Yield x _)  = yield x EOF
enumFile fp st@(Continue k) = do
    -- for small files we'll use mmap to save ourselves a copy, otherwise
    -- we'll stream it
    tooBig <- lift $ tooBigForMMap fp

    if tooBig
      then _enumFile fp st
      else do
        es <- try $ lift $ unsafeMMapFile fp
        case es of
          (Left (e :: SomeException)) -> throwError e
          (Right s) -> k $ Chunks [s]


------------------------------------------------------------------------------
enumFilePartial _ _ (Error e)   = throwError e
enumFilePartial _ _ (Yield x _) = yield x EOF
enumFilePartial fp rng@(start,end) st@(Continue k) = do
    when (end < start) $ throwError InvalidRangeException

    let len = end - start

    tooBig <- lift $ tooBigForMMap fp

    if tooBig
      then _enumFilePartial fp rng st
      else do
        es <- try $ lift $ unsafeMMapFile fp

        case es of
          (Left (e::SomeException)) -> throwError e
          (Right s) -> k $ Chunks [ S.take (fromEnum len) $
                                    S.drop (fromEnum start) s ]

#endif


------------------------------------------------------------------------------
mapEnum :: (Monad m) =>
           (aOut -> aIn)
        -> (aIn -> aOut)
        -> Enumerator aIn m a
        -> Enumerator aOut m a
mapEnum f g enum outStep = do
    let z = IL.map g outStep
    let p = joinI z
    let q = enum $$ p
    (I.joinI . IL.map f) $$ q


------------------------------------------------------------------------------
mapIter :: (Monad m) =>
           (aOut -> aIn)
        -> (aIn -> aOut)
        -> Iteratee aIn m a
        -> Iteratee aOut m a
mapIter f g iter = do
    step <- lift $ runIteratee iter
    mapStep step
  where
    -- mapStep :: Step aIn m a -> Iteratee aOut m a
    mapStep (Continue k)   = continue $ wrapK k
    mapStep (Yield x rest) = yield x (fmap g rest)
    mapStep (Error e)      = throwError e

    -- wrapK :: (Stream aIn  -> Iteratee aIn m a)
    --       -> (Stream aOut -> Iteratee aOut m a)
    wrapK k streamOut = mapIter f g iterIn
      where
        streamIn = fmap f streamOut
        iterIn   = k streamIn


------------------------------------------------------------------------------
enumBuilderToByteString :: MonadIO m => Enumeratee Builder ByteString m a
enumBuilderToByteString = builderToByteString

------------------------------------------------------------------------------
unsafeEnumBuilderToByteString :: MonadIO m => Enumeratee Builder ByteString m a
unsafeEnumBuilderToByteString =
    builderToByteStringWith (reuseBufferStrategy (allocBuffer 65536))
    

------------------------------------------------------------------------------
enumByteStringToBuilder :: MonadIO m => Enumeratee ByteString Builder m a
enumByteStringToBuilder = IL.map fromByteString


------------------------------------------------------------------------------
joinI' :: Monad m => Iteratee a m (Step a m b)
       -> Iteratee a m b
joinI' outer = outer >>= check where
    check (Continue k) = k EOF >>== \s -> case s of
        Continue _ -> error "joinI: divergent iteratee"
        _ -> check s
    check (Yield x r) = yield x r
    check (Error e) = throwError e


------------------------------------------------------------------------------
killIfTooSlow :: (MonadIO m) =>
                 m ()                     -- ^ action to bump timeout
              -> Double                   -- ^ minimum data rate, in bytes per
                                          -- second
              -> Int                      -- ^ minimum amount of time to let
                                          -- the iteratee run for
              -> Iteratee ByteString m a  -- ^ iteratee consumer to wrap
              -> Iteratee ByteString m a
killIfTooSlow !bump !minRate !minSeconds' !inputIter = do
    !_ <- lift bump
    startTime <- liftIO getTime
    step <- lift $ runIteratee inputIter
    wrap startTime (0::Int64) step

  where
    minSeconds = fromIntegral minSeconds'

    wrap !startTime = proc
      where
        proc !nb (Continue !k) = continue $ cont nb k
        proc _ !z              = returnI z

        cont _ !k EOF = k EOF
        cont !nBytesRead !k !stream = do
            let !slen = toEnum $ streamLength stream
            now <- liftIO getTime
            let !delta = now - startTime
            let !newBytes = nBytesRead + slen
            when (delta > minSeconds+1 &&
                  fromIntegral newBytes / (delta-minSeconds) < minRate) $
              throwError RateTooSlowException

            -- otherwise bump the timeout and continue running the iteratee
            !_ <- lift bump
            lift (runIteratee $! k stream) >>= proc newBytes


------------------------------------------------------------------------------
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
