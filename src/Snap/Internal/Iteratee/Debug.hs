-- | An internal Snap module for debugging iteratees.
--
-- /N.B./ this is an internal interface, please don't write user code that
-- depends on it.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}


module Snap.Internal.Iteratee.Debug
  ( debugIteratee
  , iterateeDebugWrapper
  ) where

------------------------------------------------------------------------------
import           Data.Iteratee.WrappedByteString
import           Data.Word (Word8)
import           System.IO
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Iteratee
------------------------------------------------------------------------------


------------------------------------------------------------------------------
instance Show (WrappedByteString Word8) where
    show (WrapBS s) = show s


------------------------------------------------------------------------------
debugIteratee :: Iteratee IO ()
debugIteratee = IterateeG f
  where
    f c@(EOF _) = do
        putStrLn $ "got EOF: " ++ show c
        hFlush stdout
        return (Done () c)

    f c@(Chunk _) = do
        putStrLn $ "got chunk: " ++ show c
        hFlush stdout
        return $ Cont debugIteratee Nothing


#if defined(DEBUG)

iterateeDebugWrapper :: String -> Iteratee IO a -> Iteratee IO a
iterateeDebugWrapper name iter = IterateeG f
  where
    f c@(EOF Nothing) = do
        debug $ name ++ ": got EOF: " ++ show c
        runIter iter c

    f c@(EOF (Just e)) = do
        debug $ name ++ ": got EOF **error**: " ++ show c
        runIter iter c

    f c@(Chunk _) = do
        debug $ name ++ ": got chunk: " ++ show c
        wrapResult $ runIter iter c

    wrapResult m = do
        iv <- m
        let i = liftI iv

        return $ Cont (iterateeDebugWrapper name i) Nothing

#else

iterateeDebugWrapper :: String -> Iteratee IO a -> Iteratee IO a
iterateeDebugWrapper _ = id
{-# INLINE iterateeDebugWrapper #-}

#endif
