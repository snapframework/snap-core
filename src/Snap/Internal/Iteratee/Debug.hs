{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Snap.Internal.Iteratee.Debug ( debugIteratee ) where

------------------------------------------------------------------------------
import           Data.Iteratee.WrappedByteString
import           Data.Word (Word8)
import           System.IO
------------------------------------------------------------------------------
import           Snap.Iteratee
------------------------------------------------------------------------------


instance Show (WrappedByteString Word8) where
    show (WrapBS s) = show s

debugIteratee :: Iteratee IO ()
debugIteratee = IterateeG f
  where
    f c@(EOF _) = do
        putStrLn $ "got chunk: " ++ show c
        hFlush stdout
        return (Done () c)

    f c@(Chunk _) = do
        putStrLn $ "got chunk: " ++ show c
        hFlush stdout
        return $ Cont debugIteratee Nothing
