-- | An internal Snap module for debugging iteratees.
--
-- /N.B./ this is an internal interface, please don't write user code that
-- depends on it.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports    #-}


module Snap.Internal.Iteratee.Debug
  ( debugIteratee
  , iterateeDebugWrapper
  , iterateeDebugWrapperWith
  , showBuilder
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           System.IO
------------------------------------------------------------------------------
#ifndef NODEBUG
import           Snap.Internal.Debug
#endif
import           Snap.Iteratee hiding (map)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
showBuilder :: Builder -> String
showBuilder = show . toByteString


------------------------------------------------------------------------------
debugIteratee :: Iteratee ByteString IO ()
debugIteratee = continue f
  where
    f EOF = do
        liftIO $ putStrLn $ "got EOF"
        liftIO $ hFlush stdout
        yield () EOF

    f (Chunks xs) = do
        liftIO $ putStrLn $ "got chunk: " ++ show (xs)
        liftIO $ hFlush stdout
        continue f


#ifndef NODEBUG

iterateeDebugWrapperWith :: (MonadIO m) =>
                            (a -> String)
                         -> String
                         -> Iteratee a m b
                         -> Iteratee a m b
iterateeDebugWrapperWith showFunc name iter = do
    debug $ name ++ ": BEGIN"
    step <- lift $ runIteratee iter
    whatWasReturn step
    check step

  where
    whatWasReturn (Continue _) = debug $ name ++ ": continue"
    whatWasReturn (Yield _ z)  = debug $ name ++ ": yield, with remainder "
                                              ++ showStream z
    whatWasReturn (Error e)    = debug $ name ++ ": error, with " ++ show e

    check (Continue k) = continue $ f k
    check st           = returnI st


    f k EOF = do
        debug $ name ++ ": got EOF"
        k EOF

    f k ch@(Chunks xs) = do
        debug $ name ++ ": got chunk: " ++ showL xs
        step <- lift $ runIteratee $ k ch
        whatWasReturn step
        check step

    showStream = show . fmap showFunc
    showL = show . map showFunc


iterateeDebugWrapper :: (Show a, MonadIO m) =>
                        String
                     -> Iteratee a m b
                     -> Iteratee a m b
iterateeDebugWrapper = iterateeDebugWrapperWith show


#else

iterateeDebugWrapperWith :: (MonadIO m) =>
                            (s -> String)
                         -> String
                         -> Iteratee s m a
                         -> Iteratee s m a
iterateeDebugWrapperWith _ _ = id
{-# INLINE iterateeDebugWrapperWith #-}


iterateeDebugWrapper :: (MonadIO m, Show s) =>
                        String -> Iteratee s m a -> Iteratee s m a
iterateeDebugWrapper _ = id
{-# INLINE iterateeDebugWrapper #-}

#endif
