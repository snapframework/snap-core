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
  ) where

------------------------------------------------------------------------------
import "monads-fd" Control.Monad.Trans
import             Data.ByteString (ByteString)
import             System.IO
------------------------------------------------------------------------------
#ifndef NODEBUG
import           Snap.Internal.Debug
#endif
import           Snap.Iteratee
------------------------------------------------------------------------------


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

iterateeDebugWrapper :: (MonadIO m) =>
                        String
                     -> Iteratee ByteString m a
                     -> Iteratee ByteString m a
iterateeDebugWrapper name iter = do
    debug $ name ++ ": BEGIN"
    step <- lift $ runIteratee iter
    whatWasReturn step
    check step

  where
    whatWasReturn (Continue _) = debug $ name ++ ": continue"
    whatWasReturn (Yield _ z)  = debug $ name ++ ": yield, with remainder " ++ show z
    whatWasReturn (Error e)    = debug $ name ++ ": error, with " ++ show e

    check (Continue k) = continue $ f k
    check st           = returnI st


    f k EOF = do
        debug $ name ++ ": got EOF"
        k EOF

    f k ch@(Chunks xs) = do
        debug $ name ++ ": got chunk: " ++ show xs
        step <- lift $ runIteratee $ k ch
        whatWasReturn step
        check step


#else

iterateeDebugWrapper :: String -> Iteratee IO a -> Iteratee IO a
iterateeDebugWrapper _ = id
{-# INLINE iterateeDebugWrapper #-}

#endif
