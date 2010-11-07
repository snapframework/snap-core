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

iterateeDebugWrapper :: String
                     -> Iteratee ByteString IO a
                     -> Iteratee ByteString IO a
iterateeDebugWrapper name iter = Iteratee $ do
    step <- runIteratee iter
    return $ Continue $ f step

  where
    f step EOF = do
        debug $ name ++ ": got EOF"
        step' <- checkDone (\k -> lift $ runIteratee $ k EOF) step
        returnI step'

    f step ch@(Chunks xs) = do
        debug $ name ++ ": got chunk: " ++ show xs
        step' <- checkDone (\k -> lift $ runIteratee $ k ch) step
        continue $ f step'


#else

iterateeDebugWrapper :: String -> Iteratee IO a -> Iteratee IO a
iterateeDebugWrapper _ = id
{-# INLINE iterateeDebugWrapper #-}

#endif
