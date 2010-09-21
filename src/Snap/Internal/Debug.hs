-- | An internal Snap module for (optionally) printing debugging
-- messages. Normally 'debug' does nothing, but if you set @DEBUG=1@ in the
-- environment you'll get debugging messages. We use 'unsafePerformIO' to make
-- sure that the call to 'getEnv' is only made once.
--
-- /N.B./ this is an internal interface, please don't write external code that
-- depends on it.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-cse         #-}


module Snap.Internal.Debug where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Trans
import           Data.Char
import           Data.List
import           Data.Maybe
import           Foreign.C.Error
import           System.Environment
import           System.IO
import           System.IO.Unsafe
import           Text.Printf
------------------------------------------------------------------------------

debug, debugErrno :: forall m . (MonadIO m => String -> m ())


#ifndef NODEBUG

{-# NOINLINE debug #-}
debug = let !x = unsafePerformIO $! do
            !e <- try $ getEnv "DEBUG"
            
            !f <- either (\(_::SomeException) -> return debugIgnore)
                         (\x -> if x == "1" || x == "on"
                                  then return debugOn
                                  else if x == "testsuite"
                                         then return debugSeq
                                         else return debugIgnore)
                         (fmap (map toLower) e)
            return $! f
        in x


{-# NOINLINE debugErrno #-}
debugErrno = let !x = unsafePerformIO $ do
                 e <- try $ getEnv "DEBUG"
                 
                 !f <- either (\(_::SomeException) -> return debugErrnoIgnore)
                              (\x -> if x == "1" || x == "on"
                                       then return debugErrnoOn
                                       else if x == "testsuite"
                                              then return debugErrnoSeq
                                              else return debugErrnoIgnore)
                              (fmap (map toLower) e)
                 return $! f
             in x


------------------------------------------------------------------------------
debugSeq :: (MonadIO m) => String -> m ()
debugSeq !s = let !s' = rnf s in return $! s' `deepseq` ()
{-# NOINLINE debugSeq #-}

debugErrnoSeq :: (MonadIO m) => String -> m ()
debugErrnoSeq !s = let !s' = rnf s in return $! s' `deepseq` ()
{-# NOINLINE debugErrnoSeq #-}

------------------------------------------------------------------------------
_debugMVar :: MVar ()
_debugMVar = unsafePerformIO $ newMVar ()
{-# NOINLINE _debugMVar #-}


------------------------------------------------------------------------------
debugOn :: (MonadIO m) => String -> m ()
debugOn s = liftIO $ withMVar _debugMVar $ \_ -> do
                tid <- myThreadId
                hPutStrLn stderr $ s' tid
                hFlush stderr
  where
    chop x = let y = fromMaybe x $ stripPrefix "ThreadId " x
             in printf "%8s" y

    s' t   = "[" ++ chop (show t) ++ "] " ++ s

{-# NOINLINE debugOn #-}


------------------------------------------------------------------------------
debugErrnoOn :: (MonadIO m) => String -> m ()
debugErrnoOn loc = liftIO $ do
    err <- getErrno
    let ex = errnoToIOError loc err Nothing Nothing
    debug $ show ex
------------------------------------------------------------------------------

#else

debug      = debugIgnore
debugErrno = debugErrnoIgnore

#endif

------------------------------------------------------------------------------
debugIgnore :: (MonadIO m) => String -> m ()
debugIgnore _ = return ()

debugErrnoIgnore :: (MonadIO m) => String -> m ()
debugErrnoIgnore _ = return ()
------------------------------------------------------------------------------
