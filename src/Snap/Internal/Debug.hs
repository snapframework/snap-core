-- | An internal Snap module for (optionally) printing debugging messages. To
-- enable debug output, compile the library with the @debug@ flag (off by
-- default) and set the environment variable @DEBUG@ to @1@. We use
-- 'unsafePerformIO' to make sure that the call to 'getEnv' is only made once.
--
-- /N.B./ this is an internal interface, please don't write external code that
-- depends on it.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-cse         #-}


module Snap.Internal.Debug (debug, debugErrno) where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO (..))

#ifndef NODEBUG
import           Control.Concurrent     (MVar, myThreadId, newMVar, withMVar)
import           Control.Exception      (SomeException, try)
import           Data.Char              (toLower)
import           Data.List              (stripPrefix)
import           Data.Maybe             (fromMaybe)
import           Foreign.C.Error        (errnoToIOError, getErrno)
import           System.Environment     (getEnv)
import           System.IO              (hFlush, hPutStrLn, stderr)
import           System.IO.Unsafe       (unsafePerformIO)
import           Text.Printf            (printf)
#endif
------------------------------------------------------------------------------

-- | Print out the provided debug message prefixed by the thread ID.
--
-- Example:
--
-- @
-- ghci> debug "Some debug message"
-- [     225] Some debug message
-- @
debug :: MonadIO m => String -> m ()


-- | Print out the error message corresponding to the 'Foreign.C.Error.Errno'
-- value returned by 'Foreign.C.Error.getErrno' together with any additional
-- information provided by the user (usually the location where the error
-- occurred).
--
-- Example:
--
-- @
-- ghci> debugErrno "path/to/Source.hs:34"
-- [     323] path/to/Source.hs:34: failed (Success)
-- @
debugErrno :: MonadIO m => String -> m ()


#ifndef NODEBUG

{-# NOINLINE debug #-}
debug = let !x = unsafePerformIO $ do
                !e <- try $ getEnv "DEBUG"

                !f <- either (\(_::SomeException) -> return debugIgnore)
                             (\y0 -> let y = map toLower y0
                                     in if y == "1" || y == "on"
                                       then return debugOn
                                       else if y == "testsuite"
                                              then return debugSeq
                                              else return debugIgnore)
                             e
                return $! f
        in x


{-# NOINLINE debugErrno #-}
debugErrno = let !x = unsafePerformIO $ do
                     e <- try $ getEnv "DEBUG"
                     !f <- either (\(_::SomeException) -> return debugErrnoIgnore)
                                  (\y0 -> let y = map toLower y0
                                          in if y == "1" || y == "on"
                                            then return debugErrnoOn
                                            else if y == "testsuite"
                                                   then return debugSeq
                                                   else return debugErrnoIgnore)
                                  e
                     return $! f
             in x


------------------------------------------------------------------------------
debugSeq :: (MonadIO m) => String -> m ()
debugSeq !s = length s `seq` return $! ()
{-# NOINLINE debugSeq #-}

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
{-# INLINE debug #-}

debugErrno = debugErrnoIgnore
{-# INLINE debugErrno #-}

#endif

------------------------------------------------------------------------------
debugIgnore :: (MonadIO m) => String -> m ()
debugIgnore _ = return ()
{-# INLINE debugIgnore #-}

debugErrnoIgnore :: (MonadIO m) => String -> m ()
debugErrnoIgnore _ = return ()
{-# INLINE debugErrnoIgnore #-}
------------------------------------------------------------------------------
