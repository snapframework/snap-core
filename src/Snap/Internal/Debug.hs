-- | An internal Snap module for (optionally) printing debugging
-- messages. Normally 'debug' does nothing, but you can pass \"-fdebug\" to
-- @cabal install@ to build a @snap-core@ which debugs to stderr.
--
-- /N.B./ this is an internal interface, please don't write external code that
-- depends on it.

{-# LANGUAGE CPP #-}

module Snap.Internal.Debug where

import           Control.Monad.Trans

#ifdef DEBUG

------------------------------------------------------------------------------
import           Control.Concurrent
import           Foreign.C.Error
import           System.IO
import           System.IO.Unsafe


_debugMVar :: MVar ()
_debugMVar = unsafePerformIO $ newMVar ()

debug :: (MonadIO m) => String -> m ()
debug s = liftIO $ withMVar _debugMVar $ \_ -> hPutStrLn stderr s >> hFlush stderr
{-# INLINE debug #-}

debugErrno :: (MonadIO m) => String -> m ()
debugErrno loc = liftIO $ do
    err <- getErrno
    let ex = errnoToIOError loc err Nothing Nothing
    debug $ show ex
------------------------------------------------------------------------------

#else

------------------------------------------------------------------------------
debug :: (MonadIO m) => String -> m ()
debug _ = return ()
{-# INLINE debug #-}

debugErrno :: (MonadIO m) => String -> m ()
debugErrno _ = return ()
------------------------------------------------------------------------------

#endif
