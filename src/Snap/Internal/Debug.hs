-- | An internal Snap module for (optionally) printing debugging
-- messages. Normally 'debug' does nothing, but you can hand-edit this file to
-- cause debug messages to be sent to @stdout@.
--
-- /N.B./ this is an internal interface, please don't write external code that
-- depends on it.

module Snap.Internal.Debug where

import           Control.Monad.Trans

{-
import           Control.Concurrent
import           Foreign.C.Error
import           System.IO
import           System.IO.Unsafe


_debugMVar :: MVar ()
_debugMVar = unsafePerformIO $ newMVar ()

debug :: (MonadIO m) => String -> m ()
debug s = liftIO $ withMVar _debugMVar $ \_ -> hPutStrLn stderr s >> hFlush stderr

debugErrno :: (MonadIO m) => String -> m ()
debugErrno loc = liftIO $ do
    err <- getErrno
    let ex = errnoToIOError loc err Nothing Nothing
    debug $ show ex
-}

-- {-
debug :: (MonadIO m) => String -> m ()
debug _ = return ()

debugErrno :: (MonadIO m) => String -> m ()
debugErrno _ = return ()
-- -}
{-# INLINE debug #-}
