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
