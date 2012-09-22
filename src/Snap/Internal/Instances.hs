{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE PackageImports       #-}

module Snap.Internal.Instances where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.List
import           Control.Monad.Trans.RWS.Strict    hiding (pass)
import qualified Control.Monad.Trans.RWS.Lazy      as LRWS
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy    as LState
import           Control.Monad.Trans.Writer.Strict hiding (pass)
import qualified Control.Monad.Trans.Writer.Lazy   as LWriter
import           Data.Monoid
#if !MIN_VERSION_base(4,6,0)
import           Prelude                     hiding (catch)
#endif

------------------------------------------------------------------------------
import           Snap.Internal.Types


------------------------------------------------------------------------------
instance (MonadSnap m, Error e) => MonadSnap (ErrorT e m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance MonadSnap m => MonadSnap (ListT m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance (MonadSnap m, Monoid w) => MonadSnap (RWST r w s m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance (MonadSnap m, Monoid w) => MonadSnap (LRWS.RWST r w s m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance MonadSnap m => MonadSnap (ReaderT r m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance MonadSnap m => MonadSnap (StateT s m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance MonadSnap m => MonadSnap (LState.StateT s m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance (MonadSnap m, Monoid w) => MonadSnap (WriterT w m) where
    liftSnap = lift . liftSnap


------------------------------------------------------------------------------
instance (MonadSnap m, Monoid w) => MonadSnap (LWriter.WriterT w m) where
    liftSnap = lift . liftSnap
