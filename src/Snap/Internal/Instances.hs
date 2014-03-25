{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Instances () where
------------------------------------------------------------------------------
import           Control.Monad.Trans.Class         (MonadTrans (lift))
import           Control.Monad.Trans.Error         (Error, ErrorT)
import           Control.Monad.Trans.List          (ListT)
import           Control.Monad.Trans.Reader        (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy      as LRWS (RWST)
import           Control.Monad.Trans.RWS.Strict    (RWST)
import qualified Control.Monad.Trans.State.Lazy    as LState (StateT)
import           Control.Monad.Trans.State.Strict  (StateT)
import qualified Control.Monad.Trans.Writer.Lazy   as LWriter (WriterT)
import           Control.Monad.Trans.Writer.Strict (WriterT)
import           Data.Monoid                       (Monoid)
import           Snap.Internal.Types               (MonadSnap (..))
------------------------------------------------------------------------------


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
