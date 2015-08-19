{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE PackageImports #-}

-- Note re: "-fno-warn-warnings-deprecations" above: transformers has
-- deprecated Control.Monad.Trans.Error (which we like) but we are going to
-- provide an ErrorT instance for compatibility until the deprecated modules
-- are removed.

module Snap.Internal.Instances () where
------------------------------------------------------------------------------
import           Control.Monad.Trans.Class         (MonadTrans (lift))
import           Control.Monad.Trans.Error         (Error, ErrorT)
#if MIN_VERSION_transformers(0,4,0)
import           Control.Monad.Trans.Except        (ExceptT)
#endif
import           Control.Monad.Trans.List          (ListT)
import           Control.Monad.Trans.Reader        (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy      as LRWS (RWST)
import           Control.Monad.Trans.RWS.Strict    (RWST)
import qualified Control.Monad.Trans.State.Lazy    as LState (StateT)
import           Control.Monad.Trans.State.Strict  (StateT)
import qualified Control.Monad.Trans.Writer.Lazy   as LWriter (WriterT)
import           Control.Monad.Trans.Writer.Strict (WriterT)
import           Snap.Internal.Core                (MonadSnap (..))
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid                       (Monoid)
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
instance (MonadSnap m, Error e) => MonadSnap (ErrorT e m) where
    liftSnap = lift . liftSnap

#if MIN_VERSION_transformers(0,4,0)
instance (MonadSnap m, Monoid e) => MonadSnap (ExceptT e m) where
    liftSnap = lift . liftSnap
#endif

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
