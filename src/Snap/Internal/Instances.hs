{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Instances where

import           Control.Applicative
import           Control.Exception.Control ()
import           Control.Monad.Cont
import           Control.Monad.Error
import           Control.Monad.List
import           Control.Monad.RWS.Strict    hiding (pass)
import qualified Control.Monad.RWS.Lazy      as LRWS
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Control.Monad.State.Lazy    as LState
import           Control.Monad.Writer.Strict hiding (pass)
import qualified Control.Monad.Writer.Lazy   as LWriter
import           Prelude                     hiding (catch)

------------------------------------------------------------------------------
import           Snap.Internal.Types


------------------------------------------------------------------------------
instance MonadPlus m => MonadPlus (ContT c m) where
    mzero = lift mzero
    m `mplus` n = ContT $ \ f -> runContT m f `mplus` runContT n f


------------------------------------------------------------------------------
instance MonadPlus m => Alternative (ContT c m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
{- instance MonadSnap m => MonadSnap (ContT c m) where-}
    {- liftSnap = lift . liftSnap-}
 -- TODO: Re-enable

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


instance (Error e, MonadCatchControl m) => MonadCatchControl (ErrorT e m)
instance (MonadCatchControl m) => MonadCatchControl (ListT m)
instance (MonadCatchControl m) => MonadCatchControl (StateT s m)
instance (MonadCatchControl m) => MonadCatchControl (ReaderT r m)
instance (Monoid w, MonadCatchControl m) => MonadCatchControl (RWST r w s m)
instance (Monoid w, MonadCatchControl m) => MonadCatchControl (LRWS.RWST r w s m)
instance (Monoid w, MonadCatchControl m) => MonadCatchControl (WriterT w m)
instance (Monoid w, MonadCatchControl m) => MonadCatchControl (LWriter.WriterT w m)

