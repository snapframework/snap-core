{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Instances where

import                       Control.Applicative
import           "monads-fd" Control.Monad.Cont
import           "monads-fd" Control.Monad.Error
import           "monads-fd" Control.Monad.List
import           "monads-fd" Control.Monad.RWS.Strict hiding (pass)
import qualified "monads-fd" Control.Monad.RWS.Lazy as LRWS
import           "monads-fd" Control.Monad.Reader
import           "monads-fd" Control.Monad.State.Strict
import qualified "monads-fd" Control.Monad.State.Lazy as LState
import           "monads-fd" Control.Monad.Writer.Strict hiding (pass)
import qualified "monads-fd" Control.Monad.Writer.Lazy as LWriter
import                       Prelude hiding (catch)

------------------------------------------------------------------------------
import                       Snap.Internal.Types


------------------------------------------------------------------------------
instance MonadPlus m => MonadPlus (ContT c m) where
    mzero = lift mzero
    m `mplus` n = ContT $ \ f -> runContT m f `mplus` runContT n f


------------------------------------------------------------------------------
instance MonadPlus m => Alternative (ContT c m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
instance MonadSnap m => MonadSnap (ContT c m) where
    liftSnap = lift . liftSnap


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
