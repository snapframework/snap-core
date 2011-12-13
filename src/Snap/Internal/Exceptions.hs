{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | An internal Snap module containing the exception that escapes HTTP types.
--
-- /N.B./ this is an internal interface, please don't write user code that
-- depends on it. Interfaces subject to change etc etc etc.
--
module Snap.Internal.Exceptions where

------------------------------------------------------------------------------
import           Control.Exception
import           Data.ByteString.Char8 (ByteString)
import           Data.Typeable
import           Snap.Iteratee

------------------------------------------------------------------------------
-- | An exception hierarchy for exceptions that cannot be caught by
-- user-defined error handlers
data UncatchableException = forall e. Exception e => UncatchableException e
  deriving (Typeable)


------------------------------------------------------------------------------
instance Show UncatchableException where
    show (UncatchableException e) = "Uncatchable exception: " ++ show e


------------------------------------------------------------------------------
instance Exception UncatchableException


------------------------------------------------------------------------------
uncatchableExceptionToException :: Exception e => e -> SomeException
uncatchableExceptionToException = toException . UncatchableException


------------------------------------------------------------------------------
uncatchableExceptionFromException :: Exception e => SomeException -> Maybe e
uncatchableExceptionFromException e = do
    UncatchableException ue <- fromException e
    cast ue


------------------------------------------------------------------------------
data ConnectionTerminatedException =
    ConnectionTerminatedException SomeException
  deriving (Typeable)


------------------------------------------------------------------------------
instance Show ConnectionTerminatedException where
    show (ConnectionTerminatedException e) =
        "Connection terminated with exception: " ++ show e


------------------------------------------------------------------------------
instance Exception ConnectionTerminatedException where
    toException   = uncatchableExceptionToException
    fromException = uncatchableExceptionFromException


------------------------------------------------------------------------------
-- | This exception is thrown if the handler chooses to escape regular HTTP
-- traffic.
data EscapeHttpException = EscapeHttpException
    ((Int -> IO ()) -> Iteratee ByteString IO () -> Iteratee ByteString IO ())
        deriving (Typeable)


------------------------------------------------------------------------------
instance Show EscapeHttpException where
    show = const "HTTP traffic was escaped"


------------------------------------------------------------------------------
instance Exception EscapeHttpException where
    toException   = uncatchableExceptionToException
    fromException = uncatchableExceptionFromException


