{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

This module contains the core type definitions, class instances, and functions
for the 'Snap' monad, which is the monad used for web handlers in the Snap
framework.

-}
module Snap.Types
  ( 
    -- * The Snap Monad
    Snap
  , runSnap
  , NoHandlerException(..)

    -- ** Functions for control flow and early termination
  , finishWith
  , pass

    -- ** Access to state
  , getRequest
  , getResponse
  , putRequest
  , putResponse
  , modifyRequest
  , modifyResponse
  , localRequest

    -- ** Grabbing request bodies
  , runRequestBody
  , getRequestBody

    -- * HTTP
    -- ** Datatypes
  , Cookie(..)
  , Headers
  , HasHeaders(..)
  , HttpVersion
  , Method(..)
  , Params
  , Request
  , Response

    -- ** Headers
  , addHeader
  , setHeader
  , getHeader

    -- ** Requests
  , rqServerName
  , rqServerPort
  , rqRemoteAddr
  , rqRemotePort
  , rqLocalAddr
  , rqLocalHostname
  , rqIsSecure
  , rqContentLength
  , rqMethod
  , rqVersion
  , rqCookies
  , rqPathInfo
  , rqContextPath
  , rqURI
  , rqQueryString
  , rqParams
  , rqParam
  , rqModifyParams
  , rqSetParam

    -- ** Responses
  , emptyResponse
  , setResponseBody
  , setResponseStatus
  , rspStatus
  , rspStatusReason
  , modifyResponseBody
  , setContentType
  , addCookie
  , setContentLength
  , clearContentLength

    -- * Iteratee
  , Enumerator
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception
import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as L
import qualified Data.Iteratee as Iter
import           Data.Maybe
import           Data.Typeable
------------------------------------------------------------------------------
import           Snap.Iteratee (Iteratee, run, fromWrap, stream2stream, enumBS)
import           Snap.Internal.Http.Types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- The Snap Monad
------------------------------------------------------------------------------

{-|

'Snap' is the 'Monad' that user web handlers run in. 'Snap' gives you:

1. stateful access to fetch or modify the HTTP 'Request'

2. stateful access to fetch or modify the HTTP 'Response'

3. failure \/ 'Alternative' \/ 'MonadPlus' semantics: a 'Snap' handler can
   choose not to handle a given request, using 'empty' or its synonym 'pass',
   and you can try alternative handlers with the '<|>' operator:

   > a :: Snap String
   > a = pass
   >
   > b :: Snap String
   > b = return "foo"
   >
   > c :: Snap String
   > c = a <|> b             -- try running a, if it fails then try b

4. early termination: if you call 'finishWith':

   @FIXME: prefer something nicer than setResponseBody (enumBS ....)@

   > a :: Snap ()
   > a = do
   >   r <- getResponse
   >   finishWith (setResponseStatus 500 $
   >               setResponseBody (enumBS $ B.pack "500 error") r)

   then any subsequent processing will be skipped and supplied 'Response' value
   will be returned from 'runSnap' as-is.

5. access to the 'IO' monad through a 'MonadIO' instance:

   > a :: Snap ()
   > a = liftIO fireTheMissiles
-}

newtype Snap a = Snap {
      unSnap :: StateT SnapState (Iteratee IO) (Maybe (Either Response a))
}


data SnapState = SnapState
    { _snapRequest  :: Request
    , _snapResponse :: Response }


instance Monad Snap where
    (Snap m) >>= f =
        Snap $ do
            eth <- m
            maybe (return Nothing)
                  (either (return . Just . Left)
                          (unSnap . f))
                  eth

    return = Snap . return . Just . Right
    fail   = const $ Snap $ return Nothing

instance MonadIO Snap where
    liftIO m = Snap $ liftM (Just . Right) $ liftIO m


instance MonadPlus Snap where
    mzero = Snap $ return Nothing

    a `mplus` b =
        Snap $ do
            mb <- unSnap a
            if isJust mb then return mb else unSnap b


instance Functor Snap where
    fmap = liftM


instance Applicative Snap where
    pure  = return
    (<*>) = ap

    
instance Alternative Snap where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------

-- | Given an iteratee (data consumer), send the request body through it and
-- return the result
runRequestBody :: Iteratee IO a -> Snap a
runRequestBody iter = do
    req  <- getRequest
    let enum = rqBody req

    -- make sure the iteratee consumes all of the output
    let iter' = iter >>= (\a -> Iter.skipToEof >> return a)

    -- run the iteratee
    result <- liftIO $ enum iter' >>= run

    -- stuff a new dummy enumerator into the request, so you can only try to
    -- read the request body from the socket once
    modifyRequest $ \r -> r { rqBody = enumBS "" }

    return result


-- | Return the request body as a bytestring
getRequestBody :: Snap L.ByteString
getRequestBody = liftM fromWrap $ runRequestBody stream2stream
{-# INLINE getRequestBody #-}

-- | Short-circuit a 'Snap' monad action early, storing the given 'Response'
-- value in its state
finishWith :: Response -> Snap ()
finishWith = Snap . return . Just . Left
{-# INLINE finishWith #-}

-- | Fails out of a 'Snap' monad action; used to indicate that you choose not
-- to handle the given request within the given handler
pass :: Snap a
pass = empty


-- | Local Snap version of 'get'
sget :: Snap SnapState
sget = Snap $ liftM (Just . Right) get
{-# INLINE sget #-}

-- | Local Snap monad version of 'modify'
smodify :: (SnapState -> SnapState) -> Snap ()
smodify f = Snap $ modify f >> return (Just $ Right ())
{-# INLINE smodify #-}

-- | Grab the 'Request' object out of the 'Snap' monad
getRequest :: Snap Request
getRequest = liftM _snapRequest sget
{-# INLINE getRequest #-}

-- | Grab the 'Response' object out of the 'Snap' monad
getResponse :: Snap Response
getResponse = liftM _snapResponse sget
{-# INLINE getResponse #-}

-- | Put a new 'Response' object into the 'Snap' monad
putResponse :: Response -> Snap ()
putResponse r = smodify $ \ss -> ss { _snapResponse = r }
{-# INLINE putResponse #-}

-- | Put a new 'Request' object into the 'Snap' monad
putRequest :: Request -> Snap ()
putRequest r = smodify $ \ss -> ss { _snapRequest = r }
{-# INLINE putRequest #-}

-- | Modify the 'Request' object stored in a 'Snap' monad
modifyRequest :: (Request -> Request) -> Snap ()
modifyRequest f = smodify $ \ss -> ss { _snapRequest = f $ _snapRequest ss }
{-# INLINE modifyRequest #-}

-- | Modify the 'Response' object stored in a 'Snap' monad
modifyResponse :: (Response -> Response) -> Snap () 
modifyResponse f = smodify $ \ss -> ss { _snapResponse = f $ _snapResponse ss }
{-# INLINE modifyResponse #-}

-- | Run a 'Snap' action with a locally-modified 'Request' state object. The
-- 'Request' object in the Snap monad state after the call to localRequest will
-- be unchanged.
localRequest :: (Request -> Request) -> Snap a -> Snap a
localRequest f m = do
    req <- getRequest
    modifyRequest f
    result <- m
    putRequest req
    return result
{-# INLINE localRequest #-}


-- | This exception is thrown if the handler you supply to 'runSnap' fails.
data NoHandlerException = NoHandlerException
   deriving (Eq, Typeable)

instance Show NoHandlerException where
    show NoHandlerException = "No handler for request"

instance Exception NoHandlerException


-- | Run a 'Snap' monad action in the 'Iteratee IO' monad.
runSnap :: Snap a -> Request -> Iteratee IO (Request,Response)
runSnap (Snap m) req = do
    (r, ss') <- runStateT m ss

    e <- maybe (liftIO $ throwIO NoHandlerException)
               return
               r

    -- is this a case of early termination?
    let resp = case e of 
                 Left x  -> x
                 Right _ -> _snapResponse ss'

    return (_snapRequest ss', resp)

  where
    ss = SnapState req emptyResponse
{-# INLINE runSnap #-}


