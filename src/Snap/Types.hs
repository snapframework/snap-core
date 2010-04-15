{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-|

This module contains the core type definitions, class instances, and functions
for HTTP as well as the 'Snap' monad, which is used for web handlers.

-}
module Snap.Types
  ( 
    -- * HTTP
    -- ** Datatypes
    Cookie(..)
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
  , setResponseStatus
  , rspStatus
  , rspStatusReason
  , setContentType
  , addCookie
  , setContentLength
  , clearContentLength

    -- *** Response I/O
  , setResponseBody
  , modifyResponseBody
  , addToOutput
  , writeBS
  , writeLBS

    -- * The Snap Monad
  , Snap
  , runSnap
  , NoHandlerException(..)

    -- ** Functions for control flow and early termination
  , finishWith
  , pass

    -- ** Routing
  , method
  , path
  , dir
  , top
  , route

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
  , unsafeDetachRequestBody

    -- * Iteratee
  , Enumerator

    -- * HTTP utilities
  , formatHttpTime
  , parseHttpTime 
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception
import           Control.Monad.State.Strict
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Iteratee as Iter
import           Data.Maybe
import           Data.Monoid
import qualified Data.Map as Map
import           Data.Typeable
------------------------------------------------------------------------------
import           Snap.Iteratee ( Iteratee
                               , run
                               , (>.)
                               , fromWrap
                               , stream2stream
                               , enumBS
                               , enumLBS )
import           Snap.Internal.Http.Types
import           Snap.Internal.Debug
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- The Snap Monad
------------------------------------------------------------------------------

{-|

'Snap' is the 'Monad' that user web handlers run in. 'Snap' gives you:

1. stateful access to fetch or modify an HTTP 'Request'

2. stateful access to fetch or modify an HTTP 'Response'

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

4. convenience functions ('writeBS', 'writeLBS', 'addToOutput') for writing
output to the 'Response':

   > a :: (forall a . Enumerator a) -> Snap ()
   > a someEnumerator = do
   >     writeBS "I'm a strict bytestring"
   >     writeLBS "I'm a lazy bytestring"
   >     addToOutput someEnumerator

5. early termination: if you call 'finishWith':

   > a :: Snap ()
   > a = do
   >   modifyResponse $ setResponseStatus 500
   >   writeBS "500 error"
   >   r <- getResponse
   >   finishWith r

   then any subsequent processing will be skipped and supplied 'Response' value
   will be returned from 'runSnap' as-is.

6. access to the 'IO' monad through a 'MonadIO' instance:

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


{-

'Route' is the internal data type you use to build a routing tree. Matching is
done unambiguously.

'Capture' and 'Dir' routes can have a "fallback" route:

  - For 'Capture', the fallback is routed when there is nothing to capture
  - For 'Dir', the fallback is routed when we can't find a route in its map

Fallback routes are stacked: i.e. for a route like:

> Dir [("foo", Capture "bar" (Action bar) Unrouted)] baz

visiting the URI foo/ will result in the "bar" capture being empty and
triggering its fallback. It's Unrouted, so we go to the nearest parent
fallback and try that, which is the baz action.

-}
data Route = Action (Snap ())                     -- wraps a 'Snap' action
           | Capture ByteString Route Route       -- captures the dir in a param
           | Dir (Map.Map ByteString Route) Route -- match on a dir
           | Unrouted

instance Show Route where
    show (Action _)       = "action"
    show (Capture p r fb) = concat [ "capture ", show p, " (", show r, ")"
                                   , " (fallback: ", show fb, ")" ]
    show (Dir rm fb)      = concat [ "dir ", show rm
                                   , " (fallback: ", show fb, ")" ]
    show Unrouted         = "unrouted"

instance Monoid Route where
    mempty = Unrouted

    -- Unions two routes, favoring the right-hand side
    mappend Unrouted r = r

    mappend l@(Action _) r = case r of
      (Action _)        -> r
      (Capture p r' fb) -> Capture p r' (mappend fb l)
      (Dir _ _)         -> mappend (Dir Map.empty l) r
      Unrouted          -> l

    mappend (Capture p r' fb) r = Capture p (mappend r' r) fb

    mappend l@(Dir rm fb) r = case r of
      (Action _)      -> Dir rm (mappend fb r)
      (Capture _ _ _) -> Dir rm (mappend fb r)
      (Dir rm' fb')   -> Dir (Map.unionWith mappend rm rm') (mappend fb fb')
      Unrouted        -> l


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


-- | Detach the request body's 'Enumerator' from the 'Request' and return
-- it. You would want to use this if you needed to send the HTTP request body
-- (transformed or otherwise) through to the output in O(1) space. (Examples:
-- transcoding, \"echo\", etc)
-- 
-- Normally Snap is careful to ensure that the request body is fully consumed
-- after your web handler runs; this function is marked \"unsafe\" because it
-- breaks this guarantee and leaves the responsibility up to you. If you don't
-- fully consume the 'Enumerator' you get here, the next HTTP request in the
-- pipeline (if any) will misparse. Be careful with exception handlers.
unsafeDetachRequestBody :: Snap (Enumerator a)
unsafeDetachRequestBody = do
    req <- getRequest
    let enum = rqBody req
    putRequest $ req {rqBody = enumBS ""}
    return enum

-- | Short-circuit a 'Snap' monad action early, storing the given 'Response'
-- value in its state
finishWith :: Response -> Snap ()
finishWith = Snap . return . Just . Left
{-# INLINE finishWith #-}

-- | Fails out of a 'Snap' monad action; used to indicate that you choose not
-- to handle the given request within the given handler
pass :: Snap a
pass = empty

-- | Runs a 'Snap' monad action only for the given method
method :: Method -> Snap a -> Snap a
method m action = do
    req <- getRequest
    unless (rqMethod req == m) pass
    action
{-# INLINE method #-}


-- Take n bytes of the path info and append it to the context path, with the
-- trailing slash
updateContextPath :: Int -> Request -> Request
updateContextPath n req | n > 0     = req { rqContextPath = ctx
                                          , rqPathInfo    = pinfo }
                        | otherwise = req
  where
    ctx'  = B.take n (rqPathInfo req)
    ctx   = B.concat [rqContextPath req, ctx', "/"]
    pinfo = B.drop (n+1) (rqPathInfo req)

-- Runs a 'Snap' monad action only for requests with a path that matches
-- the comparator
pathWith :: (ByteString -> ByteString -> Bool)
         -> ByteString
         -> Snap a
         -> Snap a
pathWith c p action = do
    req <- getRequest
    unless (c p (rqPathInfo req)) pass
    modifyRequest $ updateContextPath (B.length p)
    action

-- | Runs a 'Snap' monad action only for requests starting with a given path
dir :: ByteString -> Snap a -> Snap a
dir = pathWith B.isPrefixOf
{-# INLINE dir #-}

-- | Runs a 'Snap' monad action only for requests with the same path as
-- the given one
path :: ByteString -> Snap a -> Snap a
path = pathWith (==)
{-# INLINE path #-}

-- | Runs a 'Snap' monad action only for the top level
top :: Snap a -> Snap a
top = path ""
{-# INLINE top #-}

-- | Routes many 'Snap' monad actions unambiguously in O(log n)
--
-- The routes are given as relative paths. If a component starts with colon
-- (":"), it tells the router to capture that component. Captured components
-- are available in the params of the 'Request'.
--
-- Longer paths are matched first, and specific routes are matched before
-- captures. That is, if I have routes for @a@, @a/b@, and @a/:x@, a request
-- to @a/b@ will go to @a/b@, @a/s@ for any s will go to @a/:x@, and @a@ will
-- go to @a@.
--
-- The following example matches "article" to an article index, "login" to
-- a login, and "article/:id" to an article renderer.
--
-- > route [ ("article",     renderIndex)
-- >       , ("article/:id", renderArticle)
-- >       , ("login",       method POST doLogin) ]
route :: [(ByteString, Snap ())] -> Snap ()
route rts = do
  -- FIXME How do we make this debug only print once?
  debug $ "Types.route: " ++ show rts'
  route' rts' []
  where
    rts' = mconcat (map pRoute rts)

pRoute :: (ByteString, Snap ()) -> Route
pRoute (r, a) = foldr f (Action a) hier
  where
    hier   = filter (not . B.null) $ B.splitWith (== (c2w '/')) r
    f s rt = if B.head s == c2w ':'
        then Capture (B.tail s) rt Unrouted
        else Dir (Map.fromList [(s, rt)]) Unrouted

route' :: Route -> [Route] -> Snap ()
route' (Action action) _ = action

route' (Capture param rt fb) fbs = do
    cwd <- getRequest >>= return . B.takeWhile (/= (c2w '/')) . rqPathInfo
    if B.null cwd
      then route' fb fbs
      else do modifyRequest $ updateContextPath (B.length cwd) . (f cwd)
              route' rt (fb:fbs)
  where
    f v req = req { rqParams = Map.insertWith (++) param [v] (rqParams req) }

route' (Dir rtm fb) fbs = do
    cwd <- getRequest >>= return . B.takeWhile (/= (c2w '/')) . rqPathInfo
    case Map.lookup cwd rtm of
      Just rt -> do
          modifyRequest $ updateContextPath (B.length cwd)
          route' rt (fb:fbs)
      Nothing -> route' fb fbs

route' Unrouted       [] = pass
route' Unrouted (fb:fbs) = route' fb fbs


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


-- | Add the output from the given enumerator to the 'Response' stored in the
-- 'Snap' monad state.
addToOutput :: (forall a . Enumerator a)   -- ^ output to add
            -> Snap ()
addToOutput enum = modifyResponse $ modifyResponseBody (>. enum)


-- | Add the given strict 'ByteString' to the body of the 'Response' stored in
-- the 'Snap' monad state.
writeBS :: ByteString -> Snap ()
writeBS s = addToOutput $ enumBS s


-- | Add the given lazy 'L.ByteString' to the body of the 'Response' stored in
-- the 'Snap' monad state.
writeLBS :: L.ByteString -> Snap ()
writeLBS s = addToOutput $ enumLBS s


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


