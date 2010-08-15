{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Snap.Internal.Types where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.CatchIO
import           Control.Monad.State.Strict
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.CIByteString as CIB
import           Data.IORef
import qualified Data.Iteratee as Iter
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import           Data.Typeable
import qualified Data.IntMap as IM

------------------------------------------------------------------------------
import           Snap.Iteratee hiding (Enumerator)
import           Snap.Internal.Http.Types


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

4. convenience functions ('writeBS', 'writeLBS', 'writeText', 'writeLazyText',
   'addToOutput') for writing output to the 'Response':

   > a :: (forall a . Enumerator a) -> Snap ()
   > a someEnumerator = do
   >     writeBS "I'm a strict bytestring"
   >     writeLBS "I'm a lazy bytestring"
   >     addToOutput someEnumerator

5. early termination: if you call 'finishWith':

   > a :: Snap ()
   > a = do
   >   modifyResponse $ setResponseStatus 500 "Internal Server Error"
   >   writeBS "500 error"
   >   r <- getResponse
   >   finishWith r

   then any subsequent processing will be skipped and supplied 'Response' value
   will be returned from 'runSnap' as-is.

6. access to the 'IO' monad through a 'MonadIO' instance:

   > a :: Snap ()
   > a = liftIO fireTheMissiles
-}


------------------------------------------------------------------------------
newtype Snap a = Snap {
      unSnap :: StateT SnapState (Iteratee IO) (Maybe (Either Response a))
}


------------------------------------------------------------------------------
data SnapState = SnapState
    { _snapRequest  :: Request
    , _snapResponse :: Response
    , _snapLogError :: ByteString -> IO () }


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
instance MonadIO Snap where
    liftIO m = Snap $ liftM (Just . Right) $ liftIO m


------------------------------------------------------------------------------
instance MonadCatchIO Snap where
    catch (Snap m) handler = Snap $ do
        x <- try m
        case x of
          (Left e)  -> let (Snap z) = handler e in z
          (Right y) -> return y

    block (Snap m) = Snap $ block m
    unblock (Snap m) = Snap $ unblock m


------------------------------------------------------------------------------
instance MonadPlus Snap where
    mzero = Snap $ return Nothing

    a `mplus` b =
        Snap $ do
            mb <- unSnap a
            if isJust mb then return mb else unSnap b


------------------------------------------------------------------------------
instance Functor Snap where
    fmap = liftM


------------------------------------------------------------------------------
instance Applicative Snap where
    pure  = return
    (<*>) = ap


------------------------------------------------------------------------------
instance Alternative Snap where
    empty = mzero
    (<|>) = mplus

------------------------------------------------------------------------------
-- | The Typeable instance is here so Snap can be dynamically executed with
-- Hint.
snapTyCon :: TyCon
snapTyCon = mkTyCon "Snap.Types.Snap"
{-# NOINLINE snapTyCon #-}

instance Typeable1 Snap where
    typeOf1 _ = mkTyConApp snapTyCon []


------------------------------------------------------------------------------
liftIter :: Iteratee IO a -> Snap a
liftIter i = Snap (lift i >>= return . Just . Right)


------------------------------------------------------------------------------
-- | Sends the request body through an iteratee (data consumer) and
-- returns the result.
runRequestBody :: Iteratee IO a -> Snap a
runRequestBody iter = do
    req  <- getRequest
    senum <- liftIO $ readIORef $ rqBody req
    let (SomeEnumerator enum) = senum

    -- make sure the iteratee consumes all of the output
    let iter' = iter >>= (\a -> Iter.skipToEof >> return a)

    -- run the iteratee
    result <- liftIter $ Iter.joinIM $ enum iter'

    -- stuff a new dummy enumerator into the request, so you can only try to
    -- read the request body from the socket once
    liftIO $ writeIORef (rqBody req)
                        (SomeEnumerator $ return . Iter.joinI . Iter.take 0 )

    return result


------------------------------------------------------------------------------
-- | Returns the request body as a bytestring.
getRequestBody :: Snap L.ByteString
getRequestBody = liftM fromWrap $ runRequestBody stream2stream
{-# INLINE getRequestBody #-}


------------------------------------------------------------------------------
-- | Detaches the request body's 'Enumerator' from the 'Request' and
-- returns it. You would want to use this if you needed to send the
-- HTTP request body (transformed or otherwise) through to the output
-- in O(1) space. (Examples: transcoding, \"echo\", etc)
--
-- Normally Snap is careful to ensure that the request body is fully
-- consumed after your web handler runs; this function is marked
-- \"unsafe\" because it breaks this guarantee and leaves the
-- responsibility up to you. If you don't fully consume the
-- 'Enumerator' you get here, the next HTTP request in the pipeline
-- (if any) will misparse. Be careful with exception handlers.
unsafeDetachRequestBody :: Snap (Enumerator a)
unsafeDetachRequestBody = do
    req <- getRequest
    let ioref = rqBody req
    senum <- liftIO $ readIORef ioref
    let (SomeEnumerator enum) = senum
    liftIO $ writeIORef ioref
               (SomeEnumerator $ return . Iter.joinI . Iter.take 0)
    return enum


------------------------------------------------------------------------------
-- | Short-circuits a 'Snap' monad action early, storing the given
-- 'Response' value in its state.
finishWith :: Response -> Snap ()
finishWith = Snap . return . Just . Left
{-# INLINE finishWith #-}


------------------------------------------------------------------------------
-- | Fails out of a 'Snap' monad action.  This is used to indicate
-- that you choose not to handle the given request within the given
-- handler.
pass :: Snap a
pass = empty


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only if the request's HTTP method matches
-- the given method.
method :: Method -> Snap a -> Snap a
method m action = do
    req <- getRequest
    unless (rqMethod req == m) pass
    action
{-# INLINE method #-}


------------------------------------------------------------------------------
-- Appends n bytes of the path info to the context path with a
-- trailing slash.
updateContextPath :: Int -> Request -> Request
updateContextPath n req | n > 0     = req { rqContextPath = ctx
                                          , rqPathInfo    = pinfo }
                        | otherwise = req
  where
    ctx'  = S.take n (rqPathInfo req)
    ctx   = S.concat [rqContextPath req, ctx', "/"]
    pinfo = S.drop (n+1) (rqPathInfo req)


------------------------------------------------------------------------------
-- Runs a 'Snap' monad action only if the 'rqPathInfo' matches the given
-- predicate.
pathWith :: (ByteString -> ByteString -> Bool)
         -> ByteString
         -> Snap a
         -> Snap a
pathWith c p action = do
    req <- getRequest
    unless (c p (rqPathInfo req)) pass
    localRequest (updateContextPath $ S.length p) action


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only when the 'rqPathInfo' of the request
-- starts with the given path. For example,
--
-- > dir "foo" handler
--
-- Will fail if 'rqPathInfo' is not \"@\/foo@\" or \"@\/foo\/...@\", and will
-- add @\"foo\/\"@ to the handler's local 'rqContextPath'.
dir :: ByteString  -- ^ path component to match
    -> Snap a      -- ^ handler to run
    -> Snap a
dir = pathWith f
  where
    f dr pinfo = dr == x
      where
        (x,_) = S.break (=='/') pinfo
{-# INLINE dir #-}


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only for requests where 'rqPathInfo' is exactly
-- equal to the given string. If the path matches, locally sets 'rqContextPath'
-- to the old value of 'rqPathInfo', sets 'rqPathInfo'=\"\", and runs the given
-- handler.
path :: ByteString  -- ^ path to match against
     -> Snap a      -- ^ handler to run
     -> Snap a
path = pathWith (==)
{-# INLINE path #-}


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only when 'rqPathInfo' is empty.
ifTop :: Snap a -> Snap a
ifTop = path ""
{-# INLINE ifTop #-}


------------------------------------------------------------------------------
-- | Local Snap version of 'get'.
sget :: Snap SnapState
sget = Snap $ liftM (Just . Right) get
{-# INLINE sget #-}


------------------------------------------------------------------------------
-- | Local Snap monad version of 'modify'.
smodify :: (SnapState -> SnapState) -> Snap ()
smodify f = Snap $ modify f >> return (Just $ Right ())
{-# INLINE smodify #-}


------------------------------------------------------------------------------
-- | Grabs the 'Request' object out of the 'Snap' monad.
getRequest :: Snap Request
getRequest = liftM _snapRequest sget
{-# INLINE getRequest #-}


------------------------------------------------------------------------------
-- | Grabs the 'Response' object out of the 'Snap' monad.
getResponse :: Snap Response
getResponse = liftM _snapResponse sget
{-# INLINE getResponse #-}


------------------------------------------------------------------------------
-- | Puts a new 'Response' object into the 'Snap' monad.
putResponse :: Response -> Snap ()
putResponse r = smodify $ \ss -> ss { _snapResponse = r }
{-# INLINE putResponse #-}


------------------------------------------------------------------------------
-- | Puts a new 'Request' object into the 'Snap' monad.
putRequest :: Request -> Snap ()
putRequest r = smodify $ \ss -> ss { _snapRequest = r }
{-# INLINE putRequest #-}


------------------------------------------------------------------------------
-- | Modifies the 'Request' object stored in a 'Snap' monad.
modifyRequest :: (Request -> Request) -> Snap ()
modifyRequest f = smodify $ \ss -> ss { _snapRequest = f $ _snapRequest ss }
{-# INLINE modifyRequest #-}


------------------------------------------------------------------------------
-- | Modifes the 'Response' object stored in a 'Snap' monad.
modifyResponse :: (Response -> Response) -> Snap ()
modifyResponse f = smodify $ \ss -> ss { _snapResponse = f $ _snapResponse ss }
{-# INLINE modifyResponse #-}


------------------------------------------------------------------------------
-- | Performs a redirect by setting the @Location@ header to the given target
-- URL/path and the status code to 302 in the 'Response' object stored in a
-- 'Snap' monad. Note that the target URL is not validated in any way. Consider
-- using 'redirect\'' instead, which allows you to choose the correct status
-- code.
redirect :: ByteString -> Snap ()
redirect target = redirect' target 302
{-# INLINE redirect #-}


------------------------------------------------------------------------------
-- | Performs a redirect by setting the @Location@ header to the given target
-- URL/path and the status code (should be one of 301, 302, 303 or 307) in the
-- 'Response' object stored in a 'Snap' monad. Note that the target URL is not
-- validated in any way.
redirect' :: ByteString -> Int -> Snap ()
redirect' target status =
    finishWith
        $ setResponseStatus status reason
        $ setHeader "Location" target emptyResponse
    where
        reason = fromMaybe "Unknown" (IM.lookup status statusReasonMap)
{-# INLINE redirect' #-}


------------------------------------------------------------------------------
-- | Log an error message in the 'Snap' monad
logError :: ByteString -> Snap ()
logError s = Snap $ gets _snapLogError >>= (\l -> liftIO $ l s)
                                       >>  return (Just $ Right ())
{-# INLINE logError #-}


------------------------------------------------------------------------------
-- | Adds the output from the given enumerator to the 'Response'
-- stored in the 'Snap' monad state.
addToOutput :: (forall a . Enumerator a)   -- ^ output to add
            -> Snap ()
addToOutput enum = modifyResponse $ modifyResponseBody (>. enum)


------------------------------------------------------------------------------
-- | Adds the given strict 'ByteString' to the body of the 'Response' stored in
-- the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeBS :: ByteString -> Snap ()
writeBS s = addToOutput $ enumBS s


------------------------------------------------------------------------------
-- | Adds the given lazy 'L.ByteString' to the body of the 'Response' stored in
-- the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeLBS :: L.ByteString -> Snap ()
writeLBS s = addToOutput $ enumLBS s


------------------------------------------------------------------------------
-- | Adds the given strict 'T.Text' to the body of the 'Response' stored in the
-- 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeText :: T.Text -> Snap ()
writeText s = writeBS $ T.encodeUtf8 s


------------------------------------------------------------------------------
-- | Adds the given lazy 'LT.Text' to the body of the 'Response' stored in the
-- 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeLazyText :: LT.Text -> Snap ()
writeLazyText s = writeLBS $ LT.encodeUtf8 s


------------------------------------------------------------------------------
-- | Sets the output to be the contents of the specified file.
--
-- Calling 'sendFile' will overwrite any output queued to be sent in the
-- 'Response'. If the response body is not modified after the call to
-- 'sendFile', Snap will use the efficient @sendfile()@ system call on
-- platforms that support it.
--
-- If the response body is modified (using 'modifyResponseBody'), the file will
-- be read using @mmap()@.
sendFile :: FilePath -> Snap ()
sendFile f = modifyResponse $ \r -> r { rspBody = SendFile f }


------------------------------------------------------------------------------
-- | Runs a 'Snap' action with a locally-modified 'Request' state
-- object. The 'Request' object in the Snap monad state after the call
-- to localRequest will be unchanged.
localRequest :: (Request -> Request) -> Snap a -> Snap a
localRequest f m = do
    req <- getRequest

    runAct req <|> (putRequest req >> pass)

  where
    runAct req = do
        modifyRequest f
        result <- m
        putRequest req
        return result
{-# INLINE localRequest #-}


------------------------------------------------------------------------------
-- | Fetches the 'Request' from state and hands it to the given action.
withRequest :: (Request -> Snap a) -> Snap a
withRequest = (getRequest >>=)
{-# INLINE withRequest #-}


------------------------------------------------------------------------------
-- | Fetches the 'Response' from state and hands it to the given action.
withResponse :: (Response -> Snap a) -> Snap a
withResponse = (getResponse >>=)
{-# INLINE withResponse #-}


------------------------------------------------------------------------------
-- | Modifies the 'Request' in the state to set the 'rqRemoteAddr'
-- field to the value in the X-Forwarded-For header. If the header is
-- not present, this action has no effect.
--
-- This action should be used only when working behind a reverse http
-- proxy that sets the X-Forwarded-For header. This is the only way to
-- ensure the value in the X-Forwarded-For header can be trusted.
--
-- This is provided as a filter so actions that require the remote
-- address can get it in a uniform manner. It has specifically limited
-- functionality to ensure that its transformation can be trusted,
-- when used correctly.
ipHeaderFilter :: Snap ()
ipHeaderFilter = ipHeaderFilter' "x-forwarded-for"


------------------------------------------------------------------------------
-- | Modifies the 'Request' in the state to set the 'rqRemoteAddr'
-- field to the value from the header specified.  If the header
-- specified is not present, this action has no effect.
--
-- This action should be used only when working behind a reverse http
-- proxy that sets the header being looked at. This is the only way to
-- ensure the value in the header can be trusted.
--
-- This is provided as a filter so actions that require the remote
-- address can get it in a uniform manner. It has specifically limited
-- functionality to ensure that its transformation can be trusted,
-- when used correctly.
ipHeaderFilter' :: CIB.CIByteString -> Snap ()
ipHeaderFilter' header = do
    headerContents <- getHeader header <$> getRequest

    let whitespace = " \t\r\n"
        ipChrs = ".0123456789"
        trim f s = f (`elem` s)

        clean = trim S.takeWhile ipChrs . trim S.dropWhile whitespace
        setIP ip = modifyRequest $ \rq -> rq { rqRemoteAddr = clean ip }
    maybe (return ()) setIP headerContents


------------------------------------------------------------------------------
-- | This exception is thrown if the handler you supply to 'runSnap' fails.
data NoHandlerException = NoHandlerException
   deriving (Eq, Typeable)


------------------------------------------------------------------------------
instance Show NoHandlerException where
    show NoHandlerException = "No handler for request"


------------------------------------------------------------------------------
instance Exception NoHandlerException


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action in the 'Iteratee IO' monad.
runSnap :: Snap a
        -> (ByteString -> IO ())
        -> Request
        -> Iteratee IO (Request,Response)
runSnap (Snap m) logerr req = do
    (r, ss') <- runStateT m ss

    e <- maybe (return $ Left fourohfour)
               return
               r

    -- is this a case of early termination?
    let resp = case e of
                 Left x  -> x
                 Right _ -> _snapResponse ss'

    return (_snapRequest ss', resp)

  where
    fourohfour = setContentLength 3 $
                 setResponseStatus 404 "Not Found" $
                 modifyResponseBody (>. enumBS "404") $
                 emptyResponse

    dresp = emptyResponse { rspHttpVersion = rqVersion req }

    ss = SnapState req dresp logerr
{-# INLINE runSnap #-}


------------------------------------------------------------------------------
evalSnap :: Snap a
         -> (ByteString -> IO ())
         -> Request
         -> Iteratee IO a
evalSnap (Snap m) logerr req = do
    (r, _) <- runStateT m ss

    e <- maybe (liftIO $ throwIO NoHandlerException)
               return
               r

    -- is this a case of early termination?
    case e of
      Left _  -> liftIO $ throwIO $ ErrorCall "no value"
      Right x -> return x
  where
    dresp = emptyResponse { rspHttpVersion = rqVersion req }
    ss = SnapState req dresp logerr
{-# INLINE evalSnap #-}



------------------------------------------------------------------------------
-- | See 'rqParam'. Looks up a value for the given named parameter in the
-- 'Request'. If more than one value was entered for the given parameter name,
-- 'getParam' gloms the values together with:
--
-- @    'S.intercalate' \" \"@
--
getParam :: ByteString          -- ^ parameter name to look up
         -> Snap (Maybe ByteString)
getParam k = do
    rq <- getRequest
    return $ liftM (S.intercalate " ") $ rqParam k rq


statusReasonMap :: IM.IntMap ByteString
statusReasonMap = IM.fromList [
        (100, "Continue"),
        (101, "Switching Protocols"),
        (200, "OK"),
        (201, "Created"),
        (202, "Accepted"),
        (203, "Non-Authoritative Information"),
        (204, "No Content"),
        (205, "Reset Content"),
        (206, "Partial Content"),
        (300, "Multiple Choices"),
        (301, "Moved Permanently"),
        (302, "Found"),
        (303, "See Other"),
        (304, "Not Modified"),
        (305, "Use Proxy"),
        (307, "Temporary Redirect"),
        (400, "Bad Request"),
        (401, "Unauthorized"),
        (402, "Payment Required"),
        (403, "Forbidden"),
        (404, "Not Found"),
        (405, "Method Not Allowed"),
        (406, "Not Acceptable"),
        (407, "Proxy Authentication Required"),
        (408, "Request Time-out"),
        (409, "Conflict"),
        (410, "Gone"),
        (411, "Length Required"),
        (412, "Precondition Failed"),
        (413, "Request Entity Too Large"),
        (414, "Request-URI Too Large"),
        (415, "Unsupported Media Type"),
        (416, "Requested range not satisfiable"),
        (417, "Expectation Failed"),
        (500, "Internal Server Error"),
        (501, "Not Implemented"),
        (502, "Bad Gateway"),
        (503, "Service Unavailable"),
        (504, "Gateway Time-out"),
        (505, "HTTP Version not supported")
    ]
