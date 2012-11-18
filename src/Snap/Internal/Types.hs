{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Snap.Internal.Types where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Applicative
import           Control.Exception (SomeException, throwIO, ErrorCall(..))
import           Control.Monad
import           Control.Monad.CatchIO
import qualified Control.Monad.Error.Class as EC
import           Control.Monad.State
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.CaseInsensitive (CI)
import           Data.Int
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Typeable
#if MIN_VERSION_base(4,6,0)
import           Prelude hiding (take)
#else
import           Prelude hiding (catch, take)
#endif
import           System.PosixCompat.Files hiding (setFileSize)
import           System.Posix.Types (FileOffset)
------------------------------------------------------------------------------
import           Snap.Internal.Exceptions
import           Snap.Internal.Http.Types
import           Snap.Internal.Iteratee.Debug
import           Snap.Iteratee hiding (map)
import qualified Snap.Types.Headers as H
import           Snap.Util.Readable
------------------------------------------------------------------------------


                             --------------------
                             -- The Snap Monad --
                             --------------------
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
   'addToOutput') for queueing output to be written to the 'Response':

   > a :: (forall a . Enumerator a) -> Snap ()
   > a someEnumerator = do
   >     writeBS "I'm a strict bytestring"
   >     writeLBS "I'm a lazy bytestring"
   >     writeText "I'm strict text"
   >     addToOutput someEnumerator

5. early termination: if you call 'finishWith':

   > a :: Snap ()
   > a = do
   >   modifyResponse $ setResponseStatus 500 "Internal Server Error"
   >   writeBS "500 error"
   >   r <- getResponse
   >   finishWith r

   then any subsequent processing will be skipped and supplied 'Response'
   value will be returned from 'runSnap' as-is.

6. access to the 'IO' monad through a 'MonadIO' instance:

   > a :: Snap ()
   > a = liftIO fireTheMissiles

7. the ability to set or extend a timeout which will kill the handler thread
   after @N@ seconds of inactivity (the default is 20 seconds):

   > a :: Snap ()
   > a = setTimeout 30

8. throw and catch exceptions using a 'MonadCatchIO' instance:

   > foo :: Snap ()
   > foo = bar `catch` \(e::SomeException) -> baz
   >   where
   >     bar = throw FooException

9. log a message to the error log:

   > foo :: Snap ()
   > foo = logError "grumble."

You may notice that most of the type signatures in this module contain a
@(MonadSnap m) => ...@ typeclass constraint. 'MonadSnap' is a typeclass which,
in essence, says \"you can get back to the 'Snap' monad from here\". Using
'MonadSnap' you can extend the 'Snap' monad with additional functionality and
still have access to most of the 'Snap' functions without writing 'lift'
everywhere. Instances are already provided for most of the common monad
transformers ('ReaderT', 'WriterT', 'StateT', etc.).

-}

------------------------------------------------------------------------------
-- | 'MonadSnap' is a type class, analogous to 'MonadIO' for 'IO', that makes
-- it easy to wrap 'Snap' inside monad transformers.
class (Monad m, MonadIO m, MonadCatchIO m, MonadPlus m, Functor m,
       Applicative m, Alternative m) => MonadSnap m where
    liftSnap :: Snap a -> m a


------------------------------------------------------------------------------
data SnapResult a = SnapValue a
                  | PassOnProcessing String
                  | EarlyTermination Response

------------------------------------------------------------------------------
newtype Snap a = Snap {
      unSnap :: StateT SnapState (Iteratee ByteString IO) (SnapResult a)
    }


------------------------------------------------------------------------------
data SnapState = SnapState
    { _snapRequest       :: Request
    , _snapResponse      :: Response
    , _snapLogError      :: ByteString -> IO ()
    , _snapModifyTimeout :: (Int -> Int) -> IO ()
    }


------------------------------------------------------------------------------
instance Monad Snap where
    (>>=)  = snapBind
    return = snapReturn
    fail   = snapFail


------------------------------------------------------------------------------
snapBind :: Snap a -> (a -> Snap b) -> Snap b
snapBind (Snap m) f = Snap $ do
    res <- m

    case res of
      SnapValue a        -> unSnap $! f a
      PassOnProcessing r -> return $! PassOnProcessing r
      EarlyTermination r -> return $! EarlyTermination r
{-# INLINE snapBind #-}


snapReturn :: a -> Snap a
snapReturn = Snap . return . SnapValue
{-# INLINE snapReturn #-}


snapFail :: String -> Snap a
snapFail !m = Snap $! return $! PassOnProcessing m
{-# INLINE snapFail #-}


------------------------------------------------------------------------------
instance MonadIO Snap where
    liftIO m = Snap $! liftM SnapValue $! liftIO m


------------------------------------------------------------------------------
instance MonadCatchIO Snap where
    catch (Snap m) handler = Snap $! m `catch` h
      where
        h e = do
            rethrowIfUncatchable $ fromException e
            maybe (throw e)
                  (\e' -> let (Snap z) = handler e' in z)
                  (fromException e)

    block (Snap m) = Snap $ block m
    unblock (Snap m) = Snap $ unblock m


------------------------------------------------------------------------------
rethrowIfUncatchable :: (MonadCatchIO m) =>
                        Maybe UncatchableException ->
                        m ()
rethrowIfUncatchable Nothing  = return ()
rethrowIfUncatchable (Just e) = throw e


------------------------------------------------------------------------------
instance MonadPlus Snap where
    mzero = Snap $! return $! PassOnProcessing ""

    a `mplus` b =
        Snap $! do
            r <- unSnap a
            -- redundant just in case ordering by frequency helps here.
            case r of
              SnapValue _        -> return r
              PassOnProcessing _ -> unSnap b
              _                  -> return r


------------------------------------------------------------------------------
instance (EC.MonadError String) Snap where
    throwError = fail
    catchError act hndl = Snap $ do
        r <- unSnap act
        -- redundant just in case ordering by frequency helps here.
        case r of
          SnapValue _        -> return r
          PassOnProcessing m -> unSnap $ hndl m
          _                  -> return r


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
instance MonadSnap Snap where
    liftSnap = id



------------------------------------------------------------------------------
-- | The Typeable instance is here so Snap can be dynamically executed with
-- Hint.
snapTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
snapTyCon = mkTyCon3 "snap-core" "Snap.Core" "Snap"
#else
snapTyCon = mkTyCon "Snap.Core.Snap"
#endif
{-# NOINLINE snapTyCon #-}

instance Typeable1 Snap where
    typeOf1 _ = mkTyConApp snapTyCon []


------------------------------------------------------------------------------
liftIter :: MonadSnap m => Iteratee ByteString IO a -> m a
liftIter i = liftSnap $ Snap (lift i >>= return . SnapValue)


------------------------------------------------------------------------------
-- | Sends the request body through an iteratee (data consumer) and
-- returns the result.
--
-- If the iteratee you pass in here throws an exception, Snap will attempt to
-- clear the rest of the unread request body before rethrowing the exception.
-- If your iteratee used 'terminateConnection', however, Snap will give up and
-- immediately close the socket.
runRequestBody :: MonadSnap m => Iteratee ByteString IO a -> m a
runRequestBody iter = do
    bumpTimeout <- liftM ($ max 5) getTimeoutModifier
    req         <- getRequest
    senum       <- liftIO $ readIORef $ rqBody req
    let (SomeEnumerator enum) = senum

    -- make sure the iteratee consumes all of the output
    let iter' = handle bumpTimeout req
                       (iter >>= \a -> skipToEnd bumpTimeout >> return a)

    -- run the iteratee
    step   <- liftIO $ runIteratee iter'
    result <- liftIter $ enum step

    -- stuff a new dummy enumerator into the request, so you can only try to
    -- read the request body from the socket once
    resetEnum req
    return result

  where
    resetEnum req = liftIO $
                    writeIORef (rqBody req) $
                    SomeEnumerator $ joinI . take 0

    skipToEnd bump = killIfTooSlow bump 500 5 skipToEof `catchError` \e ->
                     throwError $ ConnectionTerminatedException e

    handle bump req =
        (`catches` [
          Handler $ \(e :: ConnectionTerminatedException) -> do
              let en = SomeEnumerator $ const $ throwError e
              liftIO $ writeIORef (rqBody req) en
              throwError e
         , Handler $ \(e :: SomeException) -> do
              resetEnum req
              skipToEnd bump
              throwError e
         ])


------------------------------------------------------------------------------
-- | Returns the request body as a lazy bytestring.
--
-- This function is deprecated as of 0.6; it places no limits on the size of
-- the request being read, and as such, if used, can result in a
-- denial-of-service attack on your server. Please use 'readRequestBody'
-- instead.
getRequestBody :: MonadSnap m => m L.ByteString
getRequestBody = liftM L.fromChunks $ runRequestBody consume
{-# INLINE getRequestBody #-}
{-# DEPRECATED getRequestBody
    "As of 0.6, please use 'readRequestBody' instead" #-}


------------------------------------------------------------------------------
-- | Returns the request body as a lazy bytestring. /New in 0.6./
readRequestBody :: MonadSnap m =>
                   Int64  -- ^ size of the largest request body we're willing
                          -- to accept. If a request body longer than this is
                          -- received, a 'TooManyBytesReadException' is
                          -- thrown. See 'takeNoMoreThan'.
                -> m L.ByteString
readRequestBody sz = liftM L.fromChunks $ runRequestBody $
                     joinI $ takeNoMoreThan sz $$ consume


------------------------------------------------------------------------------
-- | Normally Snap is careful to ensure that the request body is fully
-- consumed after your web handler runs, but before the 'Response' enumerator
-- is streamed out the socket. If you want to transform the request body into
-- some output in O(1) space, you should use this function.
--
-- Note that upon calling this function, response processing finishes early as
-- if you called 'finishWith'. Make sure you set any content types, headers,
-- cookies, etc. before you call this function.
--
transformRequestBody :: (forall a . Enumerator Builder IO a)
                         -- ^ the output 'Iteratee' is passed to this
                         -- 'Enumerator', and then the resulting 'Iteratee' is
                         -- fed the request body stream. Your 'Enumerator' is
                         -- responsible for transforming the input.
                     -> Snap ()
transformRequestBody trans = do
    req <- getRequest
    let ioref = rqBody req
    senum <- liftIO $ readIORef ioref
    let (SomeEnumerator enum') = senum
    let enum = mapEnum toByteString fromByteString enum'
    liftIO $ writeIORef ioref (SomeEnumerator enumEOF)

    origRsp <- getResponse
    let rsp = setResponseBody
                (\writeEnd -> do
                     let i = iterateeDebugWrapperWith showBuilder
                                                      "transformRequestBody"
                                                      $ trans writeEnd
                     st <- liftIO $ runIteratee i

                     enum st)
                $ origRsp { rspTransformingRqBody = True }
    finishWith rsp


------------------------------------------------------------------------------
-- | Short-circuits a 'Snap' monad action early, storing the given
-- 'Response' value in its state.
finishWith :: MonadSnap m => Response -> m a
finishWith = liftSnap . Snap . return . EarlyTermination
{-# INLINE finishWith #-}


------------------------------------------------------------------------------
-- | Capture the flow of control in case a handler calls 'finishWith'.
--
-- /WARNING/: in the event of a call to 'transformRequestBody' it is possible
-- to violate HTTP protocol safety when using this function. If you call
-- 'catchFinishWith' it is suggested that you do not modify the body of the
-- 'Response' which was passed to the 'finishWith' call.
catchFinishWith :: Snap a -> Snap (Either Response a)
catchFinishWith (Snap m) = Snap $ do
    r <- m
    case r of
      SnapValue a           -> return $! SnapValue $! Right a
      PassOnProcessing e    -> return $! PassOnProcessing e
      EarlyTermination resp -> return $! SnapValue $! Left resp
{-# INLINE catchFinishWith #-}


------------------------------------------------------------------------------
-- | Fails out of a 'Snap' monad action.  This is used to indicate
-- that you choose not to handle the given request within the given
-- handler.
pass :: MonadSnap m => m a
pass = empty


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only if the request's HTTP method matches
-- the given method.
method :: MonadSnap m => Method -> m a -> m a
method m action = do
    req <- getRequest
    unless (rqMethod req == m) pass
    action
{-# INLINE method #-}


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only if the request's HTTP method matches
-- one of the given methods.
methods :: MonadSnap m => [Method] -> m a -> m a
methods ms action = do
    req <- getRequest
    unless (rqMethod req `elem` ms) pass
    action
{-# INLINE methods #-}


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
pathWith :: MonadSnap m
         => (ByteString -> ByteString -> Bool)
         -> ByteString
         -> m a
         -> m a
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
dir :: MonadSnap m
    => ByteString  -- ^ path component to match
    -> m a         -- ^ handler to run
    -> m a
dir = pathWith f
  where
    f dr pinfo = dr == x
      where
        (x,_) = S.break (=='/') pinfo
{-# INLINE dir #-}


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only for requests where 'rqPathInfo' is
-- exactly equal to the given string. If the path matches, locally sets
-- 'rqContextPath' to the old value of 'rqPathInfo', sets 'rqPathInfo'=\"\",
-- and runs the given handler.
path :: MonadSnap m
     => ByteString  -- ^ path to match against
     -> m a         -- ^ handler to run
     -> m a
path = pathWith (==)
{-# INLINE path #-}


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only when the first path component is
-- successfully parsed as the argument to the supplied handler function.
pathArg :: (Readable a, MonadSnap m)
        => (a -> m b)
        -> m b
pathArg f = do
    req <- getRequest
    let (p,_) = S.break (=='/') (rqPathInfo req)
    a <- fromBS p
    localRequest (updateContextPath $ S.length p) (f a)


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only when 'rqPathInfo' is empty.
ifTop :: MonadSnap m => m a -> m a
ifTop = path ""
{-# INLINE ifTop #-}


------------------------------------------------------------------------------
-- | Local Snap version of 'get'.
sget :: Snap SnapState
sget = Snap $ liftM SnapValue get
{-# INLINE sget #-}


------------------------------------------------------------------------------
-- | Local Snap monad version of 'modify'.
smodify :: (SnapState -> SnapState) -> Snap ()
smodify f = Snap $ modify f >> return (SnapValue ())
{-# INLINE smodify #-}


------------------------------------------------------------------------------
-- | Grabs the 'Request' object out of the 'Snap' monad.
getRequest :: MonadSnap m => m Request
getRequest = liftSnap $ liftM _snapRequest sget
{-# INLINE getRequest #-}


------------------------------------------------------------------------------
-- | Grabs something out of the 'Request' object, using the given projection
-- function. See 'gets'.
getsRequest :: MonadSnap m => (Request -> a) -> m a
getsRequest f = liftSnap $ liftM (f . _snapRequest) sget
{-# INLINE getsRequest #-}


------------------------------------------------------------------------------
-- | Grabs the 'Response' object out of the 'Snap' monad.
getResponse :: MonadSnap m => m Response
getResponse = liftSnap $ liftM _snapResponse sget
{-# INLINE getResponse #-}


------------------------------------------------------------------------------
-- | Grabs something out of the 'Response' object, using the given projection
-- function. See 'gets'.
getsResponse :: MonadSnap m => (Response -> a) -> m a
getsResponse f = liftSnap $ liftM (f . _snapResponse) sget
{-# INLINE getsResponse #-}


------------------------------------------------------------------------------
-- | Puts a new 'Response' object into the 'Snap' monad.
putResponse :: MonadSnap m => Response -> m ()
putResponse r = liftSnap $ smodify $ \ss -> ss { _snapResponse = r }
{-# INLINE putResponse #-}


------------------------------------------------------------------------------
-- | Puts a new 'Request' object into the 'Snap' monad.
putRequest :: MonadSnap m => Request -> m ()
putRequest r = liftSnap $ smodify $ \ss -> ss { _snapRequest = r }
{-# INLINE putRequest #-}


------------------------------------------------------------------------------
-- | Modifies the 'Request' object stored in a 'Snap' monad.
modifyRequest :: MonadSnap m => (Request -> Request) -> m ()
modifyRequest f = liftSnap $
    smodify $ \ss -> ss { _snapRequest = f $ _snapRequest ss }
{-# INLINE modifyRequest #-}


------------------------------------------------------------------------------
-- | Modifes the 'Response' object stored in a 'Snap' monad.
modifyResponse :: MonadSnap m => (Response -> Response) -> m ()
modifyResponse f = liftSnap $
     smodify $ \ss -> ss { _snapResponse = f $ _snapResponse ss }
{-# INLINE modifyResponse #-}


------------------------------------------------------------------------------
-- | Performs a redirect by setting the @Location@ header to the given target
-- URL/path and the status code to 302 in the 'Response' object stored in a
-- 'Snap' monad. Note that the target URL is not validated in any way.
-- Consider using 'redirect\'' instead, which allows you to choose the correct
-- status code.
redirect :: MonadSnap m => ByteString -> m a
redirect target = redirect' target 302
{-# INLINE redirect #-}


------------------------------------------------------------------------------
-- | Performs a redirect by setting the @Location@ header to the given target
-- URL/path and the status code (should be one of 301, 302, 303 or 307) in the
-- 'Response' object stored in a 'Snap' monad. Note that the target URL is not
-- validated in any way.
redirect' :: MonadSnap m => ByteString -> Int -> m a
redirect' target status = do
    r <- getResponse

    finishWith
        $ setResponseCode status
        $ setContentLength 0
        $ modifyResponseBody (const $ enumBuilder mempty)
        $ setHeader "Location" target r

{-# INLINE redirect' #-}


------------------------------------------------------------------------------
-- | Log an error message in the 'Snap' monad
logError :: MonadSnap m => ByteString -> m ()
logError s = liftSnap $ Snap $ gets _snapLogError >>= (\l -> liftIO $ l s)
                                       >>  return (SnapValue ())
{-# INLINE logError #-}


------------------------------------------------------------------------------
-- | Adds the output from the given enumerator to the 'Response'
-- stored in the 'Snap' monad state.
addToOutput :: MonadSnap m
            => (forall a . Enumerator Builder IO a)   -- ^ output to add
            -> m ()
addToOutput enum = modifyResponse $ modifyResponseBody (>==> enum)


------------------------------------------------------------------------------
-- | Adds the given 'Builder' to the body of the 'Response' stored in the
-- | 'Snap' monad state.
writeBuilder :: MonadSnap m => Builder -> m ()
writeBuilder b = addToOutput $ enumBuilder b
{-# INLINE writeBuilder #-}


------------------------------------------------------------------------------
-- | Adds the given strict 'ByteString' to the body of the 'Response' stored
-- in the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeBS :: MonadSnap m => ByteString -> m ()
writeBS s = writeBuilder $ fromByteString s


------------------------------------------------------------------------------
-- | Adds the given lazy 'L.ByteString' to the body of the 'Response' stored
-- in the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeLBS :: MonadSnap m => L.ByteString -> m ()
writeLBS s = writeBuilder $ fromLazyByteString s


------------------------------------------------------------------------------
-- | Adds the given strict 'T.Text' to the body of the 'Response' stored in
-- the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeText :: MonadSnap m => T.Text -> m ()
writeText s = writeBuilder $ fromText s


------------------------------------------------------------------------------
-- | Adds the given lazy 'LT.Text' to the body of the 'Response' stored in the
-- 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeLazyText :: MonadSnap m => LT.Text -> m ()
writeLazyText s = writeBuilder $ fromLazyText s


------------------------------------------------------------------------------
-- | Sets the output to be the contents of the specified file.
--
-- Calling 'sendFile' will overwrite any output queued to be sent in the
-- 'Response'. If the response body is not modified after the call to
-- 'sendFile', Snap will use the efficient @sendfile()@ system call on
-- platforms that support it.
--
-- If the response body is modified (using 'modifyResponseBody'), the file
-- will be read using @mmap()@.
sendFile :: (MonadSnap m) => FilePath -> m ()
sendFile f = modifyResponse $ \r -> r { rspBody = SendFile f Nothing }


------------------------------------------------------------------------------
-- | Sets the output to be the contents of the specified file, within the
-- given (start,end) range.
--
-- Calling 'sendFilePartial' will overwrite any output queued to be sent in
-- the 'Response'. If the response body is not modified after the call to
-- 'sendFilePartial', Snap will use the efficient @sendfile()@ system call on
-- platforms that support it.
--
-- If the response body is modified (using 'modifyResponseBody'), the file
-- will be read using @mmap()@.
sendFilePartial :: (MonadSnap m) => FilePath -> (Int64,Int64) -> m ()
sendFilePartial f rng = modifyResponse $ \r ->
                        r { rspBody = SendFile f (Just rng) }


------------------------------------------------------------------------------
-- | Runs a 'Snap' action with a locally-modified 'Request' state
-- object. The 'Request' object in the Snap monad state after the call
-- to localRequest will be unchanged.
localRequest :: MonadSnap m => (Request -> Request) -> m a -> m a
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
withRequest :: MonadSnap m => (Request -> m a) -> m a
withRequest = (getRequest >>=)
{-# INLINE withRequest #-}


------------------------------------------------------------------------------
-- | Fetches the 'Response' from state and hands it to the given action.
withResponse :: MonadSnap m => (Response -> m a) -> m a
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
ipHeaderFilter :: MonadSnap m => m ()
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
ipHeaderFilter' :: MonadSnap m => CI ByteString -> m ()
ipHeaderFilter' header = do
    headerContents <- getHeader header <$> getRequest

    let whitespace = " \t\r\n"
        ipChrs = ".0123456789"
        trim f s = f (`elem` s)

        clean = trim S.takeWhile ipChrs . trim S.dropWhile whitespace
        setIP ip = modifyRequest $ \rq -> rq { rqRemoteAddr = clean ip }
    maybe (return ()) setIP headerContents


------------------------------------------------------------------------------
-- | This function brackets a Snap action in resource acquisition and
-- release. This is provided because MonadCatchIO's 'bracket' function
-- doesn't work properly in the case of a short-circuit return from
-- the action being bracketed.
--
-- In order to prevent confusion regarding the effects of the
-- aquisition and release actions on the Snap state, this function
-- doesn't accept Snap actions for the acquire or release actions.
--
-- This function will run the release action in all cases where the
-- acquire action succeeded.  This includes the following behaviors
-- from the bracketed Snap action.
--
-- 1. Normal completion
--
-- 2. Short-circuit completion, either from calling 'fail' or 'finishWith'
--
-- 3. An exception being thrown.
bracketSnap :: IO a -> (a -> IO b) -> (a -> Snap c) -> Snap c
bracketSnap before after thing = block . Snap $ do
    a <- liftIO before
    let after' = liftIO $ after a
        (Snap thing') = thing a
    r <- unblock thing' `onException` after'
    _ <- after'
    return r


------------------------------------------------------------------------------
-- | This exception is thrown if the handler you supply to 'runSnap' fails.
data NoHandlerException = NoHandlerException String
   deriving (Eq, Typeable)


------------------------------------------------------------------------------
instance Show NoHandlerException where
    show (NoHandlerException e) = "No handler for request: failure was " ++ e


------------------------------------------------------------------------------
instance Exception NoHandlerException


------------------------------------------------------------------------------
-- | Terminate the HTTP session with the given exception.
terminateConnection :: (Exception e, MonadCatchIO m) => e -> m a
terminateConnection = throw . ConnectionTerminatedException . toException


------------------------------------------------------------------------------
-- | Terminate the HTTP session and hand control to some external handler,
-- escaping all further HTTP traffic.
--
-- The external handler takes two arguments: a function to modify the thread's
-- timeout, and a write end to the socket.
escapeHttp :: MonadCatchIO m =>
              EscapeHttpHandler
           -> m ()
escapeHttp = throw . EscapeHttpException


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action in the 'Iteratee IO' monad.
runSnap :: Snap a
        -> (ByteString -> IO ())
        -> ((Int -> Int) -> IO ())
        -> Request
        -> Iteratee ByteString IO (Request,Response)
runSnap (Snap m) logerr timeoutAction req = do
    (r, ss') <- runStateT m ss

    let resp = case r of
                 SnapValue _        -> _snapResponse ss'
                 PassOnProcessing _ -> fourohfour
                 EarlyTermination x -> x

    let req' = _snapRequest ss'

    resp' <- liftIO $ fixupResponse req' resp

    return (req', resp')

  where
    --------------------------------------------------------------------------
    fourohfour = do
        clearContentLength                  $
          setResponseStatus 404 "Not Found" $
          setResponseBody enum404           $
          emptyResponse

    --------------------------------------------------------------------------
    enum404 = enumBuilder $ mconcat $ map fromByteString html

    --------------------------------------------------------------------------
    html = [ S.concat [ "<!DOCTYPE html>\n"
                      , "<html>\n"
                      , "<head>\n"
                      , "<title>Not found</title>\n"
                      , "</head>\n"
                      , "<body>\n"
                      , "<code>No handler accepted \""
                      ]
           , rqURI req
           , "\"</code>\n</body></html>"
           ]

    --------------------------------------------------------------------------
    dresp = emptyResponse { rspHttpVersion = rqVersion req }

    --------------------------------------------------------------------------
    ss = SnapState req dresp logerr timeoutAction
{-# INLINE runSnap #-}



--------------------------------------------------------------------------
-- | Post-process a finalized HTTP response:
--
-- * fixup content-length header
-- * properly handle 204/304 responses
-- * if request was HEAD, remove response body
--
-- Note that we do NOT deal with transfer-encoding: chunked or "connection:
-- close" here.
fixupResponse :: Request -> Response -> IO Response
fixupResponse req rsp = {-# SCC "fixupResponse" #-} do
    let code = rspStatus rsp
    let rsp' = if code == 204 || code == 304
                 then handle304 rsp
                 else rsp

    rsp'' <- do
        z <- case rspBody rsp' of
               (Enum _)                  -> return rsp'
               (SendFile f Nothing)      -> setFileSize f rsp'
               (SendFile _ (Just (s,e))) -> return $!
                                            setContentLength (e-s) rsp'

        return $!
          case rspContentLength z of
            Nothing   -> deleteHeader "Content-Length" z
            (Just sz) -> setHeader "Content-Length"
                                   (toByteString $ fromShow sz)
                                   z

    -- HEAD requests cannot have bodies per RFC 2616 sec. 9.4
    if rqMethod req == HEAD
      then return $! deleteHeader "Transfer-Encoding" $
           rsp'' { rspBody = Enum $ enumBuilder mempty }
      else return $! rsp''

  where
    --------------------------------------------------------------------------
    setFileSize :: FilePath -> Response -> IO Response
    setFileSize fp r = {-# SCC "setFileSize" #-} do
        fs <- liftM fromIntegral $ getFileSize fp
        return $! r { rspContentLength = Just fs }

    ------------------------------------------------------------------------------
    getFileSize :: FilePath -> IO FileOffset
    getFileSize fp = liftM fileSize $ getFileStatus fp

    --------------------------------------------------------------------------
    handle304 :: Response -> Response
    handle304 r = setResponseBody (enumBuilder mempty) $
                  updateHeaders (H.delete "Transfer-Encoding") $
                  setContentLength 0 r
{-# INLINE fixupResponse #-}


------------------------------------------------------------------------------
evalSnap :: Snap a
         -> (ByteString -> IO ())
         -> ((Int -> Int) -> IO ())
         -> Request
         -> Iteratee ByteString IO a
evalSnap (Snap m) logerr timeoutAction req = do
    (r, _) <- runStateT m ss

    case r of
      SnapValue x        -> return x
      PassOnProcessing e -> liftIO $ throwIO $ NoHandlerException e
      EarlyTermination _ -> liftIO $ throwIO $ ErrorCall "no value"

  where
    dresp = emptyResponse { rspHttpVersion = rqVersion req }
    ss = SnapState req dresp logerr timeoutAction
{-# INLINE evalSnap #-}


------------------------------------------------------------------------------
getParamFrom :: MonadSnap m =>
                (ByteString -> Request -> Maybe [ByteString])
             -> ByteString
             -> m (Maybe ByteString)
getParamFrom f k = do
    rq <- getRequest
    return $! liftM (S.intercalate " ") $ f k rq
{-# INLINE getParamFrom #-}


------------------------------------------------------------------------------
-- | See 'rqParam'. Looks up a value for the given named parameter in the
-- 'Request'. If more than one value was entered for the given parameter name,
-- 'getParam' gloms the values together with:
--
-- @    'S.intercalate' \" \"@
--
getParam :: MonadSnap m
         => ByteString          -- ^ parameter name to look up
         -> m (Maybe ByteString)
getParam = getParamFrom rqParam
{-# INLINE getParam #-}


------------------------------------------------------------------------------
-- | See 'rqPostParam'. Looks up a value for the given named parameter in the
-- POST form parameters mapping in 'Request'. If more than one value was
-- entered for the given parameter name, 'getPostParam' gloms the values
-- together with:
--
-- @    'S.intercalate' \" \"@
--
getPostParam :: MonadSnap m
             => ByteString          -- ^ parameter name to look up
             -> m (Maybe ByteString)
getPostParam = getParamFrom rqPostParam
{-# INLINE getPostParam #-}


------------------------------------------------------------------------------
-- | See 'rqQueryParam'. Looks up a value for the given named parameter in the
-- query string parameters mapping in 'Request'. If more than one value was
-- entered for the given parameter name, 'getQueryParam' gloms the values
-- together with:
--
-- @    'S.intercalate' \" \"@
--
getQueryParam :: MonadSnap m
              => ByteString          -- ^ parameter name to look up
              -> m (Maybe ByteString)
getQueryParam = getParamFrom rqQueryParam
{-# INLINE getQueryParam #-}


------------------------------------------------------------------------------
-- | See 'rqParams'. Convenience function to return 'Params' from the
-- 'Request' inside of a 'MonadSnap' instance.
getParams :: MonadSnap m => m Params
getParams = getRequest >>= return . rqParams


------------------------------------------------------------------------------
-- | See 'rqParams'. Convenience function to return 'Params' from the
-- 'Request' inside of a 'MonadSnap' instance.
getPostParams :: MonadSnap m => m Params
getPostParams = getRequest >>= return . rqPostParams


------------------------------------------------------------------------------
-- | See 'rqParams'. Convenience function to return 'Params' from the
-- 'Request' inside of a 'MonadSnap' instance.
getQueryParams :: MonadSnap m => m Params
getQueryParams = getRequest >>= return . rqQueryParams


------------------------------------------------------------------------------
-- | Gets the HTTP 'Cookie' with the specified name.
getCookie :: MonadSnap m
          => ByteString
          -> m (Maybe Cookie)
getCookie name = withRequest $
    return . listToMaybe . filter (\c -> cookieName c == name) . rqCookies


------------------------------------------------------------------------------
-- | Gets the HTTP 'Cookie' with the specified name and decodes it.  If the
-- decoding fails, the handler calls pass.
readCookie :: (MonadSnap m, Readable a)
           => ByteString
           -> m a
readCookie name = maybe pass (fromBS . cookieValue) =<< getCookie name


------------------------------------------------------------------------------
-- | Expire the given 'Cookie' in client's browser.
expireCookie :: (MonadSnap m)
             => ByteString
             -- ^ Cookie name
             -> Maybe ByteString
             -- ^ Cookie domain
             -> m ()
expireCookie nm dm = do
  let old = UTCTime (ModifiedJulianDay 0) 0
  modifyResponse $ addResponseCookie
                 $ Cookie nm "" (Just old) Nothing dm False False


------------------------------------------------------------------------------
-- | Causes the handler thread to be killed @n@ seconds from now.
setTimeout :: MonadSnap m => Int -> m ()
setTimeout = modifyTimeout . const


------------------------------------------------------------------------------
-- | Causes the handler thread to be killed at least @n@ seconds from now.
extendTimeout :: MonadSnap m => Int -> m ()
extendTimeout = modifyTimeout . max


------------------------------------------------------------------------------
-- | Modifies the amount of time remaining before the request times out.
modifyTimeout :: MonadSnap m => (Int -> Int) -> m ()
modifyTimeout f = do
    m <- getTimeoutModifier
    liftIO $ m f


------------------------------------------------------------------------------
-- | Returns an 'IO' action which you can use to set the handling thread's
-- timeout value.
getTimeoutAction :: MonadSnap m => m (Int -> IO ())
getTimeoutAction = do
    modifier <- liftSnap $ liftM _snapModifyTimeout sget
    return $! modifier . const
{-# DEPRECATED getTimeoutAction
      "use getTimeoutModifier instead. Since 0.8." #-}


------------------------------------------------------------------------------
-- | Returns an 'IO' action which you can use to modify the timeout value.
getTimeoutModifier :: MonadSnap m => m ((Int -> Int) -> IO ())
getTimeoutModifier = liftSnap $ liftM _snapModifyTimeout sget
