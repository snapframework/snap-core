{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE StandaloneDeriving         #-}
#endif

module Snap.Internal.Core
  ( MonadSnap(..)
  , SnapResult(..)
  , EscapeHttpHandler
  , EscapeSnap(..)
  , Zero(..)
  , Snap(..)
  , SnapState(..)
  , runRequestBody
  , readRequestBody
  , transformRequestBody
  , finishWith
  , catchFinishWith
  , pass
  , method
  , methods
  , updateContextPath
  , pathWith
  , dir
  , path
  , pathArg
  , ifTop
  , sget
  , smodify
  , getRequest
  , getResponse
  , getsRequest
  , getsResponse
  , putRequest
  , putResponse
  , modifyRequest
  , modifyResponse
  , redirect
  , redirect'
  , logError
  , addToOutput
  , writeBuilder
  , writeBS
  , writeLBS
  , writeText
  , writeLazyText
  , sendFile
  , sendFilePartial
  , localRequest
  , withRequest
  , withResponse
  , ipHeaderFilter
  , ipHeaderFilter'
  , bracketSnap
  , NoHandlerException(..)
  , terminateConnection
  , escapeHttp
  , runSnap
  , fixupResponse
  , evalSnap
  , getParamFrom
  , getParam
  , getPostParam
  , getQueryParam
  , getParams
  , getPostParams
  , getQueryParams
  , getCookie
  , readCookie
  , expireCookie
  , setTimeout
  , extendTimeout
  , modifyTimeout
  , getTimeoutModifier
  , module Snap.Internal.Http.Types
  ) where

------------------------------------------------------------------------------
import           Control.Applicative                (Alternative ((<|>), empty), Applicative ((<*>), pure), (<$>))
import           Control.Exception.Lifted           (ErrorCall (..), Exception, Handler (..), SomeException (..), catch, catches, mask, onException, throwIO)
import           Control.Monad                      (Functor (..), Monad (..), MonadPlus (..), ap, liftM, unless, (=<<))
import qualified Control.Monad.Fail                 as Fail
import           Control.Monad.Base                 (MonadBase (..))
import           Control.Monad.IO.Class             (MonadIO (..))
import           Control.Monad.Trans.Control        (MonadBaseControl (..))
import           Control.Monad.Trans.State          (StateT (..))
import           Data.ByteString.Builder            (Builder, byteString, lazyByteString)
import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as S (break, concat, drop, dropWhile, intercalate, length, take, takeWhile)
import qualified Data.ByteString.Internal           as S (create)
import qualified Data.ByteString.Lazy.Char8         as L (ByteString, fromChunks)
import           Data.CaseInsensitive               (CI)
import           Data.Maybe                         (Maybe (..), listToMaybe, maybe)
import qualified Data.Text                          as T (Text)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import qualified Data.Text.Lazy                     as LT (Text)
import           Data.Time                          (Day (ModifiedJulianDay), UTCTime (UTCTime))
#if __GLASGOW_HASKELL__ < 708
import           Data.Typeable                      (TyCon, Typeable, Typeable1 (..), mkTyCon3, mkTyConApp)
#else
import           Data.Typeable                      (Typeable)
#endif
import           Data.Word                          (Word64, Word8)
import           Foreign.Ptr                        (Ptr, plusPtr)
import           Foreign.Storable                   (poke)
import           Prelude                            (Bool (..), Either (..), Eq (..), FilePath, IO, Int, Num (..), Ord (..), Show (..), String, const, divMod, elem, filter, fromIntegral, id, map, max, otherwise, quot, ($), ($!), (++), (.), (||))
import           System.IO.Streams                  (InputStream, OutputStream)
import qualified System.IO.Streams                  as Streams
import           System.Posix.Types                 (FileOffset)
import           System.PosixCompat.Files           (fileSize, getFileStatus)
#if !MIN_VERSION_bytestring(0,10,6)
import qualified Data.ByteString.Internal           as S (inlinePerformIO)
#else
import qualified Data.ByteString.Internal           as S (accursedUnutterablePerformIO)
#endif
------------------------------------------------------------------------------
import qualified Data.Readable                      as R
import           Snap.Internal.Http.Types           (Cookie (..), HasHeaders (..), HttpVersion, Method (..), Params, Request (..), Response (..), ResponseBody (..), StreamProc, addHeader, addResponseCookie, clearContentLength, deleteHeader, deleteResponseCookie, emptyResponse, formatHttpTime, formatLogTime, getHeader, getResponseCookie, getResponseCookies, listHeaders, modifyResponseBody, modifyResponseCookie, normalizeMethod, parseHttpTime, rqModifyParams, rqParam, rqPostParam, rqQueryParam, rqSetParam, rspBodyMap, rspBodyToEnum, setContentLength, setContentType, setHeader, setResponseBody, setResponseCode, setResponseStatus, statusReasonMap)
import           Snap.Internal.Parsing              (urlDecode)
import qualified Snap.Types.Headers                 as H
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | 'MonadSnap' is a type class, analogous to 'MonadIO' for 'IO', that makes it
-- easy to wrap 'Snap' inside monad transformers.
class (Monad m, MonadIO m, MonadBaseControl IO m, MonadPlus m, Functor m,
       Applicative m, Alternative m) => MonadSnap m where
  -- | Lift a computation from the 'Snap' monad.
  liftSnap :: Snap a -> m a


------------------------------------------------------------------------------
data SnapResult a = SnapValue a
                  | Zero Zero


------------------------------------------------------------------------------
-- | Type of external handler passed to 'escapeHttp'.
type EscapeHttpHandler =  ((Int -> Int) -> IO ())    -- ^ timeout modifier
                       -> InputStream ByteString     -- ^ socket read end
                       -> OutputStream Builder       -- ^ socket write end
                       -> IO ()


------------------------------------------------------------------------------
-- | Used internally to implement 'escapeHttp'.
data EscapeSnap = TerminateConnection SomeException
                | EscapeHttp EscapeHttpHandler
  deriving (Typeable)

instance Exception EscapeSnap

instance Show EscapeSnap where
    show (TerminateConnection e) = "<terminated: " ++ show e ++ ">"
    show (EscapeHttp _)          = "<escape http>"


------------------------------------------------------------------------------
data Zero = PassOnProcessing
          | EarlyTermination Response
          | EscapeSnap EscapeSnap

                             --------------------
                             -- The Snap Monad --
                             --------------------
{-|
'Snap' is the 'Monad' that user web handlers run in. 'Snap' gives you:

1. Stateful access to fetch or modify an HTTP 'Request'.

    @
    printRqContextPath :: Snap ()
    printRqContextPath = 'writeBS' . 'rqContextPath' =<< 'getRequest'
    @

2. Stateful access to fetch or modify an HTTP 'Response'.

    @
    printRspStatusReason :: Snap ()
    printRspStatusReason = 'writeBS' . 'rspStatusReason' =<< 'getResponse'
    @

3. Failure \/ 'Alternative' \/ 'MonadPlus' semantics: a 'Snap' handler can
choose not to handle a given request, using 'empty' or its synonym 'pass', and
you can try alternative handlers with the '<|>' operator:

    @
    a :: Snap String
    a = 'pass'

    b :: Snap String
    b = return \"foo\"

    c :: Snap String
    c = a '<|>' b             -- try running a, if it fails then try b
    @

4. Convenience functions ('writeBS', 'writeLBS', 'writeText', 'writeLazyText',
'addToOutput') for queueing output to be written to the 'Response', or for
streaming to the response using
<http://hackage.haskell.org/package/io-streams io-streams>:

    @
    example :: ('OutputStream' 'Builder' -> IO ('OutputStream' 'Builder')) -> Snap ()
    example streamProc = do
        'writeBS'   \"I\'m a strict bytestring\"
        'writeLBS'  \"I\'m a lazy bytestring\"
        'writeText' \"I\'m strict text\"
        'addToOutput' streamProc
    @

5. Early termination: if you call 'finishWith':

    @
    a :: Snap ()
    a = do
        'modifyResponse' $ 'setResponseStatus' 500 \"Internal Server Error\"
        'writeBS' \"500 error\"
        r <- 'getResponse'
        'finishWith' r
    @

    then any subsequent processing will be skipped and the supplied 'Response'
    value will be returned from 'runSnap' as-is.

6. Access to the 'IO' monad through a 'MonadIO' instance:

    @
    a :: Snap ()
    a = 'liftIO' fireTheMissiles
    @

7. The ability to set or extend a timeout which will kill the handler thread
after @N@ seconds of inactivity (the default is 20 seconds):

    @
    a :: Snap ()
    a = 'setTimeout' 30
    @

8. Throw and catch exceptions using a 'MonadBaseControl' instance:

    @
    import "Control.Exception.Lifted" ('SomeException', 'throwIO', 'catch')

    foo :: Snap ()
    foo = bar \`catch\` \(e::'SomeException') -> baz
      where
        bar = 'throwIO' FooException
    @

9. Log a message to the error log:

    @
    foo :: Snap ()
    foo = 'logError' \"grumble.\"
    @
-}

-- Haddock comment broken in two to work around https://github.com/haskell/haddock/issues/313

-- | You may notice that most of the type signatures in this module contain a
-- @('MonadSnap' m) => ...@ typeclass constraint. 'MonadSnap' is a typeclass
-- which, in essence, says \"you can get back to the 'Snap' monad from
-- here\". Using 'MonadSnap' you can extend the 'Snap' monad with additional
-- functionality and still have access to most of the 'Snap' functions without
-- writing 'Control.Monad.Trans.Class.lift' everywhere. Instances are already
-- provided for most of the common monad transformers
-- ('Control.Monad.Trans.Reader.ReaderT', 'Control.Monad.Trans.Writer.WriterT',
-- 'Control.Monad.Trans.State.StateT', etc.).
newtype Snap a = Snap {
      unSnap :: forall r . (a -> SnapState -> IO r)   -- success continuation
             -> (Zero -> SnapState -> IO r)           -- mzero continuation
             -> SnapState                             -- state for the monad
             -> IO r
    }


------------------------------------------------------------------------------
data SnapState = SnapState
    { _snapRequest       :: Request
    , _snapResponse      :: Response
    , _snapLogError      :: ByteString -> IO ()
    , _snapModifyTimeout :: (Int -> Int) -> IO ()
    }

-- TODO(greg): error log action and timeout modifier are never modified.
-- Splitting them out into their own datatype would save 16 bytes of allocation
-- every time you modify the request or response, but would gobble a register.
-- Benchmark it both ways.

------------------------------------------------------------------------------
instance Monad Snap where
    (>>=)  = snapBind
#if !MIN_VERSION_base(4,8,0)
    -- pre-AMP
    return = pure
    {-# INLINE return #-}
#endif
#if !MIN_VERSION_base(4,13,0)
    fail   = Fail.fail
#endif

instance Fail.MonadFail Snap where
    fail   = snapFail

------------------------------------------------------------------------------
snapBind :: Snap a -> (a -> Snap b) -> Snap b
snapBind m f = Snap $ \sk fk st -> unSnap m (\a st' -> unSnap (f a) sk fk st') fk st
{-# INLINE snapBind #-}

snapFail :: String -> Snap a
snapFail !_ = Snap $ \_ fk st -> fk PassOnProcessing st
{-# INLINE snapFail #-}


------------------------------------------------------------------------------
instance MonadIO Snap where
    liftIO m = Snap $ \sk _ st -> do x <- m
                                     sk x st


------------------------------------------------------------------------------
instance (MonadBase IO) Snap where
    liftBase = liftIO


------------------------------------------------------------------------------
newtype StSnap a = StSnap {
      unStSnap :: StM (StateT SnapState IO) (SnapResult a)
    }

instance (MonadBaseControl IO) Snap where
    type StM Snap a = StSnap a

    liftBaseWith f = stateTToSnap $ liftM SnapValue $
                     liftBaseWith $ \g' -> f $ \m ->
                     liftM StSnap $ g' $ snapToStateT m
    {-# INLINE liftBaseWith #-}

    restoreM = stateTToSnap . restoreM . unStSnap
    {-# INLINE restoreM #-}

------------------------------------------------------------------------------
snapToStateT :: Snap a -> StateT SnapState IO (SnapResult a)
snapToStateT m = StateT $ \st -> do
    unSnap m (\a st' -> return (SnapValue a, st'))
             (\z st' -> return (Zero z, st')) st
{-# INLINE snapToStateT #-}


------------------------------------------------------------------------------
{-# INLINE stateTToSnap #-}
stateTToSnap :: StateT SnapState IO (SnapResult a) -> Snap a
stateTToSnap m = Snap $ \sk fk st -> do
    (a, st') <- runStateT m st
    case a of
      SnapValue x -> sk x st'
      Zero z      -> fk z st'


------------------------------------------------------------------------------
instance MonadPlus Snap where
    mzero = Snap $ \_ fk st -> fk PassOnProcessing st

    a `mplus` b =
        Snap $ \sk fk st ->
            let fk' z st' = case z of
                              PassOnProcessing -> unSnap b sk fk st'
                              _                -> fk z st'
            in unSnap a sk fk' st


------------------------------------------------------------------------------
instance Functor Snap where
    fmap f m = Snap $ \sk fk st -> unSnap m (sk . f) fk st

------------------------------------------------------------------------------
instance Applicative Snap where
    pure x  = Snap $ \sk _ st -> sk x st
    (<*>)   = ap


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
#if __GLASGOW_HASKELL__ < 708
snapTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
snapTyCon = mkTyCon3 "snap-core" "Snap.Core" "Snap"
#else
snapTyCon = mkTyCon "Snap.Core.Snap"
#endif
{-# NOINLINE snapTyCon #-}

instance Typeable1 Snap where
    typeOf1 _ = mkTyConApp snapTyCon []
#else
deriving instance Typeable Snap
#endif

------------------------------------------------------------------------------
-- | Pass the request body stream to a consuming procedure, returning the
-- result.
--
-- If the consuming procedure you pass in here throws an exception, Snap will
-- attempt to clear the rest of the unread request body (using
-- 'System.IO.Streams.Combinators.skipToEof') before rethrowing the
-- exception. If you used 'terminateConnection', however, Snap will give up and
-- immediately close the socket.
--
-- To prevent slowloris attacks, the connection will be also terminated if the
-- input socket produces data too slowly (500 bytes per second is the default
-- limit).
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> import qualified "Data.ByteString.Lazy" as L
-- ghci> import "Data.Char" (toUpper)
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "System.IO.Streams" as Streams
-- ghci> let r = T.put \"\/foo\" \"text\/plain\" \"some text\"
-- ghci> :{
-- ghci| let f s = do u \<- Streams.map (B8.map toUpper) s
-- ghci|              l \<- Streams.toList u
-- ghci|              return $ L.fromChunks l
-- ghci| :}
-- ghci> T.runHandler r ('runRequestBody' f >>= 'writeLBS')
-- HTTP/1.1 200 OK
-- server: Snap/test
-- date: Thu, 07 Aug 2014 20:48:40 GMT
--
-- SOME TEXT
-- @
runRequestBody :: MonadSnap m =>
                  (InputStream ByteString -> IO a)
               -> m a
runRequestBody proc = do
    bumpTimeout <- liftM ($ max 5) getTimeoutModifier
    req         <- getRequest
    body        <- liftIO $ Streams.throwIfTooSlow bumpTimeout 500 5 $
                            rqBody req
    run body

  where
    skip body = liftIO (Streams.skipToEof body) `catch` tooSlow

    tooSlow (e :: Streams.RateTooSlowException) =
        terminateConnection e

    run body = (liftIO $ do
        x <- proc body
        Streams.skipToEof body
        return x) `catches` handlers
      where
        handlers = [ Handler tooSlow, Handler other ]
        other (e :: SomeException) = skip body >> throwIO e


------------------------------------------------------------------------------
-- | Returns the request body as a lazy bytestring. /Note that the request is
-- not actually provided lazily!/
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.put \"\/foo\" \"text\/plain\" \"some text\"
-- ghci> T.runHandler r ('readRequestBody' 2048 >>= 'writeLBS')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 20:08:44 GMT
--
-- some text
-- @
--
-- /Since: 0.6/
readRequestBody :: MonadSnap m =>
                   Word64  -- ^ size of the largest request body we're willing
                           -- to accept. If a request body longer than this is
                           -- received, a 'TooManyBytesReadException' is
                           -- thrown. See 'takeNoMoreThan'.
                -> m L.ByteString
readRequestBody sz = liftM L.fromChunks $ runRequestBody f
  where
    f str = Streams.throwIfProducesMoreThan (fromIntegral sz) str >>=
            Streams.toList


------------------------------------------------------------------------------
-- | Normally Snap is careful to ensure that the request body is fully
-- consumed after your web handler runs, but before the 'Response' body
-- is streamed out the socket. If you want to transform the request body into
-- some output in O(1) space, you should use this function.
--
-- Take care: in order for this to work, the HTTP client must be written with
-- input-to-output streaming in mind.
--
-- Note that upon calling this function, response processing finishes early as
-- if you called 'finishWith'. Make sure you set any content types, headers,
-- cookies, etc. before you call this function.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> import "Data.Char" (toUpper)
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "System.IO.Streams" as Streams
-- ghci> let r = T.put \"\/foo\" \"text\/plain\" \"some text\"
-- ghci> let f = Streams.map (B8.map toUpper)
-- ghci> T.runHandler r ('transformRequestBody' f >> 'readRequestBody' 2048 >>= 'writeLBS')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 20:30:15 GMT
--
-- SOME TEXT
-- @
transformRequestBody :: (InputStream ByteString -> IO (InputStream ByteString))
                         -- ^ the 'InputStream' from the 'Request' is passed to
                         -- this function, and then the resulting 'InputStream'
                         -- is fed to the output.
                     -> Snap ()
transformRequestBody trans = do
    req     <- getRequest
    is      <- liftIO ((trans $ rqBody req) >>=
                         Streams.mapM (return . byteString))
    origRsp <- getResponse
    let rsp = setResponseBody (\out -> Streams.connect is out >> return out) $
              origRsp { rspTransformingRqBody = True }
    finishWith rsp


------------------------------------------------------------------------------
-- | Short-circuits a 'Snap' monad action early, storing the given
-- 'Response' value in its state.
--
-- IMPORTANT: Be vary careful when using this with things like a DB library's
-- `withTransaction` function or any other kind of setup/teardown block, as it
-- can prevent the cleanup from being called and result in resource leaks.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import "Control.Applicative"
-- ghci> let r = T.get \"\/\" M.empty
-- ghci> T.runHandler r (('ifTop' $ 'writeBS' \"TOP\") \<|> 'finishWith' 'emptyResponse')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 16:58:57 GMT
--
-- TOP
-- ghci> let r\' = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r\' (('ifTop' $ 'writeBS' \"TOP\") \<|> 'finishWith' 'emptyResponse')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 17:50:50 GMT
--
--
-- @
finishWith :: MonadSnap m => Response -> m a
finishWith r = liftSnap $ Snap $ \_ fk st -> fk (EarlyTermination r) st
{-# INLINE finishWith #-}


------------------------------------------------------------------------------
-- | Capture the flow of control in case a handler calls 'finishWith'.
--
-- /WARNING/: in the event of a call to 'transformRequestBody' it is possible
-- to violate HTTP protocol safety when using this function. If you call
-- 'catchFinishWith' it is suggested that you do not modify the body of the
-- 'Response' which was passed to the 'finishWith' call.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import "Control.Applicative"
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> let h = ('ifTop' $ 'writeBS' \"TOP\") \<|> 'finishWith' 'emptyResponse'
-- ghci> T.runHandler r ('catchFinishWith' h >>= 'writeBS' . B8.pack . show)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 18:35:42 GMT
--
-- Left HTTP\/1.1 200 OK
--
--
-- @
catchFinishWith :: Snap a -> Snap (Either Response a)
catchFinishWith (Snap m) = Snap $ \sk fk st -> do
    let sk' v s = sk (Right v) s
    let fk' z s = case z of
                    (EarlyTermination resp) -> sk (Left resp) s
                    _                       -> fk z s
    m sk' fk' st
{-# INLINE catchFinishWith #-}


------------------------------------------------------------------------------
-- | Fails out of a 'Snap' monad action.  This is used to indicate
-- that you choose not to handle the given request within the given
-- handler.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r 'pass'
-- HTTP\/1.1 404 Not Found
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 13:35:42 GMT
--
-- \<!DOCTYPE html>
-- \<html>
-- \<head>
-- \<title>Not found\<\/title>
-- \<\/head>
-- \<body>
-- \<code>No handler accepted \"\/foo\/bar\"<\/code>
-- \<\/body>\<\/html>
-- @
pass :: MonadSnap m => m a
pass = empty


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only if the request's HTTP method matches
-- the given method.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('method' 'GET' $ 'writeBS' \"OK\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 13:38:48 GMT
--
-- OK
-- ghci> T.runHandler r ('method' 'POST' $ 'writeBS' \"OK\")
-- HTTP\/1.1 404 Not Found
-- ...
-- @
method :: MonadSnap m => Method -> m a -> m a
method m action = do
    req <- getRequest
    unless (rqMethod req == m) pass
    action
{-# INLINE method #-}


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only if the request's HTTP method matches
-- one of the given methods.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('methods' ['GET', 'POST'] $ 'writeBS' \"OK\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 13:38:48 GMT
--
-- OK
-- ghci> T.runHandler r ('methods' ['POST'] $ 'writeBS' \"OK\")
-- HTTP\/1.1 404 Not Found
-- ...
-- @
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('dir' \"foo\" $ 'writeBS' \"OK\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 14:52:24 GMT
--
-- OK
-- ghci> T.runHandler r ('dir' \"baz\" $ 'writeBS' \"OK\")
-- HTTP\/1.1 404 Not Found
-- ...
-- @
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> T.runHandler (T.get \"\/foo\" M.empty) ('path' \"foo\" $ 'writeBS' \"bar\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 14:15:42 GMT
--
-- bar
-- ghci> T.runHandler (T.get \"\/foo\" M.empty) ('path' \"bar\" $ 'writeBS' \"baz\")
-- HTTP\/1.1 404 Not Found
-- ...
-- @
path :: MonadSnap m
     => ByteString  -- ^ path to match against
     -> m a         -- ^ handler to run
     -> m a
path = pathWith (==)
{-# INLINE path #-}


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only when the first path component is
-- successfully parsed as the argument to the supplied handler function.
--
-- Note that the path segment is url-decoded prior to being passed to 'fromBS';
-- this is new as of snap-core 0.10.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/11\/foo\/bar\" M.empty
-- ghci> let f = (\\i -> if i == 11 then 'writeBS' \"11\" else 'writeBS' \"???\")
-- ghci> T.runHandler r ('pathArg' f)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 14:27:10 GMT
--
-- 11
-- ghci> let r\' = T.get \"\/foo\/11\/bar\" M.empty
-- ghci> T.runHandler r\' ('pathArg' f)
-- HTTP\/1.1 404 Not Found
-- ...
-- @
pathArg :: (R.Readable a, MonadSnap m)
        => (a -> m b)
        -> m b
pathArg f = do
    req <- getRequest
    let (p,_) = S.break (=='/') (rqPathInfo req)
    p' <- maybe mzero return $ urlDecode p
    a <- R.fromBS p'
    localRequest (updateContextPath $ S.length p) (f a)


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only when 'rqPathInfo' is empty.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/\" M.empty
-- ghci> T.runHandler r ('ifTop' $ 'writeBS' "OK")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 14:56:39 GMT
--
-- OK
-- ghci> let r\' = T.get \"\/foo\" M.empty
-- ghci> T.runHandler r\' ('ifTop' $ 'writeBS' \"OK\")
-- HTTP\/1.1 404 Not Found
-- ...
-- @
ifTop :: MonadSnap m => m a -> m a
ifTop = path ""
{-# INLINE ifTop #-}


------------------------------------------------------------------------------
-- | Local Snap version of 'get'.
sget :: Snap SnapState
sget = Snap $ \sk _ st -> sk st st
{-# INLINE sget #-}


------------------------------------------------------------------------------
-- | Local Snap monad version of 'modify'.
smodify :: (SnapState -> SnapState) -> Snap ()
smodify f = Snap $ \sk _ st -> sk () (f st)
{-# INLINE smodify #-}


------------------------------------------------------------------------------
-- | Grabs the 'Request' object out of the 'Snap' monad.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeBS' . 'rqURI' =\<\< 'getRequest')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Sat, 02 Aug 2014 07:51:54 GMT
--
-- \/foo\/bar
-- @
getRequest :: MonadSnap m => m Request
getRequest = liftSnap $ liftM _snapRequest sget
{-# INLINE getRequest #-}


------------------------------------------------------------------------------
-- | Grabs something out of the 'Request' object, using the given projection
-- function. See 'gets'.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeBS' =\<\< 'getsRequest' 'rqURI')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Sat, 02 Aug 2014 07:51:54 GMT
--
-- \/foo\/bar
-- @
getsRequest :: MonadSnap m => (Request -> a) -> m a
getsRequest f = liftSnap $ liftM (f . _snapRequest) sget
{-# INLINE getsRequest #-}


------------------------------------------------------------------------------
-- | Grabs the 'Response' object out of the 'Snap' monad.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeBS' . 'rspStatusReason' =\<\< 'getResponse')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Sat, 02 Aug 2014 15:06:00 GMT
--
-- OK
-- @
getResponse :: MonadSnap m => m Response
getResponse = liftSnap $ liftM _snapResponse sget
{-# INLINE getResponse #-}


------------------------------------------------------------------------------
-- | Grabs something out of the 'Response' object, using the given projection
-- function. See 'gets'.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeBS' =\<\< 'getsResponse' 'rspStatusReason')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 13:35:45 GMT
--
-- OK
-- @
getsResponse :: MonadSnap m => (Response -> a) -> m a
getsResponse f = liftSnap $ liftM (f . _snapResponse) sget
{-# INLINE getsResponse #-}


------------------------------------------------------------------------------
-- | Puts a new 'Response' object into the 'Snap' monad.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let rsp = 'setResponseCode' 404 'emptyResponse'
-- ghci> let req = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler req ('putResponse' rsp)
-- HTTP\/1.1 404 Not Found
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 13:59:58 GMT
--
--
-- @
putResponse :: MonadSnap m => Response -> m ()
putResponse r = liftSnap $ smodify $ \ss -> ss { _snapResponse = r }
{-# INLINE putResponse #-}


------------------------------------------------------------------------------
-- | Puts a new 'Request' object into the 'Snap' monad.
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> :{
-- ghci| let hndlr = do rq \<- T.buildRequest (T.get \"\/bar\/foo\" M.empty)
-- ghci|                'putRequest' rq
-- ghci|                uri\' \<- 'getsRequest' 'rqURI'
-- ghci|                'writeBS' uri\'
-- ghci| :}
-- ghci> T.runHandler (T.get \"\/foo\/bar\" M.empty) hndlr
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 15:13:46 GMT
--
-- \/bar\/foo
-- @
putRequest :: MonadSnap m => Request -> m ()
putRequest r = liftSnap $ smodify $ \ss -> ss { _snapRequest = r }
{-# INLINE putRequest #-}


------------------------------------------------------------------------------
-- | Modifies the 'Request' object stored in a 'Snap' monad.
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> r\' \<- T.buildRequest $ T.get \"\/bar\/foo\" M.empty
-- ghci> T.runHandler r ('modifyRequest' (const r\') >> 'getsRequest' 'rqURI' >>= 'writeBS')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 15:24:25 GMT
--
-- \/bar\/foo
-- @
modifyRequest :: MonadSnap m => (Request -> Request) -> m ()
modifyRequest f = liftSnap $
    smodify $ \ss -> ss { _snapRequest = f $ _snapRequest ss }
{-# INLINE modifyRequest #-}


------------------------------------------------------------------------------
-- | Modifes the 'Response' object stored in a 'Snap' monad.
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('modifyResponse' $ 'setResponseCode' 404)
-- HTTP\/1.1 404 Not Found
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 15:27:11 GMT
--
--
-- @
modifyResponse :: MonadSnap m => (Response -> Response) -> m ()
modifyResponse f = liftSnap $
     smodify $ \ss -> ss { _snapResponse = f $ _snapResponse ss }
{-# INLINE modifyResponse #-}


------------------------------------------------------------------------------
-- | Performs a redirect by setting the @Location@ header to the given target
-- URL/path and the status code to 302 in the 'Response' object stored in a
-- 'Snap' monad. Note that the target URL is not validated in any way.
-- Consider using 'redirect'' instead, which allows you to choose the correct
-- status code.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('redirect' \"http:\/\/snapframework.com\")
-- HTTP\/1.1 302 Found
-- content-length: 0
-- location: http:\/\/snapframework.com
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 08:52:11 GMT
-- Content-Length: 0
--
--
-- @
redirect :: MonadSnap m => ByteString -> m a
redirect target = redirect' target 302
{-# INLINE redirect #-}


------------------------------------------------------------------------------
-- | Performs a redirect by setting the @Location@ header to the given target
-- URL/path and the status code (should be one of 301, 302, 303 or 307) in the
-- 'Response' object stored in a 'Snap' monad. Note that the target URL is not
-- validated in any way.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('redirect'' \"http:\/\/snapframework.com\" 301)
-- HTTP\/1.1 307 Temporary Redirect
-- content-length: 0
-- location: http:\/\/snapframework.com
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 08:55:51 GMT
-- Content-Length: 0
--
--
-- @
redirect' :: MonadSnap m => ByteString -> Int -> m a
redirect' target status = do
    r <- getResponse

    finishWith
        $ setResponseCode status
        $ setContentLength 0
        $ modifyResponseBody (const $ return . id)
        $ setHeader "Location" target r

{-# INLINE redirect' #-}


------------------------------------------------------------------------------
-- | Log an error message in the 'Snap' monad.
--
-- Example:
--
-- @
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> 'runSnap' ('logError' \"fatal error!\") ('error' . B8.unpack) undefined undefined
-- *** Exception: fatal error!
-- @
logError :: MonadSnap m => ByteString -> m ()
logError s = liftSnap $ Snap $ \sk _ st -> do
    _snapLogError st s
    sk () st
{-# INLINE logError #-}


------------------------------------------------------------------------------
-- | Run the given stream procedure, adding its output to the 'Response' stored
-- in the 'Snap' monad state.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Builder" as B
-- ghci> import qualified "System.IO.Streams" as Streams
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> :{
-- ghci| let f str = do {
-- ghci|   Streams.write (Just $ B.byteString \"Hello, streams world\") str;
-- ghci|   return str }
-- ghci| :}
-- ghci> T.runHandler r ('addToOutput' f)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 17:55:47 GMT
--
-- Hello, streams world
-- @
addToOutput :: MonadSnap m
            => (OutputStream Builder -> IO (OutputStream Builder))
                    -- ^ output to add
            -> m ()
addToOutput enum = modifyResponse $ modifyResponseBody (c enum)
  where
    c a b = \out -> b out >>= a

------------------------------------------------------------------------------
-- | Adds the given 'Builder' to the body of the 'Response' stored in the
-- | 'Snap' monad state.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Builder" as B
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeBuilder' $ B.byteString \"Hello, world\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 17:33:33 GMT
--
-- Hello, world
-- @
writeBuilder :: MonadSnap m => Builder -> m ()
writeBuilder b = addToOutput f
  where
    f str = Streams.write (Just b) str >> return str
{-# INLINE writeBuilder #-}


------------------------------------------------------------------------------
-- | Adds the given strict 'ByteString' to the body of the 'Response' stored
-- in the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeBS' \"Hello, bytestring world\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 17:34:27 GMT
--
-- Hello, bytestring world
-- @
writeBS :: MonadSnap m => ByteString -> m ()
writeBS = writeBuilder . byteString
{-# INLINE writeBS #-}


------------------------------------------------------------------------------
-- | Adds the given lazy 'L.ByteString' to the body of the 'Response' stored
-- in the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeLBS' \"Hello, lazy bytestring world\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 17:35:15 GMT
--
-- Hello, lazy bytestring world
-- @
writeLBS :: MonadSnap m => L.ByteString -> m ()
writeLBS = writeBuilder . lazyByteString
{-# INLINE writeLBS #-}


------------------------------------------------------------------------------
-- | Adds the given strict 'T.Text' to the body of the 'Response' stored in
-- the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeText' \"Hello, text world\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 17:36:38 GMT
--
-- Hello, text world
-- @
writeText :: MonadSnap m => T.Text -> m ()
writeText = writeBS . T.encodeUtf8
  -- it's inefficient, but we don't have bytestring builder text functions for
  -- 0.9-era bytestring
{-# INLINE writeText #-}


------------------------------------------------------------------------------
-- | Adds the given lazy 'LT.Text' to the body of the 'Response' stored in the
-- 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('writeLazyText' \"Hello, lazy text world\")
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 17:37:41 GMT
--
-- Hello, lazy text world
-- @
writeLazyText :: MonadSnap m => LT.Text -> m ()
writeLazyText = writeLBS . LT.encodeUtf8
{-# INLINE writeLazyText #-}


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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> 'writeFile' \"\/tmp\/snap-file\" \"Hello, sendFile world\"
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('sendFile' \"\/tmp\/snap-file\")
-- HTTP\/1.1 200 OK
-- content-length: 21
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 17:45:10 GMT
-- Content-Length: 21
--
-- Hello, sendFile world
-- @
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> 'writeFile' \"\/tmp\/snap-file\" \"Hello, sendFilePartial world\"
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('sendFilePartial' \"\/tmp\/snap-file\" (7, 28))
-- HTTP\/1.1 200 OK
-- content-length: 21
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 17:47:20 GMT
-- Content-Length: 21
--
-- sendFilePartial world
-- @
sendFilePartial :: (MonadSnap m) => FilePath -> (Word64, Word64) -> m ()
sendFilePartial f rng = modifyResponse $ \r ->
                        r { rspBody = SendFile f (Just rng) }


------------------------------------------------------------------------------
-- | Runs a 'Snap' action with a locally-modified 'Request' state
-- object. The 'Request' object in the Snap monad state after the call
-- to localRequest will be unchanged.
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> r\' \<- T.buildRequest $ T.get \"\/bar\/foo\" M.empty
-- ghci> let printRqURI = 'getsRequest' 'rqURI' >>= 'writeBS' >> 'writeBS' \"\\n\"
-- ghci> T.runHandler r (printRqURI >> 'localRequest' (const r\') printRqURI)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 15:34:12 GMT
--
-- \/foo\/bar
-- \/bar\/foo
--
-- @
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
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import "Control.Monad.IO.Class"
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> let h = 'withRequest' (\\rq -> 'liftIO' (T.requestToString rq) >>= 'writeBS')
-- ghci> T.runHandler r h
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 15:44:24 GMT
--
-- GET \/foo\/bar HTTP\/1.1
-- host: localhost
--
--
-- @
withRequest :: MonadSnap m => (Request -> m a) -> m a
withRequest = (getRequest >>=)
{-# INLINE withRequest #-}


------------------------------------------------------------------------------
-- | Fetches the 'Response' from state and hands it to the given action.
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r ('withResponse' $ 'writeBS' . 'rspStatusReason')
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Wed, 06 Aug 2014 15:48:45 GMT
--
-- OK
-- @
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

    let whitespace = [ ' ', '\t', '\r', '\n' ]
        ipChrs = '.' : "0123456789"
        trim f s = f (`elem` s)

        clean = trim S.takeWhile ipChrs . trim S.dropWhile whitespace
        setIP ip = modifyRequest $ \rq -> rq { rqClientAddr = clean ip }
    maybe (return $! ()) setIP headerContents


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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let br = 'bracketSnap' (putStrLn \"before\") (const $ putStrLn \"after\")
-- ghci> T.runHandler (T.get \"/\" M.empty) (br $ const $ writeBS \"OK\")
-- before
-- after
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Thu, 07 Aug 2014 18:41:50 GMT
--
-- OK
-- @
bracketSnap :: IO a -> (a -> IO b) -> (a -> Snap c) -> Snap c
bracketSnap before after thing = mask $ \restore ->
                                 stateTToSnap $ do
    a <- liftIO before
    let after' = liftIO $ after a
    r <- snapToStateT (restore $ thing a) `onException` after'
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Control.Exception" as E
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> T.runHandler r (terminateConnection $ E.AssertionFailed \"Assertion failed!\")
-- *** Exception: \<terminated: Assertion failed!>
-- @
terminateConnection :: (Exception e, MonadSnap m) => e -> m a
terminateConnection e =
    liftSnap $ Snap $ \_ fk -> fk $ EscapeSnap $ TerminateConnection
                                  $ SomeException e


------------------------------------------------------------------------------
-- | Terminate the HTTP session and hand control to some external handler,
-- escaping all further HTTP traffic.
--
-- The external handler takes three arguments: a function to modify the thread's
-- timeout, and a read and a write ends to the socket.
escapeHttp :: MonadSnap m =>
              EscapeHttpHandler
           -> m ()
escapeHttp h = liftSnap $ Snap $ \_ fk st -> fk (EscapeSnap $ EscapeHttp h) st


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action.
--
-- This function is mostly intended for library writers; instead of invoking
-- 'runSnap' directly, use 'Snap.Http.Server.httpServe' or
-- 'Snap.Test.runHandler' (for testing).
runSnap :: Snap a                   -- ^ Action to run.
        -> (ByteString -> IO ())    -- ^ Error logging action.
        -> ((Int -> Int) -> IO ())  -- ^ Timeout action.
        -> Request                  -- ^ HTTP request.
        -> IO (Request, Response)
runSnap (Snap m) logerr timeoutAction req =
    m ok diediedie ss
  where
    ok _ st = return (_snapRequest st, _snapResponse st)

    diediedie z !st = do
        resp <- case z of
                  PassOnProcessing     -> return fourohfour
                  (EarlyTermination x) -> return x
                  (EscapeSnap e)       -> throwIO e
        return (_snapRequest st, resp)

    --------------------------------------------------------------------------
    fourohfour = do
        clearContentLength                  $
          setResponseStatus 404 "Not Found" $
          setResponseBody enum404           $
          emptyResponse

    --------------------------------------------------------------------------
    enum404 out = do
        is <- Streams.fromList html
        Streams.connect is out
        return out

    --------------------------------------------------------------------------
    html = map byteString [ "<!DOCTYPE html>\n"
                          , "<html>\n"
                          , "<head>\n"
                          , "<title>Not found</title>\n"
                          , "</head>\n"
                          , "<body>\n"
                          , "<code>No handler accepted \""
                          , rqURI req
                          , "\"</code>\n</body></html>"
                          ]

    --------------------------------------------------------------------------
    dresp = emptyResponse

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
--
{-# INLINE fixupResponse #-}
fixupResponse :: Request -> Response -> IO Response
fixupResponse req rsp = {-# SCC "fixupResponse" #-} do
    rsp' <- case rspBody rsp of
              (Stream _)                -> return rsp
              (SendFile f Nothing)      -> setFileSize f rsp
              (SendFile _ (Just (s,e))) -> return $! setContentLength (e-s) rsp
    let !cl = if noBody then Nothing else rspContentLength rsp'
    let rsp'' = if noBody
                  then rsp' { rspBody          = Stream $ return . id
                            , rspContentLength = Nothing
                            }
                  else rsp'
    return $! updateHeaders (H.fromList . addCL cl . fixup . H.toList) rsp''

  where
    --------------------------------------------------------------------------
    addCL Nothing xs   = xs
    addCL (Just cl) xs = ("content-length", word64ToByteString cl):xs

    --------------------------------------------------------------------------
    setFileSize :: FilePath -> Response -> IO Response
    setFileSize fp r = {-# SCC "setFileSize" #-} do
        fs <- liftM fromIntegral $ getFileSize fp
        return $! r { rspContentLength = Just fs }

    ------------------------------------------------------------------------------
    getFileSize :: FilePath -> IO FileOffset
    getFileSize fp = liftM fileSize $ getFileStatus fp

    code   = rspStatus rsp
    noBody = code == 204 || code == 304 || rqMethod req == HEAD

    ------------------------------------------------------------------------------
    fixup [] = []
    fixup (("date",_):xs)           = fixup xs
    fixup (("content-length",_):xs) = fixup xs
    fixup (x@("transfer-encoding",_):xs) = if noBody
                                             then fixup xs
                                             else x : fixup xs
    fixup (x:xs) = x : fixup xs


------------------------------------------------------------------------------
-- This number code stolen and massaged from Bryan's blog post:
-- http://www.serpentine.com/blog/2013/03/20/whats-good-for-c-is-good-for-haskell/

{-# INLINE countDigits #-}
countDigits :: Word64 -> Int
countDigits v0 = go 1 v0
  where go !k v
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 10000 = k + 3
           | otherwise = go (k+4) (v `quot` 10000)


------------------------------------------------------------------------------
{-# INLINE word64ToByteString #-}
word64ToByteString :: Word64 -> ByteString
word64ToByteString d =
#if !MIN_VERSION_bytestring(0,10,6)
    S.inlinePerformIO $
#else
    S.accursedUnutterablePerformIO $
#endif
    if d < 10
       then S.create 1 $ \p -> poke p (i2w d)
       else let !n = countDigits d
            in S.create n $ posDecimal n d


{-# INLINE posDecimal #-}
posDecimal :: Int -> Word64 -> Ptr Word8 -> IO ()
posDecimal !n0 !v0 !op0 = go n0 (plusPtr op0 (n0-1)) v0
  where go !n !op !v
          | n == 1 = poke op $! i2w v
          | otherwise = do
              let (!v', !d) = divMod v 10
              poke op $! i2w d
              go (n-1) (plusPtr op (-1)) v'


{-# INLINE i2w #-}
i2w :: Word64 -> Word8
i2w v = 48 + fromIntegral v


------------------------------------------------------------------------------
evalSnap :: Snap a
         -> (ByteString -> IO ())
         -> ((Int -> Int) -> IO ())
         -> Request
         -> IO a
evalSnap (Snap m) logerr timeoutAction req =
    m (\v _ -> return v) diediedie ss
  where
    diediedie z _ = case z of
      PassOnProcessing     -> throwIO $ NoHandlerException "pass"
      (EarlyTermination _) -> throwIO $ ErrorCall "no value"
      (EscapeSnap e)       -> throwIO e

    dresp = emptyResponse
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
-- 'getParam' gloms the values together with @'S.intercalate' \" \"@.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> let r = T.get \"\/foo\/bar\" $ M.fromList [(\"foo\", [\"bar\"])]
-- ghci> T.runHandler r ('getParam' \"foo\" >>= 'writeBS' . B8.pack . show)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Mon, 11 Aug 2014 12:57:20 GMT
--
-- Just \"bar\"
-- @
getParam :: MonadSnap m
         => ByteString          -- ^ parameter name to look up
         -> m (Maybe ByteString)
getParam = getParamFrom rqParam
{-# INLINE getParam #-}


------------------------------------------------------------------------------
-- | See 'rqPostParam'. Looks up a value for the given named parameter in the
-- POST form parameters mapping in 'Request'. If more than one value was
-- entered for the given parameter name, 'getPostParam' gloms the values
-- together with: @'S.intercalate' \" \"@.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> let r = T.postUrlEncoded \"\/foo\/bar\" $ M.fromList [(\"foo\", [\"bar\"])]
-- ghci> T.runHandler r ('getPostParam' \"foo\" >>= 'writeBS' . B8.pack . show)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Mon, 11 Aug 2014 13:01:04 GMT
--
-- Just \"bar\"
-- @
getPostParam :: MonadSnap m
             => ByteString          -- ^ parameter name to look up
             -> m (Maybe ByteString)
getPostParam = getParamFrom rqPostParam
{-# INLINE getPostParam #-}


------------------------------------------------------------------------------
-- | See 'rqQueryParam'. Looks up a value for the given named parameter in the
-- query string parameters mapping in 'Request'. If more than one value was
-- entered for the given parameter name, 'getQueryParam' gloms the values
-- together with  @'S.intercalate' \" \"@.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> let r = T.postUrlEncoded \"\/foo\/bar\" M.empty >> T.setQueryStringRaw \"foo=bar&foo=baz\"
-- ghci> T.runHandler r ('getQueryParam' \"foo\" >>= 'writeBS' . B8.pack . show)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Mon, 11 Aug 2014 13:06:50 GMT
--
-- Just \"bar baz\"
-- @
getQueryParam :: MonadSnap m
              => ByteString          -- ^ parameter name to look up
              -> m (Maybe ByteString)
getQueryParam = getParamFrom rqQueryParam
{-# INLINE getQueryParam #-}


------------------------------------------------------------------------------
-- | See 'rqParams'. Convenience function to return 'Params' from the
-- 'Request' inside of a 'MonadSnap' instance.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> let r = T.get \"\/foo\/bar\" $ M.fromList [(\"foo\", [\"bar\"])]
-- ghci> T.runHandler r ('getParams' >>= 'writeBS' . B8.pack . show)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Mon, 11 Aug 2014 13:02:54 GMT
--
-- fromList [(\"foo\",[\"bar\"])]
-- @
getParams :: MonadSnap m => m Params
getParams = getRequest >>= return . rqParams


------------------------------------------------------------------------------
-- | See 'rqParams'. Convenience function to return 'Params' from the
-- 'Request' inside of a 'MonadSnap' instance.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> let r = T.postUrlEncoded \"\/foo\/bar\" $ M.fromList [(\"foo\", [\"bar\"])]
-- ghci> T.runHandler r ('getPostParams' >>= 'writeBS' . B8.pack . show)
-- HTTP/1.1 200 OK
-- server: Snap/test
-- date: Mon, 11 Aug 2014 13:04:34 GMT
--
-- fromList [("foo",["bar"])]
-- @
getPostParams :: MonadSnap m => m Params
getPostParams = getRequest >>= return . rqPostParams


------------------------------------------------------------------------------
-- | See 'rqParams'. Convenience function to return 'Params' from the
-- 'Request' inside of a 'MonadSnap' instance.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> let r = T.postUrlEncoded \"\/foo\/bar\" M.empty >> T.setQueryStringRaw \"foo=bar&foo=baz\"
-- ghci> T.runHandler r ('getQueryParams' >>= 'writeBS' . B8.pack . show)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Mon, 11 Aug 2014 13:10:17 GMT
--
-- fromList [(\"foo\",[\"bar\",\"baz\"])]
-- @
getQueryParams :: MonadSnap m => m Params
getQueryParams = getRequest >>= return . rqQueryParams


------------------------------------------------------------------------------
-- | Gets the HTTP 'Cookie' with the specified name.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> let cookie = 'Cookie' \"name\" \"value\" Nothing Nothing Nothing False False
-- ghci> let r = T.get \"\/foo\/bar\" M.empty >> T.addCookies [cookie]
-- ghci> T.runHandler r ('getCookie' \"name\" >>= 'writeBS' . B8.pack . show)
-- HTTP/1.1 200 OK
-- server: Snap/test
-- date: Thu, 07 Aug 2014 12:16:58 GMT
--
-- Just (Cookie {cookieName = "name", cookieValue = "value", ...})
-- @
getCookie :: MonadSnap m
          => ByteString
          -> m (Maybe Cookie)
getCookie name = withRequest $
    return . listToMaybe . filter (\c -> cookieName c == name) . rqCookies


------------------------------------------------------------------------------
-- | Gets the HTTP 'Cookie' with the specified name and decodes it.  If the
-- decoding fails, the handler calls pass.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let cookie = 'Cookie' \"name\" \"value\" Nothing Nothing Nothing False False
-- ghci> let r = T.get \"\/foo\/bar\" M.empty >> T.addCookies [cookie]
-- ghci> T.runHandler r ('readCookie' \"name\" >>= 'writeBS')
-- HTTP/1.1 200 OK
-- server: Snap/test
-- date: Thu, 07 Aug 2014 12:20:09 GMT
--
-- value
-- @
readCookie :: (MonadSnap m, R.Readable a)
           => ByteString
           -> m a
readCookie name = maybe pass (R.fromBS . cookieValue) =<< getCookie name


------------------------------------------------------------------------------
-- | Expire given 'Cookie' in client's browser.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\/bar\" M.empty
-- ghci> let cookie = Cookie "name" "" Nothing (Just "/subsite") Nothing True False
-- ghci> T.runHandler r ('expireCookie' cookie)
-- HTTP/1.1 200 OK
-- set-cookie: name=; path=/subsite; expires=Sat, 24 Dec 1994 06:28:16 GMT; Secure
-- server: Snap/test
--
-- date: Thu, 07 Aug 2014 12:21:27 GMT
-- ghci> let cookie = Cookie "name" "value" Nothing Nothing Nothing False False
-- ghci> let r2 = T.get \"\/foo\/bar\" M.empty >> T.addCookies [cookie]
-- ghci> T.runHandler r ('getCookie' "name" >>= maybe (return ()) 'expireCookie')
-- HTTP/1.1 200 OK
-- set-cookie: name=; expires=Sat, 24 Dec 1994 06:28:16 GMT
-- server: Snap/test
--
--
-- @
expireCookie :: (MonadSnap m) => Cookie -> m ()
expireCookie cookie = do
  let old = UTCTime (ModifiedJulianDay 0) 0
  modifyResponse $ addResponseCookie
                 $ cookie { cookieValue = ""
                          , cookieExpires = (Just old) }

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
-- | Returns an 'IO' action which you can use to modify the timeout value.
getTimeoutModifier :: MonadSnap m => m ((Int -> Int) -> IO ())
getTimeoutModifier = liftSnap $ liftM _snapModifyTimeout sget
