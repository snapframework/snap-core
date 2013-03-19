{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}

module Snap.Internal.Test.RequestBuilder
  ( RequestBuilder
  , MultipartParams
  , MultipartParam(..)
  , FileData      (..)
  , RequestType   (..)
  , addHeader
  , buildRequest
  , delete
  , dumpResponse
  , evalHandler
  , evalHandlerM
  , get
  , postMultipart
  , postRaw
  , postUrlEncoded
  , put
  , responseToString
  , runHandler
  , runHandlerM
  , setContentType
  , setHeader
  , addCookies  
  , setHttpVersion
  , setQueryString
  , setQueryStringRaw
  , setRequestPath
  , setRequestType
  , setSecure
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           Control.Monad.State            hiding (get, put)
import qualified Control.Monad.State            as State
import           Data.Bits
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as S
import qualified Data.ByteString                as S8
import           Data.CaseInsensitive           (CI)
import           Data.IORef
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Word
import qualified Data.Vector                    as V
import           System.PosixCompat.Time
import           System.Random
------------------------------------------------------------------------------
import           Snap.Internal.Http.Types hiding (addHeader,
                                                  setContentType,
                                                  setHeader)
import qualified Snap.Internal.Http.Types       as H
import           Snap.Internal.Parsing
import           Snap.Internal.Types            (evalSnap)
import           Snap.Iteratee                  hiding (map)
import           Snap.Core                      hiding ( addHeader
                                                       , setContentType
                                                       , setHeader )
import qualified Snap.Types.Headers             as H


------------------------------------------------------------------------------
-- | RequestBuilder is a monad transformer that allows you to conveniently
-- build a snap 'Request' for testing.
newtype RequestBuilder m a = RequestBuilder (StateT Request m a)
  deriving (Monad, MonadIO, MonadState Request, MonadTrans)


------------------------------------------------------------------------------
mkDefaultRequest :: IO Request
mkDefaultRequest = do
    bodyRef <- newIORef $ SomeEnumerator enumEOF
    return $ Request "localhost"
                     8080
                     "127.0.0.1"
                     60000
                     "127.0.0.1"
                     8080
                     "localhost"
                     False
                     H.empty
                     bodyRef
                     Nothing
                     GET
                     (1,1)
                     []
                     ""
                     "/"
                     "/"
                     ""
                     Map.empty
                     Map.empty
                     Map.empty


------------------------------------------------------------------------------
-- | Runs a 'RequestBuilder', producing the desired 'Request'.
--
-- N.B. /please/ don't use the request you get here in a real Snap application;
-- things will probably break. Don't say you weren't warned :-)
--
buildRequest :: MonadIO m => RequestBuilder m () -> m Request
buildRequest mm = do
    let (RequestBuilder m) = (mm >> fixup)
    rq0 <- liftIO mkDefaultRequest
    execStateT m rq0

  where
    fixup = do
        fixupURI
        fixupMethod
        fixupCL
        fixupParams

    fixupMethod = do
        rq <- rGet
        if (rqMethod rq == GET || rqMethod rq == DELETE ||
            rqMethod rq == HEAD)
          then do
              -- These requests are not permitted to have bodies
              let rq' = deleteHeader "Content-Type" rq
              liftIO $ writeIORef (rqBody rq') (SomeEnumerator enumEOF)
              rPut $ rq' { rqContentLength = Nothing }
          else return $! ()

    fixupCL = do
        rq <- rGet
        maybe (rPut $ deleteHeader "Content-Length" rq)
              (\cl -> rPut $ H.setHeader "Content-Length"
                                         (S.pack (show cl)) rq)
              (rqContentLength rq)

    fixupParams = do
        rq <- rGet
        let query       = rqQueryString rq
        let queryParams = parseUrlEncoded query
        let mbCT        = getHeader "Content-Type" rq

        postParams <- if mbCT == Just "application/x-www-form-urlencoded"
                        then do
                          (SomeEnumerator e) <- liftIO $ readIORef $ rqBody rq
                          s <- liftM S.concat (liftIO $ run_ $ e $$ consume)
                          return $ parseUrlEncoded s
                        else return Map.empty

        rPut $ rq { rqParams      = Map.unionWith (++) queryParams postParams
                  , rqQueryParams = queryParams }


------------------------------------------------------------------------------
-- | A request body of type \"@multipart/form-data@\" consists of a set of
-- named form parameters, each of which can by either a list of regular form
-- values or a set of file uploads.
type MultipartParams = [(ByteString, MultipartParam)]


------------------------------------------------------------------------------
data MultipartParam =
    FormData [ByteString]
        -- ^ a form variable consisting of the given 'ByteString' values.
  | Files [FileData]
        -- ^ a file upload consisting of the given 'FileData' values.
  deriving (Show)


------------------------------------------------------------------------------
data FileData = FileData {
      fdFileName    :: ByteString  -- ^ the file's name
    , fdContentType :: ByteString  -- ^ the file's content-type
    , fdContents    :: ByteString  -- ^ the file contents
    }
  deriving (Show)


------------------------------------------------------------------------------
-- | The 'RequestType' datatype enumerates the different kinds of HTTP
-- requests you can generate using the testing interface. Most users will
-- prefer to use the 'get', 'postUrlEncoded', 'postMultipart', 'put', and
-- 'delete' convenience functions.
data RequestType
    = GetRequest
    | RequestWithRawBody Method ByteString
    | MultipartPostRequest MultipartParams
    | UrlEncodedPostRequest Params
    | DeleteRequest
    deriving (Show)


------------------------------------------------------------------------------
-- | Sets the type of the 'Request' being built.
setRequestType :: MonadIO m => RequestType -> RequestBuilder m ()
setRequestType GetRequest = do
    rq <- rGet
    liftIO $ writeIORef (rqBody rq) $ SomeEnumerator enumEOF
    rPut $ rq { rqMethod        = GET
              , rqContentLength = Nothing
              }

setRequestType DeleteRequest = do
    rq <- rGet
    liftIO $ writeIORef (rqBody rq) $ SomeEnumerator enumEOF
    rPut $ rq { rqMethod        = DELETE
              , rqContentLength = Nothing
              }

setRequestType (RequestWithRawBody m b) = do
    rq <- rGet
    liftIO $ writeIORef (rqBody rq) $ SomeEnumerator $ enumBS b
    rPut $ rq { rqMethod        = m
              , rqContentLength = Just $ S.length b
              }

setRequestType (MultipartPostRequest fp) = encodeMultipart fp

setRequestType (UrlEncodedPostRequest fp) = do
    rq <- liftM (H.setHeader "Content-Type"
                           "application/x-www-form-urlencoded") rGet
    let b = printUrlEncoded fp
    liftIO $ writeIORef (rqBody rq) $ SomeEnumerator $ enumBS b
    rPut $ rq { rqMethod        = POST
              , rqContentLength = Just $ S.length b
              }


------------------------------------------------------------------------------
makeBoundary :: MonadIO m => m ByteString
makeBoundary = do
    xs  <- liftIO $ replicateM 16 randomWord8
    let x = S.pack $ map (toEnum . fromEnum) xs
    return $ S.concat [ "snap-boundary-", encode x ]

  where
    randomWord8 :: IO Word8
    randomWord8 = liftM (\c -> toEnum $ c .&. 0xff) randomIO

    table = V.fromList [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
                       , 'a', 'b', 'c', 'd', 'e', 'f' ]

    encode = toByteString . S8.foldl' f mempty

#if MIN_VERSION_base(4,5,0)
    shR = unsafeShiftR
#else
    shR = shiftR
#endif

    f m c = let low = c .&. 0xf
                hi  = (c .&. 0xf0) `shR` 4
                k   = \i -> fromWord8 $! toEnum $! fromEnum $!
                            V.unsafeIndex table (fromEnum i)
            in m `mappend` k hi `mappend` k low


------------------------------------------------------------------------------
multipartHeader :: ByteString -> ByteString -> Builder
multipartHeader boundary name =
    mconcat [ fromByteString boundary
            , fromByteString "\r\ncontent-disposition: form-data"
            , fromByteString "; name=\""
            , fromByteString name
            , fromByteString "\"\r\n" ]


------------------------------------------------------------------------------
-- Assume initial or preceding "--" just before this
encodeFormData :: ByteString -> ByteString -> [ByteString] -> IO Builder
encodeFormData boundary name vals =
    case vals of
      []  -> return mempty
      [v] -> return $ mconcat [ hdr
                              , cr
                              , fromByteString v
                              , fromByteString "\r\n--" ]
      _   -> multi

  where
    hdr = multipartHeader boundary name
    cr = fromByteString "\r\n"

    oneVal b v = mconcat [ fromByteString b
                         , cr
                         , cr
                         , fromByteString v
                         , fromByteString "\r\n--" ]

    multi = do
        b <- makeBoundary
        return $ mconcat [ hdr
                         , multipartMixed b
                         , cr
                         , fromByteString "--"
                         , mconcat (map (oneVal b) vals)
                         , fromByteString b
                         , fromByteString "--\r\n--" ]


------------------------------------------------------------------------------
multipartMixed :: ByteString -> Builder
multipartMixed b = mconcat [ fromByteString "Content-Type: multipart/mixed"
                           , fromByteString "; boundary="
                           , fromByteString b
                           , fromByteString "\r\n" ]


------------------------------------------------------------------------------
encodeFiles :: ByteString -> ByteString -> [FileData] -> IO Builder
encodeFiles boundary name files =
    case files of
      [] -> return mempty
      _  -> do
          b <- makeBoundary
          return $ mconcat [ hdr
                           , multipartMixed b
                           , cr
                           , fromByteString "--"
                           , mconcat (map (oneVal b) files)
                           , fromByteString b
                           , fromByteString "--\r\n--"
                           ]

  where
    --------------------------------------------------------------------------
    contentDisposition fn = mconcat [
                              fromByteString "Content-Disposition: attachment"
                            , fromByteString "; filename=\""
                            , fromByteString fn
                            , fromByteString "\"\r\n"
                            ]

    --------------------------------------------------------------------------
    contentType ct = mconcat [
                       fromByteString "Content-Type: "
                     , fromByteString ct
                     , cr
                     ]

    --------------------------------------------------------------------------
    oneVal b (FileData fileName ct contents) =
        mconcat [ fromByteString b
                , cr
                , contentType ct
                , contentDisposition fileName
                , fromByteString "Content-Transfer-Encoding: binary\r\n"
                , cr
                , fromByteString contents
                , fromByteString "\r\n--"
                ]

    --------------------------------------------------------------------------
    hdr = multipartHeader boundary name
    cr  = fromByteString "\r\n"


------------------------------------------------------------------------------
encodeMultipart :: MonadIO m => MultipartParams -> RequestBuilder m ()
encodeMultipart kvps = do
    boundary <- liftIO $ makeBoundary
    builders <- liftIO $ mapM (handleOne boundary) kvps

    let b = toByteString $
              mconcat (fromByteString "--" : builders)
                `mappend` finalBoundary boundary

    rq0 <- rGet
    liftIO $ writeIORef (rqBody rq0) $ SomeEnumerator $ enumBS b
    let rq = H.setHeader "Content-Type"
               (S.append "multipart/form-data; boundary=" boundary)
               rq0

    rPut $ rq { rqMethod        = POST
              , rqContentLength = Just $ S.length b
              }


  where
    finalBoundary b = mconcat [fromByteString b, fromByteString "--\r\n"]

    handleOne boundary (name, mp) =
        case mp of
          (FormData vals) -> encodeFormData boundary name vals
          (Files fs)      -> encodeFiles boundary name fs


------------------------------------------------------------------------------
fixupURI :: Monad m => RequestBuilder m ()
fixupURI = do
    rq <- rGet
    let u = S.concat [ rqContextPath rq
                     , rqPathInfo rq
                     , let q = rqQueryString rq
                       in if S.null q
                            then ""
                            else S.append "?" q
                     ]
    rPut $ rq { rqURI = u }


------------------------------------------------------------------------------
-- | Sets the request's query string to be the raw bytestring provided,
-- without any escaping or other interpretation. Most users should instead
-- choose the 'setQueryString' function, which takes a parameter mapping.
setQueryStringRaw :: Monad m => ByteString -> RequestBuilder m ()
setQueryStringRaw r = do
    rq <- rGet
    rPut $ rq { rqQueryString = r }
    fixupURI


------------------------------------------------------------------------------
-- | Escapes the given parameter mapping and sets it as the request's query
-- string.
setQueryString :: Monad m => Params -> RequestBuilder m ()
setQueryString p = setQueryStringRaw $ printUrlEncoded p


------------------------------------------------------------------------------
-- | Sets the given header in the request being built, overwriting any header
-- with the same name already present.
setHeader :: (Monad m) => CI ByteString -> ByteString -> RequestBuilder m ()
setHeader k v = rModify (H.setHeader k v)


------------------------------------------------------------------------------
-- | Adds the given header to the request being built.
addHeader :: (Monad m) => CI ByteString -> ByteString -> RequestBuilder m ()
addHeader k v = rModify (H.addHeader k v)

------------------------------------------------------------------------------
-- | Adds the given cookies to the request being built.
addCookies :: (Monad m) => [Cookie] -> RequestBuilder m ()
addCookies cookies = do
  rModify $ \rq -> rq { rqCookies = rqCookies rq ++ cookies }

------------------------------------------------------------------------------
-- | Sets the request's @content-type@ to the given MIME type.
setContentType :: Monad m => ByteString -> RequestBuilder m ()
setContentType c = rModify (H.setHeader "Content-Type" c)


------------------------------------------------------------------------------
-- | Controls whether the test request being generated appears to be an https
-- request or not.
setSecure :: Monad m => Bool -> RequestBuilder m ()
setSecure b = rModify $ \rq -> rq { rqIsSecure = b }


------------------------------------------------------------------------------
-- | Sets the test request's http version
setHttpVersion :: Monad m => (Int,Int) -> RequestBuilder m ()
setHttpVersion v = rModify $ \rq -> rq { rqVersion = v }


------------------------------------------------------------------------------
-- | Sets the request's path. The path provided must begin with a \"@/@\" and
-- must /not/ contain a query string; if you want to provide a query string
-- in your test request, you must use 'setQueryString' or 'setQueryStringRaw'.
-- Note that 'rqContextPath' is never set by any 'RequestBuilder' function.
setRequestPath :: Monad m => ByteString -> RequestBuilder m ()
setRequestPath p0 = do
    rModify $ \rq -> rq { rqContextPath = "/"
                        , rqPathInfo    = p }
    fixupURI

  where
    p = if S.isPrefixOf "/" p0 then S.drop 1 p0 else p0


------------------------------------------------------------------------------
-- | Builds an HTTP \"GET\" request with the given query parameters.
get :: MonadIO m =>
       ByteString               -- ^ request path
    -> Params                   -- ^ request's form parameters
    -> RequestBuilder m ()
get uri params = do
    setRequestType GetRequest
    setQueryString params
    setRequestPath uri


------------------------------------------------------------------------------
-- | Builds an HTTP \"DELETE\" request with the given query parameters.
delete :: MonadIO m =>
          ByteString            -- ^ request path
       -> Params                -- ^ request's form parameters
       -> RequestBuilder m ()
delete uri params = do
    setRequestType DeleteRequest
    setQueryString params
    setRequestPath uri


------------------------------------------------------------------------------
-- | Builds an HTTP \"POST\" request with the given form parameters, using the
-- \"application/x-www-form-urlencoded\" MIME type.
postUrlEncoded :: MonadIO m =>
                  ByteString    -- ^ request path
               -> Params        -- ^ request's form parameters
               -> RequestBuilder m ()
postUrlEncoded uri params = do
    setRequestType $ UrlEncodedPostRequest params
    setRequestPath uri


------------------------------------------------------------------------------
-- | Builds an HTTP \"POST\" request with the given form parameters, using the
-- \"form-data/multipart\" MIME type.
postMultipart :: MonadIO m =>
                 ByteString        -- ^ request path
              -> MultipartParams   -- ^ multipart form parameters
              -> RequestBuilder m ()
postMultipart uri params = do
    setRequestType $ MultipartPostRequest params
    setRequestPath uri


------------------------------------------------------------------------------
-- | Builds an HTTP \"PUT\" request.
put :: MonadIO m =>
       ByteString               -- ^ request path
    -> ByteString               -- ^ request body MIME content-type
    -> ByteString               -- ^ request body contents
    -> RequestBuilder m ()
put uri contentType putData = do
    setRequestType $ RequestWithRawBody PUT putData
    setHeader "Content-Type" contentType
    setRequestPath uri


------------------------------------------------------------------------------
-- | Builds a \"raw\" HTTP \"POST\" request, with the given MIME type and body
-- contents.
postRaw :: MonadIO m =>
           ByteString           -- ^ request path
        -> ByteString           -- ^ request body MIME content-type
        -> ByteString           -- ^ request body contents
        -> RequestBuilder m ()
postRaw uri contentType postData = do
    setRequestType $ RequestWithRawBody POST postData
    setHeader "Content-Type" contentType
    setRequestPath uri


------------------------------------------------------------------------------
-- | Given a web handler in the 'Snap' monad, and a 'RequestBuilder' defining
-- a test request, runs the handler, producing an HTTP 'Response'.
--
-- This function will produce almost exactly the same output as running the
-- handler in a real server, except that chunked transfer encoding is not
-- applied, and the \"Transfer-Encoding\" header is not set (this makes it
-- easier to test response output).
runHandler :: MonadIO m =>
              RequestBuilder m ()   -- ^ a request builder
           -> Snap a                -- ^ a web handler
           -> m Response
runHandler = runHandlerM rs
  where
    rs rq s = do
        (_,rsp) <- liftIO $ run_ $ runSnap s
                                      (const $ return $! ())
                                      (const $ return $! ())
                                      rq

        return rsp


------------------------------------------------------------------------------
-- | Given a web handler in some arbitrary 'MonadSnap' monad, a function
-- specifying how to evaluate it within the context of the test monad, and a
-- 'RequestBuilder' defining a test request, runs the handler, producing an
-- HTTP 'Response'.
runHandlerM :: (MonadIO m, MonadSnap n) =>
               (forall a . Request -> n a -> m Response)
            -- ^ a function defining how the 'MonadSnap' monad should be run
            -> RequestBuilder m ()
            -- ^ a request builder
            -> n b
            -- ^ a web handler
            -> m Response
runHandlerM rSnap rBuilder snap = do
    rq  <- buildRequest rBuilder
    rsp <- rSnap rq snap

    -- simulate server logic
    t1  <- liftIO (epochTime >>= formatHttpTime)
    return $ H.setHeader "Date" t1
           $ H.setHeader "Server" "Snap/test"
           $ if rspContentLength rsp == Nothing &&
                rspHttpVersion rsp < (1,1)
               then H.setHeader "Connection" "close" rsp
               else rsp


------------------------------------------------------------------------------
-- | Given a web handler in the 'Snap' monad, and a 'RequestBuilder' defining a
-- test request, runs the handler and returns the monadic value it produces.
--
-- Throws an exception if the 'Snap' handler early-terminates with 'finishWith'
-- or 'mzero'.
--
evalHandler :: MonadIO m =>
               RequestBuilder m ()
            -> Snap a
            -> m a
evalHandler = evalHandlerM rs
  where
    rs rq s = liftIO $ run_
                     $ evalSnap s (const $ return $! ())
                                  (const $ return $! ())
                                  rq


------------------------------------------------------------------------------
-- | Given a web handler in some arbitrary 'MonadSnap' monad, a function
-- specifying how to evaluate it within the context of the test monad, and a
-- 'RequestBuilder' defining a test request, runs the handler, returning the
-- monadic value it produces.
--
-- Throws an exception if the 'Snap' handler early-terminates with 'finishWith'
-- or 'mzero'.
--
evalHandlerM :: (MonadIO m, MonadSnap n) =>
                (forall a . Request -> n a -> m a)  -- ^ a function defining
                                                    -- how the 'MonadSnap'
                                                    -- monad should be run
             -> RequestBuilder m ()                 -- ^ a request builder
             -> n b                                 -- ^ a web handler
             -> m b
evalHandlerM rSnap rBuilder snap = do
    rq <- buildRequest rBuilder
    rSnap rq snap


------------------------------------------------------------------------------
-- | Dumps the given response to stdout.
dumpResponse :: Response -> IO ()
dumpResponse resp = responseToString resp >>= S.putStrLn


------------------------------------------------------------------------------
-- | Converts the given response to a bytestring.
responseToString :: Response -> IO ByteString
responseToString resp = do
    b <- run_ (rspBodyToEnum (rspBody resp) $$
               liftM mconcat consume)

    return $ toByteString $ fromShow resp `mappend` b


------------------------------------------------------------------------------
rGet :: Monad m => RequestBuilder m Request
rGet   = RequestBuilder State.get

rPut :: Monad m => Request -> RequestBuilder m ()
rPut s = RequestBuilder $ State.put s

rModify :: Monad m => (Request -> Request) -> RequestBuilder m ()
rModify f = RequestBuilder $ modify f
