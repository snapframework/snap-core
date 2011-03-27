
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Contains the combinators for the easy creation of Snap Requests
module Snap.Internal.Test.RequestBuilder where

-------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as Blaze
import           Data.Bits                ((.&.))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as S
import qualified Data.ByteString.Base16   as B16
import           Data.CIByteString        (CIByteString)
import           Data.Monoid              (mconcat)
import           Control.Arrow            (second)
import           Control.Monad            (liftM)
import           Control.Monad.State      (MonadState, StateT, put, execStateT)
import qualified Control.Monad.State      as State
import           Control.Monad.Trans      (MonadIO(..), liftIO)
import           Data.Enumerator          (runIteratee, run_, returnI)
import           Data.Enumerator.List     (consume)
import           Data.IORef               (IORef, newIORef, readIORef)
import qualified Data.Map                 as Map
import           System.Random            (randoms, newStdGen)
-------------------------------------------------------------------------------
import           Snap.Internal.Http.Types hiding (setContentType, setHeader)
import qualified Snap.Internal.Http.Types as H
import           Snap.Types               (Snap, runSnap)
import           Snap.Iteratee            (enumBS)
import           Snap.Util.FileServe      (defaultMimeTypes, fileType)

-------------------------------------------------------------------------------
-- | A type alias for Content-Lengths of Requests Bodies.
type ContentLength = Maybe Int

-------------------------------------------------------------------------------
-- | A type alias for Boundaries on Multipart Requests.
type Boundary      = ByteString

-------------------------------------------------------------------------------
-- | A type alias for File params, this are files that going to be sent through
-- a Multipart Request.
type FileParams    = Map.Map ByteString [(ByteString, ByteString)]

data RequestType
    = GetRequest
    | RequestWithRawBody Method ByteString
    | MultipartPostRequest FileParams
    | UrlEncodedPostRequest
    | DeleteRequest
    deriving (Show)

-------------------------------------------------------------------------------
-- | A Data type that will hold temporal values that later on will be used
-- to build Snap Request. Is really similar to the Request Data type, the only
-- difference is that it holds Content-Type of the Request, the Body as Text
-- and the File params.
data RequestProduct =
    RequestProduct {
      rqpRequestType :: RequestType
    , rqpParams      :: Params
    , rqpBody        :: Maybe ByteString
    , rqpHeaders     :: Headers
    , rqpContentType :: ByteString
    , rqpIsSecure    :: !Bool
    , rqpURI         :: ByteString
    }
    deriving (Show)

-------------------------------------------------------------------------------
instance HasHeaders RequestProduct where
  headers = rqpHeaders
  updateHeaders f rqp = rqp { rqpHeaders = f (rqpHeaders rqp) }

-------------------------------------------------------------------------------
-- | Utility function that will help get the ByteString Request Body out of
-- the the Request data type, that hold this internally as an
-- @IORef SomeEnumerator@.
class HasBody r where
  getBody :: r -> IO ByteString

instance HasBody Request where
  getBody request = do
      (SomeEnumerator enum) <- readIORef $ rqBody request
      S.concat `liftM` (runIteratee consume >>= run_ . enum)

instance HasBody Response where
  getBody response = do
      let benum = rspBodyToEnum $ rspBody response
      liftM (Blaze.toByteString . mconcat) (runIteratee consume >>= run_ . benum)

-------------------------------------------------------------------------------
-- Request Body Content Builders
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | This will recieve a @Params@ map and transform it into an encoded Query
-- String.
buildQueryString :: Params -- ^ Parameters that will be turn into a QS
                 -> ByteString
buildQueryString = S.intercalate "&" . Map.foldWithKey helper []
  where
    helper k vs acc =
        (map (\v -> S.concat [urlEncode k, "=", urlEncode v]) vs) ++
        acc

-------------------------------------------------------------------------------
-- | This will recieve a @Params@ map, a @FileParams@ map and some randomly
-- generated boundaries to create a Multipart Request Body.
buildMultipartString :: Boundary     -- ^ Params Boundary
                     ->   Boundary   -- ^ FileParams Boundary
                     ->   Params     -- ^ Params map
                     ->   FileParams -- ^ FileParams map
                     ->   ByteString
buildMultipartString boundary fileBoundary params fileParams =
    S.concat [ simpleParamsString
             , fileParamsString
             , S.concat ["--", boundary, "--"]]
  where
    crlf = "\r\n"
    contentTypeFor = fileType defaultMimeTypes . S.unpack

    ---------------------------------------------------------------------------
    -- Builds a Multipart Body for @Params@
    simpleParamsString = S.concat $ Map.foldWithKey spHelper [] params
    spHelper k vs acc  = (S.concat $ map (spPair k) vs) : acc
    spPair k v =
        S.concat [ "--"
                 , boundary
                 , crlf
                 , "Content-Disposition: "
                 , "form-data; "
                 , "name=\""
                 , k
                 , "\""
                 , crlf
                 , crlf
                 , v
                 , crlf
                 ]

    ---------------------------------------------------------------------------
    -- Build a Multipart Body for @FileParams@
    fileParamsString = S.concat $ Map.foldWithKey fpHelper [] fileParams
    fpHelper k [(fname, fcontent)] acc =
        (:acc) $ S.concat [
                   "--"
                 , boundary
                 , crlf
                 , "Content-Disposition: form-data; "
                 , "name=\""
                 , k
                 , "\"; filename=\""
                 , fname
                 , "\""
                 , crlf
                 , "Content-Type: "
                 , contentTypeFor fname
                 , crlf
                 , crlf
                 , fcontent
                 , crlf
                 ]
    fpHelper k vs acc =
        (:acc) $ S.concat [
                   "--"
                 , boundary
                 , crlf
                 , "Content-Disposition: form-data; name=\""
                 , k
                 , "\""
                 , crlf
                 , "Content-Type: multipart/mixed; boundary="
                 , fileBoundary
                 , crlf
                 , crlf
                 ] `S.append`
                 S.concat (map (fpPair k) vs) `S.append`
                 S.concat ["--", fileBoundary, "--", crlf]
    fpPair k (fname, fcontent) =
        S.concat [
          "--"
        , fileBoundary
        , crlf
        , "Content-Disposition: "
        , k
        , "; filename=\""
        , fname
        , "\""
        , crlf
        , "Content-Type: "
        , contentTypeFor fname
        , crlf
        , crlf
        , fcontent
        , crlf
        ]



-------------------------------------------------------------------------------
-- Builds an empty Request Body enumerator.
emptyRequestBody :: (MonadIO m) => m (IORef SomeEnumerator)
emptyRequestBody = liftIO . newIORef . SomeEnumerator $ returnI

-------------------------------------------------------------------------------
-- Builds a Request Body enumerator containing the given @ByteString@ content.
buildRequestBody :: (MonadIO m)
                 => ByteString
                 -> m (IORef SomeEnumerator)
buildRequestBody content =
    liftIO . newIORef . SomeEnumerator $ enumBS content

-------------------------------------------------------------------------------
-- Builds a Random boundary that will be used for multipart Requests.
buildBoundary :: (MonadIO m) => m Boundary
buildBoundary =
    liftM (S.append "snap-boundary-" .
           B16.encode .
           S.pack .
           Prelude.map (toEnum . (.&. 255)) .
           take 10 .
           randoms)
           (liftIO newStdGen)

-------------------------------------------------------------------------------
-- Request Procesors
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Builds a Query String from the @RequestPart@.
processQueryString :: Method
                   -> Params
                   -> ByteString
processQueryString GET ps = buildQueryString ps
processQueryString _   _  = ""


-------------------------------------------------------------------------------
-- Builds a Request URI from a @RequestPart@, this function handles
-- adding a Query String to the URI if method is @GET@.
processRequestURI :: RequestProduct
                  -> ByteString
processRequestURI rqp =
    case (rqpRequestType rqp) of
      GetRequest
        | (Map.null (rqpParams rqp)) -> (rqpURI rqp)
        | otherwise -> S.concat [ (rqpURI rqp)
                                , "?"
                                , buildQueryString (rqpParams rqp)
                                ]
      _   -> rqpURI rqp

-------------------------------------------------------------------------------
-- Builds the Request Headers from a @RequestPart@, the sole purpose of
-- this function is to alter the Content-Type header to add a Boundary
-- if it is a multipart/form-data.
processRequestHeaders :: Maybe Boundary
                      -> RequestProduct
                      -> Headers
processRequestHeaders Nothing rqp = (rqpHeaders rqp)
processRequestHeaders (Just boundary) rqp
  | (rqpContentType rqp) == "multipart/form-data"
    = H.setHeader
        "Content-Type"
        ("multipart/form-data; boundary=" `S.append` boundary)
        (rqpHeaders rqp)
  | otherwise = (rqpHeaders rqp)

-------------------------------------------------------------------------------
-- Given a @RequestProduct@, it gets all the info it can out of it to build
-- a Request Body Enumerator, using the body content builder functions and the
-- body enumerator builder functions. It takes into consideration the method
-- and the Content-Type of the Request to build the appropiate Request Body.
-- This function will return the Body enumerator, the Content Length and the
-- Boundary used in case this is a Multipart Request.
processRequestBody :: (MonadIO m)
                   => RequestProduct
                   -> m ( IORef SomeEnumerator
                        , ContentLength
                        , Maybe Boundary
                        )
processRequestBody rqp =
  case (rqpRequestType rqp) of

    UrlEncodedPostRequest -> do
        let qs = buildQueryString (rqpParams rqp)
        requestBody <- buildRequestBody qs
        return ( requestBody
               , Just $ S.length qs
               , Nothing
               )

    (MultipartPostRequest fileParams) -> do
      boundary     <- buildBoundary
      fileBoundary <- buildBoundary
      let multipartBody = buildMultipartString
                            boundary
                            fileBoundary
                            (rqpParams rqp)
                            fileParams
      requestBody <- buildRequestBody multipartBody
      return ( requestBody
             , Just $ S.length multipartBody
             , Just $ boundary
             )

    (RequestWithRawBody _ body) -> do
      requestBody <- buildRequestBody body
      return ( requestBody
             , S.length `liftM` (rqpBody rqp)
             , Nothing
             )

    _ -> do
      requestBody <- emptyRequestBody
      return (requestBody, Nothing, Nothing)


-------------------------------------------------------------------------------
processRequestMethod :: RequestProduct -> Method
processRequestMethod rqp =
    case (rqpRequestType rqp) of
      GetRequest             -> GET
      RequestWithRawBody m _ -> m
      MultipartPostRequest _ -> POST
      UrlEncodedPostRequest  -> POST
      DeleteRequest          -> DELETE


-------------------------------------------------------------------------------
-- | RequestBuilder is the Monad that will hold all the different combinators
-- to build in a simple way, all the Snap Request you will use for testing
-- suite of your Snap App.
newtype RequestBuilder m a
  = RequestBuilder (StateT RequestProduct m a)
  deriving (Monad, MonadIO)

-------------------------------------------------------------------------------
-- | This function will be the responsable of building Snap Request from the
-- RequestBuilder combinators, this Request is the one that will be used to
-- perform Snap handlers.
buildRequest :: (MonadIO m) => RequestBuilder m () -> m Request
buildRequest (RequestBuilder m) = do
    finalRqProduct <- execStateT m
                        (RequestProduct GetRequest
                                        Map.empty
                                        Nothing
                                        Map.empty
                                        "x-www-form-urlencoded"
                                        False
                                        "")

    (requestBody, contentLength, boundary) <- processRequestBody finalRqProduct
    let requestURI     = processRequestURI finalRqProduct
    let requestHeaders = processRequestHeaders boundary finalRqProduct
    let requestMethod  = processRequestMethod finalRqProduct

    return $ Request {
          rqServerName    = "localhost"
        , rqServerPort    = 80
        , rqRemoteAddr    = "127.0.0.1"
        , rqRemotePort    = 80
        , rqLocalAddr     = "127.0.0.1"
        , rqLocalPort     = 80
        , rqLocalHostname = "localhost"
        , rqIsSecure      = (rqpIsSecure finalRqProduct)
        , rqHeaders       = requestHeaders
        , rqBody          = requestBody
        , rqContentLength = contentLength
        , rqMethod        = requestMethod
        , rqVersion       = (1,1)
        , rqCookies       = []
        , rqSnapletPath   = ""
        , rqPathInfo      = (rqpURI finalRqProduct)
        , rqContextPath   = ""
        , rqURI           = requestURI
        , rqQueryString   = processQueryString requestMethod
                                               (rqpParams finalRqProduct)
        , rqParams        = (rqpParams finalRqProduct)
        }


-------------------------------------------------------------------------------
alterRequestProduct :: (Monad m)
                    => (RequestProduct -> RequestProduct)
                    -> RequestBuilder m ()
alterRequestProduct fn = RequestBuilder $ State.get >>= put . fn


-------------------------------------------------------------------------------
setRequestType :: (Monad m)
               => RequestType
               -> RequestBuilder m ()
setRequestType requestType =
    alterRequestProduct $ \rqp -> rqp { rqpRequestType = requestType }

-------------------------------------------------------------------------------
-- Allows you to add a value to an existing parameter. This will not replace
-- the value but add one instead.
addParam :: (Monad m) => ByteString -> ByteString -> RequestBuilder m ()
addParam name value = alterRequestProduct helper
  where
    helper rqp
        = rqp {
          rqpParams = Map.alter (return . maybe [value] (value:))
                                name
                                (rqpParams rqp)
        }


-------------------------------------------------------------------------------
-- Allows you to set a List of key-value pairs, this will be later used by the
-- Request as the parameters for the Snap Handler.
setParams :: (Monad m) => [(ByteString, ByteString)] -> RequestBuilder m ()
setParams params = alterRequestProduct $ \rqp -> rqp { rqpParams = params' }
  where
    params' = Map.fromList . map (second (:[])) $ params

-------------------------------------------------------------------------------
-- Changes the 'Method' of the 'Request' to the given type, and sets the
-- contents request body to the string you supply. This probably only makes
-- sense for 'PUT' requests.
setRequestBody :: (Monad m) => Method -> ByteString -> RequestBuilder m ()
setRequestBody m body = setRequestType (RequestWithRawBody m body)

-------------------------------------------------------------------------------
-- Allows to set a HTTP Header into the Snap Request.
--
-- Usage:
--
--     > response <- runHandler myHandler $ do
--     >               setHeader "Accepts" "application/json"
--     >               setHeader "X-Forwaded-For" "127.0.0.1"
--
setHeader :: (Monad m) => CIByteString -> ByteString -> RequestBuilder m ()
setHeader name body = alterRequestProduct (H.setHeader name body)

-------------------------------------------------------------------------------
addHeader :: (Monad m) => CIByteString -> ByteString -> RequestBuilder m ()
addHeader name body = alterRequestProduct (H.addHeader name body)

-------------------------------------------------------------------------------
setContentType :: (Monad m) => ByteString -> RequestBuilder m ()
setContentType contentType = do
    alterRequestProduct $ \rqp -> rqp { rqpContentType = contentType }
    setHeader "Content-Type" contentType

-------------------------------------------------------------------------------
-- Sets the Content-Type to x-www-form-urlencoded, this is the default.
formUrlEncoded :: (Monad m)
               => Method
               -> RequestBuilder m ()
formUrlEncoded method
  | method == GET  = do
    setContentType contentType
    setRequestType GetRequest
  | method == POST = do
    setContentType contentType
    setRequestType UrlEncodedPostRequest
  | otherwise =
      error $ "Can't set a UrlEncoded Request with method " ++ (show method)
  where
    contentType = "x-www-form-urlencoded"


-------------------------------------------------------------------------------
-- Sets the Content-Type tp multipart/form-data, useful when submitting Files.
multipartEncoded :: (Monad m)
                 => [(ByteString, (ByteString, ByteString))]
                 -> RequestBuilder m ()
multipartEncoded fileParams = do
    setContentType "multipart/form-data"
    let fileParams' = Map.fromList . map (second (:[])) $ fileParams
    setRequestType (MultipartPostRequest fileParams')

-------------------------------------------------------------------------------
-- Allows to set the Request as one using the HTTPS protocol.
useHttps :: (Monad m) => RequestBuilder m ()
useHttps = alterRequestProduct $ \rqp -> rqp { rqpIsSecure = True }

-------------------------------------------------------------------------------
-- Sets the URI that will address the Request in the different handlers.
setURI :: (Monad m) => ByteString -> RequestBuilder m ()
setURI uri = alterRequestProduct $ \rqp -> rqp { rqpURI = uri }

-------------------------------------------------------------------------------
-- Utility function that allows to create a GET request, using the parameters
-- given by the list of key-values. The Content-Type of the Request will be
-- x-www-form-urlencoded.
--
-- Usage:
--
--     > response <- runHandler $ do
--     >               get "/posts" [("ordered", "1")]
--     >               setHeader "Accepts" "application/json"
--     >
--
get :: (Monad m) => ByteString ->
                    [(ByteString, ByteString)] ->
                    RequestBuilder m ()
get uri params = do
  formUrlEncoded GET
  setURI uri
  setParams params


-------------------------------------------------------------------------------
-- Allows the creation of POST requests, using the parameters provided by the
-- list of key-values. The Content-Type of the Request will be
-- x-www-form-urlencoded.
--
-- Usage:
--
--     > response <- runHandler handler $ do
--     >               postUrlEncoded "/authenticate" [ ("login", "john@doe.com")
--     >                                              , ("password", "secret")
--     >                                              ]
--     >               setHeader "Accepts" "application/json"
--     >
--
postUrlEncoded :: (Monad m)
               => ByteString
               -> [(ByteString, ByteString)]
               -> RequestBuilder m ()
postUrlEncoded uri params = do
    formUrlEncoded POST
    setURI uri
    setParams params

-------------------------------------------------------------------------------
-- Allows the creation of POST request, that will include normal HTTP
-- parameters and File parameters, using the multipart/form-data Content-Type.
--
-- Usage:
--
--     > photoContent <- ByteString.readFile "photo.jpg"
--     > response     <- runHandler handler $ do
--     >                   postMultipart "/picture/upload"
--     >                                 []
--     >                                 [("photo", ("photo.jpg", photoContent))]
--     >
postMultipart :: (Monad m) => ByteString ->
                              [(ByteString, ByteString)] ->
                              [(ByteString, (ByteString, ByteString))] ->
                              RequestBuilder m ()
postMultipart uri params fileParams = do
  multipartEncoded fileParams
  setURI uri
  setParams params

-------------------------------------------------------------------------------
-- Request Runner
-------------------------------------------------------------------------------

runHandler :: (MonadIO m) => Snap a -> RequestBuilder m () -> m Response
runHandler handler requestSpec = do
  request       <- buildRequest requestSpec
  (_, response) <- liftIO $ run_ $ runSnap handler
                                    (const $ return ())
                                    (const $ return ())
                                    request
  return response

