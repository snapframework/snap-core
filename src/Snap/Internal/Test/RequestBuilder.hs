{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Snap.Internal.Test.RequestBuilder where

  import           Data.Bits ((.&.))
  import           Data.ByteString (ByteString)
  import qualified Data.ByteString.Char8 as S
  import qualified Data.ByteString.Base16 as B16
  import           Data.CIByteString (CIByteString)
  import           Control.Arrow (second)
  import           Control.Monad (liftM)
  import           Control.Monad.State (MonadState, StateT, get, put, execStateT)
  import           Control.Monad.Trans (MonadIO(..))
  import           Data.Enumerator (runIteratee, run_, returnI)
  import           Data.Enumerator.List (consume)
  import           Data.IORef (IORef, newIORef, readIORef)
  import qualified Data.Map as Map
  import           System.Random (randoms, newStdGen)

  import           Snap.Internal.Http.Types hiding (setHeader)
  import qualified Snap.Internal.Http.Types as H
  import           Snap.Iteratee (enumBS)
  import           Snap.Util.FileServe (defaultMimeTypes, fileType)

  getBody :: Request -> IO ByteString
  getBody request = do
    (SomeEnumerator enum) <- readIORef $ rqBody request
    S.concat `liftM` (runIteratee consume >>= run_ . enum)

  type ContentLength = Maybe Int
  type Boundary      = ByteString
  type FileParams    = Map.Map ByteString [(ByteString, ByteString)]

  data RequestProduct =
    RequestProduct {
      rqpMethod      :: Method
    , rqpParams      :: Params
    , rqpFileParams  :: FileParams
    , rqpBody        :: Maybe ByteString
    , rqpHeaders     :: Headers
    , rqpContentType :: ByteString
    }
    deriving (Show)

  instance HasHeaders RequestProduct where
    headers = rqpHeaders
    updateHeaders f rqp = rqp { rqpHeaders = f (rqpHeaders rqp) }

  newtype RequestBuilder m a
    = RequestBuilder (StateT RequestProduct m a)
    deriving (Monad, MonadIO)

  buildQueryString :: Params -> ByteString
  buildQueryString = S.intercalate "&" . Map.foldWithKey helper []
    where 
      helper k vs acc = 
          (map (\v -> S.concat [urlEncode k, "=", urlEncode v]) vs) ++
          acc

  buildMultipartString :: Boundary -> Boundary -> Params -> FileParams -> ByteString
  buildMultipartString boundary fileBoundary params fileParams = 
      S.concat [ simpleParamsString
               , fileParamsString
               , S.concat ["--", boundary, "--"]]
    where
      crlf = "\r\n"
      contentTypeFor = fileType defaultMimeTypes . S.unpack

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

  emptyRequestBody :: (MonadIO m) => m (IORef SomeEnumerator)
  emptyRequestBody = liftIO . newIORef . SomeEnumerator $ returnI

  buildRequestBody :: (MonadIO m) => ByteString -> m (IORef SomeEnumerator)
  buildRequestBody content = 
      liftIO . newIORef . SomeEnumerator $ enumBS content

  buildBoundary :: (MonadIO m) => m Boundary
  buildBoundary = 
    liftM (S.append "snap-boundary-" . 
           B16.encode . 
           S.pack . 
           Prelude.map (toEnum . (.&. 255)) . 
           take 10 . 
           randoms) 
           (liftIO newStdGen)

  processQueryString :: Method -> Params -> ByteString
  processQueryString GET ps = buildQueryString ps
  processQueryString _   _  = ""

  processRequestHeaders :: Maybe Boundary -> RequestProduct -> Headers
  processRequestHeaders Nothing rqp = (rqpHeaders rqp)
  processRequestHeaders (Just boundary) rqp 
    | (rqpContentType rqp) == "multipart/form-data"
      = H.setHeader 
          "Content-Type" 
          ("multipart/form-data; boundary=" `S.append` boundary)
          (rqpHeaders rqp)
    | otherwise = (rqpHeaders rqp)

  processRequestBody :: (MonadIO m) => RequestProduct -> m ( IORef SomeEnumerator 
                                                           , ContentLength
                                                           , Maybe Boundary
                                                           )
  processRequestBody rqp
    | (rqpMethod rqp) == POST && 
      (rqpContentType rqp) == "x-www-form-urlencoded"
      = do
        let qs = buildQueryString (rqpParams rqp)
        requestBody <- buildRequestBody qs
        return ( requestBody
               , Just $ S.length qs
               , Nothing
               )

    | (rqpMethod rqp) == POST &&
      (rqpContentType rqp) == "multipart/form-data"
      = do
        boundary     <- buildBoundary
        fileBoundary <- buildBoundary
        let multipartBody = buildMultipartString 
                              boundary
                              fileBoundary
                              (rqpParams rqp)
                              (rqpFileParams rqp)
        requestBody <- buildRequestBody multipartBody
        return ( requestBody
               , Just $ S.length multipartBody
               , Just $ boundary
               )

    | (rqpMethod rqp) == PUT
      = do 
        requestBody <- maybe emptyRequestBody buildRequestBody (rqpBody rqp)
        return ( requestBody
               , S.length `liftM` (rqpBody rqp)
               , Nothing
               )

    | otherwise = do 
      requestBody <- emptyRequestBody
      return (requestBody, Nothing, Nothing)

  buildRequest :: (MonadIO m) => RequestBuilder m () -> m Request
  buildRequest (RequestBuilder m) = do 
    finalRqProduct <- execStateT m 
                        (RequestProduct GET 
                                        Map.empty 
                                        Map.empty
                                        Nothing 
                                        Map.empty
                                        "x-www-form-urlencoded")
    (requestBody, contentLength, boundary)  <- processRequestBody finalRqProduct
    let requestHeaders = processRequestHeaders boundary finalRqProduct
    return $ Request {
      rqServerName    = "localhost"
    , rqServerPort    = 80
    , rqRemoteAddr    = "127.0.0.1"
    , rqRemotePort    = 80
    , rqLocalAddr     = "127.0.0.1"
    , rqLocalPort     = 80
    , rqLocalHostname = "localhost"
    , rqIsSecure      = False
    , rqHeaders       = requestHeaders
    , rqBody          = requestBody
    , rqContentLength = contentLength
    , rqMethod        = (rqpMethod finalRqProduct)
    , rqVersion       = (1,1)
    , rqCookies       = []
    , rqSnapletPath   = ""
    , rqPathInfo      = ""
    , rqContextPath   = ""
    , rqURI           = ""
    , rqQueryString   = processQueryString (rqpMethod finalRqProduct) 
                                           (rqpParams finalRqProduct)
    , rqParams        = (rqpParams finalRqProduct)
    }

  alterRequestProduct :: (Monad m) => (RequestProduct -> RequestProduct) -> RequestBuilder m ()
  alterRequestProduct fn = RequestBuilder $ get >>= put . fn

  setMethod :: (Monad m) => Method -> RequestBuilder m ()
  setMethod method = alterRequestProduct $ \rqp -> rqp { rqpMethod = method }

  setParam :: (Monad m) => ByteString -> ByteString -> RequestBuilder m ()
  setParam name value = alterRequestProduct helper
    where
      helper rqp = rqp { rqpParams = Map.alter (return . maybe [value] (value:)) name (rqpParams rqp) }

  setParams :: (Monad m) => [(ByteString, ByteString)] -> RequestBuilder m ()
  setParams params = alterRequestProduct $ \rqp -> rqp { rqpParams = params' }
    where
      params' = Map.fromList . map (second (:[])) $ params

  setBody :: (Monad m) => ByteString -> RequestBuilder m ()
  setBody body = alterRequestProduct $ \rqp -> rqp { rqpBody = Just body }

  setHeader :: (Monad m) => CIByteString -> ByteString -> RequestBuilder m ()
  setHeader name body = alterRequestProduct (H.setHeader name body)

  formUrlEncoded :: (Monad m) => RequestBuilder m ()
  formUrlEncoded = do
      let contentType = "x-www-form-urlencoded"
      setHeader "Content-Type"  contentType 
      alterRequestProduct $ \rqp -> rqp { rqpContentType = contentType }
  
  multipartEncoded :: (Monad m) => RequestBuilder m ()
  multipartEncoded = do
      let contentType = "multipart/form-data"
      setHeader "Content-Type" contentType
      alterRequestProduct $ \rqp -> rqp { rqpContentType = contentType }



