{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Snap.Internal.Test.RequestBuilder where

  import           Data.ByteString (ByteString)
  import qualified Data.ByteString.Char8 as S
  import           Data.CIByteString (CIByteString)
  import           Control.Arrow (second)
  import           Control.Monad (liftM)
  import           Control.Monad.State (MonadState, StateT, get, put, execStateT)
  import           Control.Monad.Trans (MonadIO(..))
  import           Data.Enumerator (runIteratee, run_, returnI)
  import           Data.Enumerator.List (consume)
  import           Data.IORef (IORef, newIORef, readIORef)
  import qualified Data.Map as Map

  import           Snap.Internal.Http.Types hiding (setHeader)
  import qualified Snap.Internal.Http.Types as H
  import           Snap.Iteratee (enumBS)

  getBody :: Request -> IO ByteString
  getBody request = do
    (SomeEnumerator enum) <- readIORef $ rqBody request
    S.concat `liftM` (runIteratee consume >>= run_ . enum)

  data RequestProduct =
    RequestProduct {
      rqpMethod      :: Method
    , rqpParams      :: Params
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

  processQueryString :: Method -> Params -> ByteString
  processQueryString GET ps = buildQueryString ps
  processQueryString _   _  = ""

  emptyRequestBody :: (MonadIO m) => m (IORef SomeEnumerator)
  emptyRequestBody = liftIO . newIORef . SomeEnumerator $ returnI

  buildRequestBody :: (MonadIO m) => ByteString -> m (IORef SomeEnumerator)
  buildRequestBody content = 
      liftIO . newIORef . SomeEnumerator $ enumBS content

  processRequestBody :: (MonadIO m) => RequestProduct -> m (IORef SomeEnumerator)
  processRequestBody rqp
    | (rqpMethod rqp) == POST && 
      (rqpContentType rqp) == "x-www-form-urlencoded"
      = buildRequestBody . 
        buildQueryString . 
        rqpParams $ rqp

    | (rqpMethod rqp) == PUT
      = maybe emptyRequestBody buildRequestBody $ rqpBody rqp

    | otherwise = emptyRequestBody

  buildRequest :: (MonadIO m) => RequestBuilder m () -> m Request
  buildRequest (RequestBuilder m) = do 
    finalRqProduct <- execStateT m 
                        (RequestProduct GET 
                                        Map.empty 
                                        Nothing 
                                        Map.empty
                                        "x-www-form-urlencoded")
    requestBody    <- processRequestBody finalRqProduct
    return $ Request {
      rqServerName    = "localhost"
    , rqServerPort    = 80
    , rqRemoteAddr    = "127.0.0.1"
    , rqRemotePort    = 80
    , rqLocalAddr     = "127.0.0.1"
    , rqLocalPort     = 80
    , rqLocalHostname = "localhost"
    , rqIsSecure      = False
    , rqHeaders       = (rqpHeaders finalRqProduct)
    , rqBody          = requestBody
    , rqContentLength = Nothing
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

