{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Snap.Internal.Test.RequestBuilder where

  import           Data.ByteString (ByteString)
  import qualified Data.ByteString.Char8 as S
  import           Control.Arrow (second)
  import           Control.Monad (liftM)
  import           Control.Monad.State (MonadState, StateT, get, put, execStateT)
  import           Control.Monad.Trans (MonadIO(..))
  import           Data.Enumerator (runIteratee, run_, returnI)
  import           Data.Enumerator.List (consume)
  import           Data.IORef (newIORef, readIORef)
  import qualified Data.Map as Map

  import           Snap.Internal.Http.Types 
  import           Snap.Iteratee (enumBS)

  getBody :: Request -> IO ByteString
  getBody request = do
    (SomeEnumerator enum) <- readIORef $ rqBody request
    S.concat `liftM` (runIteratee consume >>= run_ . enum)

  data RequestProduct =
    RequestProduct {
      rqpMethod :: Method
    , rqpParams :: Params
    , rqpBody   :: Maybe ByteString
    }
    deriving (Show)

  newtype RequestBuilder m a
    = RequestBuilder (StateT RequestProduct m a)
    deriving (Monad, MonadIO)

  buildRequest :: (MonadIO m) => RequestBuilder m () -> m Request
  buildRequest (RequestBuilder m) = do 
    finalRqProduct <- execStateT m (RequestProduct GET Map.empty Nothing)
    requestBody    <- liftIO . newIORef $ SomeEnumerator (maybe returnI enumBS (rqpBody finalRqProduct))
    return $ Request {
      rqServerName    = "localhost"
    , rqServerPort    = 80
    , rqRemoteAddr    = "127.0.0.1"
    , rqRemotePort    = 80
    , rqLocalAddr     = "127.0.0.1"
    , rqLocalPort     = 80
    , rqLocalHostname = "localhost"
    , rqIsSecure      = False
    , rqHeaders       = Map.empty
    , rqBody          = requestBody
    , rqContentLength = Nothing
    , rqMethod        = (rqpMethod finalRqProduct)
    , rqVersion       = (1,1)
    , rqCookies       = []
    , rqSnapletPath   = ""
    , rqPathInfo      = ""
    , rqContextPath   = ""
    , rqURI           = ""
    , rqQueryString   = ""
    , rqParams        = (rqpParams finalRqProduct)
    }

  alterRequestProduct :: (Monad m) => (RequestProduct -> RequestProduct) -> RequestBuilder m ()
  alterRequestProduct fn = RequestBuilder $ get >>= put . fn

  httpMethod :: (Monad m) => Method -> RequestBuilder m ()
  httpMethod method = alterRequestProduct $ \rqp -> rqp { rqpMethod = method }

  setParam :: (Monad m) => ByteString -> ByteString -> RequestBuilder m ()
  setParam name value = alterRequestProduct helper
    where
      helper rqp = rqp { rqpParams = Map.alter (return . maybe [value] (value:)) name (rqpParams rqp) }

  setParams :: (Monad m) => [(ByteString, ByteString)] -> RequestBuilder m ()
  setParams params = alterRequestProduct $ \rqp -> rqp { rqpParams = params' }
    where
      params' = Map.fromList . map (second (:[])) $ params

  httpBody :: (Monad m) => ByteString -> RequestBuilder m ()
  httpBody body = alterRequestProduct $ \rqp -> rqp { rqpBody = Just body }

