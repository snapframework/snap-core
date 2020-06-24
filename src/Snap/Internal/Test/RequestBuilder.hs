{- Temporary workaround for https://ghc.haskell.org/trac/ghc/ticket/9127 -}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}

{-# LANGUAGE BangPatterns               #-}
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
  , evalHandler
  , evalHandlerM
  , get
  , postMultipart
  , postRaw
  , postUrlEncoded
  , put
  , requestToString
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
import           Control.Monad              (liftM, replicateM, void)
import           Control.Monad.State.Strict (MonadIO (..), MonadState, MonadTrans, StateT, execStateT, modify)
import qualified Control.Monad.State.Strict as State
import           Data.Bits                  (Bits ((.&.), unsafeShiftR))
import qualified Data.ByteString            as S8
import           Data.ByteString.Builder    (Builder, byteString, char8, stringUtf8, toLazyByteString, word8)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.CaseInsensitive       (CI, original)
import qualified Data.Map                   as Map
import qualified Data.Vector                as V
import           Data.Word                  (Word8)
import           Snap.Core                  (Cookie (Cookie), Method (DELETE, GET, HEAD, POST, PUT), MonadSnap, Params, Request (rqContentLength, rqContextPath, rqCookies, rqHeaders, rqHostName, rqIsSecure, rqMethod, rqParams, rqPathInfo, rqPostParams, rqQueryParams, rqQueryString, rqURI, rqVersion), Response, Snap, deleteHeader, formatHttpTime, getHeader, parseUrlEncoded, printUrlEncoded, runSnap)
import           Snap.Internal.Core         (evalSnap, fixupResponse)
import           Snap.Internal.Http.Types   (Request (Request, rqBody), Response (rspBody, rspContentLength), rspBodyToEnum)
import qualified Snap.Internal.Http.Types   as H
import qualified Snap.Types.Headers         as H
import qualified System.IO.Streams          as Streams
import           System.PosixCompat.Time    (epochTime)
import           System.Random              (randomIO)
import           Text.Printf                (printf)
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        (Applicative)
import           Data.Monoid                (Monoid (mappend, mconcat, mempty))
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | RequestBuilder is a monad transformer that allows you to conveniently
-- build a snap 'Request' for testing.
newtype RequestBuilder m a = RequestBuilder (StateT Request m a)
  deriving ( Applicative
           , Functor
           , Monad
#if MIN_VERSION_base(4,13,0)
           , MonadFail
#endif
           , MonadIO
           , MonadState Request
           , MonadTrans
           )


------------------------------------------------------------------------------
mkDefaultRequest :: IO Request
mkDefaultRequest = do
    b <- Streams.fromList $! []
    return $ Request "localhost"
                     "127.0.0.1"
                     60000
                     "127.0.0.1"
                     8080
                     "localhost"
                     False
                     H.empty
                     b
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'get' \"\/foo\/bar\" M.empty
-- GET \/foo\/bar HTTP\/1.1
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- @
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
        fixupHost

    fixupMethod = do
        rq <- rGet
        if (rqMethod rq == GET || rqMethod rq == DELETE ||
            rqMethod rq == HEAD)
          then do
              -- drain the old request body and replace it with a new one
              !_ <- liftIO $ Streams.toList $ rqBody rq
              !b <- liftIO $ Streams.fromList $! []
              -- These requests are not permitted to have bodies
              let rq' = deleteHeader "Content-Type" $
                        rq { rqBody = b }
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
        let !query       = rqQueryString rq
        -- force the stuff from mkDefaultRequest that we just overwrite
        let !_           = rqPostParams rq
        let !_           = rqParams rq
        let !_           = rqQueryParams rq
        let !queryParams = parseUrlEncoded query
        let !mbCT        = getHeader "Content-Type" rq

        (!postParams, rq') <-
            if mbCT == Just "application/x-www-form-urlencoded"
              then liftIO $ do
                  !l <- Streams.toList $ rqBody rq
                  -- snap-server regurgitates the parsed form body
                  !b <- Streams.fromList l
                  return (parseUrlEncoded (S.concat l), rq { rqBody = b })
              else return (Map.empty, rq)
        let !newParams = Map.unionWith (flip (++)) queryParams postParams

        rPut $ rq' { rqParams      = newParams
                   , rqPostParams  = postParams
                   , rqQueryParams = queryParams }

    fixupHost = do
        rq <- rGet
        case H.getHeader "Host" rq of
          Nothing -> do
            let !hn = rqHostName rq
            rPut $ H.setHeader "Host" hn rq
          Just hn ->
            rPut $ rq { rqHostName = hn }


------------------------------------------------------------------------------
-- | A request body of type \"@multipart/form-data@\" consists of a set of
-- named form parameters, each of which can by either a list of regular form
-- values or a set of file uploads.
type MultipartParams = [(ByteString, MultipartParam)]


------------------------------------------------------------------------------
-- | A single \"@multipart/form-data@\" form parameter: either a list of regular
-- form values or a set of file uploads.
data MultipartParam =
    FormData [ByteString]
        -- ^ a form variable consisting of the given 'ByteString' values.
  | Files [FileData]
        -- ^ a file upload consisting of the given 'FileData' values.
  deriving (Show)


------------------------------------------------------------------------------
-- | Represents a single file upload for the 'MultipartParam'.
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'delete' \"\/foo\/bar\" M.empty >> 'setRequestType' GetRequest
-- GET \/foo\/bar HTTP\/1.1
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- @
setRequestType :: MonadIO m => RequestType -> RequestBuilder m ()
setRequestType GetRequest = do
    rq   <- rGet
    body <- liftIO $ Streams.fromList $! []

    rPut $ rq { rqMethod        = GET
              , rqContentLength = Nothing
              , rqBody          = body
              }

setRequestType DeleteRequest = do
    rq   <- rGet
    body <- liftIO $ Streams.fromList $! []

    rPut $ rq { rqMethod        = DELETE
              , rqContentLength = Nothing
              , rqBody          = body
              }

setRequestType (RequestWithRawBody m b) = do
    rq <- rGet
    body <- liftIO $ Streams.fromList $! [ b ]
    rPut $ rq { rqMethod        = m
              , rqContentLength = Just $ fromIntegral $ S.length b
              , rqBody          = body
              }

setRequestType (MultipartPostRequest fp) = encodeMultipart fp

setRequestType (UrlEncodedPostRequest fp) = do
    rq <- liftM (H.setHeader "Content-Type"
                             "application/x-www-form-urlencoded") rGet
    let b = printUrlEncoded fp
    body <- liftIO $ Streams.fromList $! [b]

    rPut $ rq { rqMethod        = POST
              , rqContentLength = Just $!  fromIntegral $ S.length b
              , rqBody          = body
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
                k   = \i -> word8 $! toEnum $! fromEnum $!
                            V.unsafeIndex table (fromEnum i)
            in m `mappend` k hi `mappend` k low


------------------------------------------------------------------------------
multipartHeader :: ByteString -> ByteString -> Builder
multipartHeader boundary name =
    mconcat [ byteString boundary
            , byteString "\r\ncontent-disposition: form-data"
            , byteString "; name=\""
            , byteString name
            , byteString "\"\r\n" ]


------------------------------------------------------------------------------
-- Assume initial or preceding "--" just before this
encodeFormData :: ByteString -> ByteString -> [ByteString] -> IO Builder
encodeFormData boundary name vals =
    case vals of
      []  -> return mempty
      [v] -> return $ mconcat [ hdr
                              , cr
                              , byteString v
                              , byteString "\r\n--" ]
      _   -> multi

  where
    hdr = multipartHeader boundary name
    cr = byteString "\r\n"

    oneVal b v = mconcat [ byteString b
                         , cr
                         , cr
                         , byteString v
                         , byteString "\r\n--" ]

    multi = do
        b <- makeBoundary
        return $ mconcat [ hdr
                         , multipartMixed b
                         , cr
                         , byteString "--"
                         , mconcat (map (oneVal b) vals)
                         , byteString b
                         , byteString "--\r\n--" ]


------------------------------------------------------------------------------
multipartMixed :: ByteString -> Builder
multipartMixed b = mconcat [ byteString "Content-Type: multipart/mixed"
                           , byteString "; boundary="
                           , byteString b
                           , byteString "\r\n" ]


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
                           , byteString "--"
                           , mconcat (map (oneVal b) files)
                           , byteString b
                           , byteString "--\r\n--"
                           ]

  where
    --------------------------------------------------------------------------
    contentDisposition fn = mconcat [
                              byteString "Content-Disposition: attachment"
                            , byteString "; filename=\""
                            , byteString fn
                            , byteString "\"\r\n"
                            ]

    --------------------------------------------------------------------------
    contentType ct = mconcat [
                       byteString "Content-Type: "
                     , byteString ct
                     , cr
                     ]

    --------------------------------------------------------------------------
    oneVal b fd =
        mconcat [ byteString b
                , cr
                , contentType ct
                , contentDisposition fileName
                , byteString "Content-Transfer-Encoding: binary\r\n"
                , cr
                , byteString contents
                , byteString "\r\n--"
                ]
      where
        fileName = fdFileName fd
        ct       = fdContentType fd
        contents = fdContents fd

    --------------------------------------------------------------------------
    hdr = multipartHeader boundary name
    cr  = byteString "\r\n"


------------------------------------------------------------------------------
encodeMultipart :: MonadIO m => MultipartParams -> RequestBuilder m ()
encodeMultipart kvps = do
    boundary <- liftIO $ makeBoundary
    builders <- liftIO $ mapM (handleOne boundary) kvps

    let b = toByteString $ mconcat (byteString "--" : builders)
                             `mappend` finalBoundary boundary

    rq0 <- rGet

    body <- liftIO $ Streams.fromList [b]

    let rq = H.setHeader "Content-Type"
               (S.append "multipart/form-data; boundary=" boundary)
               rq0

    rPut $ rq { rqMethod        = POST
              , rqContentLength = Just $ fromIntegral $ S.length b
              , rqBody          = body
              }


  where
    finalBoundary b = mconcat [byteString b, byteString "--\r\n"]

    handleOne boundary (name, mp) =
        case mp of
          (FormData vals) -> encodeFormData boundary name vals
          (Files fs)      -> encodeFiles boundary name fs


------------------------------------------------------------------------------
fixupURI :: Monad m => RequestBuilder m ()
fixupURI = do
    rq <- rGet
    upd rq $! S.concat [ rqContextPath rq
                       , rqPathInfo rq
                       , let q = rqQueryString rq
                         in if S.null q
                              then ""
                              else S.append "?" q
                       ]
  where
    upd rq !u = let !_ = rqURI rq
                in rPut $ rq { rqURI = u }


------------------------------------------------------------------------------
-- | Sets the request's query string to be the raw bytestring provided,
-- without any escaping or other interpretation. Most users should instead
-- choose the 'setQueryString' function, which takes a parameter mapping.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'get' \"\/foo\/bar\" M.empty >> 'setQueryStringRaw' "param0=baz&param1=qux"
-- GET \/foo\/bar?param0=baz&param1=qux HTTP\/1.1
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- params: param0: ["baz"], param1: ["qux"]
-- @
setQueryStringRaw :: Monad m => ByteString -> RequestBuilder m ()
setQueryStringRaw r = do
    rq <- rGet
    rPut $ rq { rqQueryString = r }
    fixupURI


------------------------------------------------------------------------------
-- | Escapes the given parameter mapping and sets it as the request's query
-- string.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'get' \"\/foo\/bar\" M.empty >> 'setQueryString' (M.fromList [("param0", ["baz"]), ("param1", ["qux"])])
-- GET \/foo\/bar?param0=baz&param1=qux HTTP\/1.1
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- params: param0: ["baz"], param1: ["qux"]
-- @
setQueryString :: Monad m => Params -> RequestBuilder m ()
setQueryString p = setQueryStringRaw $ printUrlEncoded p


------------------------------------------------------------------------------
-- | Sets the given header in the request being built, overwriting any header
-- with the same name already present.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> :{
-- ghci| 'buildRequest' $ do get \"\/foo\/bar\" M.empty
-- ghci|                   'setHeader' \"Accept\" "text\/html"
-- ghci|                   'setHeader' \"Accept\" "text\/plain"
-- ghci| :}
-- GET \/foo\/bar HTTP\/1.1
-- accept: text\/plain
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- @
setHeader :: (Monad m) => CI ByteString -> ByteString -> RequestBuilder m ()
setHeader k v = rModify (H.setHeader k v)


------------------------------------------------------------------------------
-- | Adds the given header to the request being built.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> :{
-- ghci| 'buildRequest' $ do 'get' \"\/foo\/bar\" M.empty
-- ghci|                   'addHeader' \"Accept\" "text\/html"
-- ghci|                   'addHeader' \"Accept\" "text\/plain"
-- ghci| :}
-- GET \/foo\/bar HTTP\/1.1
-- accept: text\/html,text\/plain
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- @
addHeader :: (Monad m) => CI ByteString -> ByteString -> RequestBuilder m ()
addHeader k v = rModify (H.addHeader k v)

------------------------------------------------------------------------------
-- | Adds the given cookies to the request being built.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import "Snap.Core"
-- ghci> let cookie = 'Snap.Core.Cookie' "name" "value" Nothing Nothing Nothing False False
-- ghci> 'buildRequest' $ 'get' \"\/foo\/bar\" M.empty >> 'addCookies' [cookie]
-- GET \/foo\/bar HTTP\/1.1
-- cookie: name=value
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- cookies: Cookie {cookieName = "name", cookieValue = "value", ...}
-- @
addCookies :: (Monad m) => [Cookie] -> RequestBuilder m ()
addCookies cookies = do
    rModify $ \rq -> rq { rqCookies = rqCookies rq ++ cookies }
    allCookies <- liftM rqCookies rGet
    let cstr = map cookieToBS allCookies
    setHeader "Cookie" $ S.intercalate "; " cstr


------------------------------------------------------------------------------
-- | Convert 'Cookie' into 'ByteString' for output.
cookieToBS :: Cookie -> ByteString
cookieToBS (Cookie k v !_ !_ !_ !_ !_) = cookie
  where
    cookie  = S.concat [k, "=", v]


------------------------------------------------------------------------------
-- | Sets the request's @content-type@ to the given MIME type.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'put' \"\/foo\/bar\" "text\/html" "some text" >> 'setContentType' "text\/plain"
-- PUT \/foo\/bar HTTP\/1.1
-- content-type: text\/plain
-- content-length: 9
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=9
-- @
setContentType :: Monad m => ByteString -> RequestBuilder m ()
setContentType c = rModify (H.setHeader "Content-Type" c)


------------------------------------------------------------------------------
-- | Controls whether the test request being generated appears to be an https
-- request or not.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'delete' \"\/foo\/bar\" M.empty >> 'setSecure' True
-- DELETE \/foo\/bar HTTP\/1.1
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a secure
-- @
setSecure :: Monad m => Bool -> RequestBuilder m ()
setSecure b = rModify $ \rq -> rq { rqIsSecure = b }


------------------------------------------------------------------------------
-- | Sets the test request's http version
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'delete' \"\/foo\/bar\" M.empty >> 'setHttpVersion' (1,0)
-- DELETE \/foo\/bar HTTP\/1.0
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- @
setHttpVersion :: Monad m => (Int,Int) -> RequestBuilder m ()
setHttpVersion v = rModify $ \rq -> rq { rqVersion = v }


------------------------------------------------------------------------------
-- | Sets the request's path. The path provided must begin with a \"@/@\" and
-- must /not/ contain a query string; if you want to provide a query string
-- in your test request, you must use 'setQueryString' or 'setQueryStringRaw'.
-- Note that 'rqContextPath' is never set by any 'RequestBuilder' function.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'get' \"\/foo\/bar\" M.empty >> 'setRequestPath' "\/bar\/foo"
-- GET \/bar\/foo HTTP\/1.1
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- @
setRequestPath :: Monad m => ByteString -> RequestBuilder m ()
setRequestPath p0 = do
    rModify $ \rq -> rq { rqContextPath = "/"
                        , rqPathInfo    = p }
    fixupURI

  where
    p = if S.isPrefixOf "/" p0 then S.drop 1 p0 else p0


------------------------------------------------------------------------------
-- | Builds an HTTP \"GET\" request with the given query parameters.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'get' \"\/foo\/bar\" (M.fromList [("param0", ["baz", "quux"])])
-- GET \/foo\/bar?param0=baz&param0=quux HTTP\/1.1
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- params: param0: ["baz","quux"]
-- @
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'delete' \"\/foo\/bar\" M.empty
-- DELETE \/foo\/bar HTTP\/1.1
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=n\/a
-- @
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> 'buildRequest' $ 'postUrlEncoded' \"\/foo\/bar\" (M.fromList [("param0", ["baz", "quux"])])
-- POST \/foo\/bar HTTP\/1.1
-- content-type: application\/x-www-form-urlencoded
-- content-length: 22
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=22
-- params: param0: ["baz","quux"]
-- @
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> 'buildRequest' $ 'postMultipart' \"\/foo\/bar\" [("param0", FormData ["baz", "quux"])]
-- POST \/foo\/bar HTTP\/1.1
-- content-type: multipart\/form-data; boundary=snap-boundary-572334111ec0c05ad4812481e8585dfa
-- content-length: 406
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=406
-- @
postMultipart :: MonadIO m =>
                 ByteString        -- ^ request path
              -> MultipartParams   -- ^ multipart form parameters
              -> RequestBuilder m ()
postMultipart uri params = do
    setRequestType $ MultipartPostRequest params
    setRequestPath uri


------------------------------------------------------------------------------
-- | Builds an HTTP \"PUT\" request.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> 'buildRequest' $ 'put' \"\/foo\/bar\" "text\/plain" "some text"
-- PUT \/foo\/bar HTTP\/1.1
-- content-type: text/plain
-- content-length: 9
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=9
-- @
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> 'buildRequest' $ 'postRaw' \"\/foo\/bar\" "text/plain" "some text"
-- POST \/foo\/bar HTTP\/1.1
-- content-type: text\/plain
-- content-length: 9
-- host: localhost
--
-- sn="localhost" c=127.0.0.1:60000 s=127.0.0.1:8080 ctx=\/ clen=9
-- @
postRaw :: MonadIO m =>
           ByteString           -- ^ request path
        -> ByteString           -- ^ request body MIME content-type
        -> ByteString           -- ^ request body contents
        -> RequestBuilder m ()
postRaw uri contentType postData = do
    setRequestType $ RequestWithRawBody POST postData
    setContentType contentType
    setRequestPath uri


------------------------------------------------------------------------------
-- | Given a web handler in the 'Snap' monad, and a 'RequestBuilder' defining
-- a test request, runs the handler, producing an HTTP 'Response'.
--
-- This function will produce almost exactly the same output as running the
-- handler in a real server, except that chunked transfer encoding is not
-- applied, and the \"Transfer-Encoding\" header is not set (this makes it
-- easier to test response output).
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import "Snap.Core"
-- ghci> 'runHandler' ('get' "foo/bar" M.empty) ('Snap.Core.writeBS' "Hello, world!")
-- HTTP\/1.1 200 OK
-- server: Snap/test
-- date: Thu, 17 Jul 2014 21:03:23 GMT
--
-- Hello, world!
-- @
runHandler :: MonadIO m =>
              RequestBuilder m ()   -- ^ a request builder
           -> Snap a                -- ^ a web handler
           -> m Response
runHandler = runHandlerM rs
  where
    rs rq s = liftIO $ do
        (_,rsp) <- runSnap s (\x -> return $! (x `seq` ()))
                             (\f -> let !_ = f 0 in return $! ())
                             rq
        fixupResponse rq rsp


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
                rqVersion rq < (1,1)
               then H.setHeader "Connection" "close" rsp
               else rsp


------------------------------------------------------------------------------
-- | Given a web handler in the 'Snap' monad, and a 'RequestBuilder' defining a
-- test request, runs the handler and returns the monadic value it produces.
--
-- Throws an exception if the 'Snap' handler early-terminates with
-- 'Snap.Core.finishWith' or 'Control.Monad.mzero'.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import "Control.Monad"
-- ghci> import qualified "Data.Map" as M
-- ghci> import "Snap.Core"
-- ghci> 'evalHandler' ('get' "foo/bar" M.empty) ('Snap.Core.writeBS' "Hello, world!" >> return 42)
-- 42
-- ghci> 'evalHandler' ('get' "foo/bar" M.empty) 'Control.Monad.mzero'
-- *** Exception: No handler for request: failure was pass
-- @
evalHandler :: MonadIO m =>
               RequestBuilder m ()
            -> Snap a
            -> m a
evalHandler = evalHandlerM rs
  where
    rs rq s = liftIO $ evalSnap s (const $ return $! ())
                                  (const $ return $! ())
                                  rq


------------------------------------------------------------------------------
-- | Given a web handler in some arbitrary 'MonadSnap' monad, a function
-- specifying how to evaluate it within the context of the test monad, and a
-- 'RequestBuilder' defining a test request, runs the handler, returning the
-- monadic value it produces.
--
-- Throws an exception if the 'Snap' handler early-terminates with
-- 'Snap.Core.finishWith' or 'Control.Monad.mzero'.
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
-- | Converts the given 'Response' to a bytestring.
--
-- Example:
--
-- @
-- ghci> import "Snap.Core"
-- ghci> 'responseToString' 'Snap.Core.emptyResponse'
-- \"HTTP\/1.1 200 OK\\r\\n\\r\\n\"
-- @
responseToString :: Response -> IO ByteString
responseToString resp = do
    let act = rspBodyToEnum $ rspBody resp

    (listOut, grab) <- Streams.listOutputStream
    void $ act listOut
    builder <- liftM mconcat grab

    return $! toByteString $ fromShow resp `mappend` builder


------------------------------------------------------------------------------
-- | Converts the given 'Request' to a bytestring.
--
-- Since: 1.0.0.0
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> r <- 'buildRequest' $ get \"\/foo\/bar\" M.empty
-- ghci> 'requestToString' r
-- \"GET \/foo\/bar HTTP\/1.1\\r\\nhost: localhost\\r\\n\\r\\n\"
-- @
requestToString :: Request -> IO ByteString
requestToString req0 = do
    (req, is) <- maybeChunk
    body <- liftM S.concat $ Streams.toList is
    return $! toByteString $ mconcat [ statusLine
                                     , mconcat . map oneHeader . H.toList
                                               $ rqHeaders req
                                     , crlf
                                     , byteString body
                                     ]
  where
    maybeChunk = do
        if getHeader "Transfer-Encoding" req0 == Just "chunked"
          then do
              let req = deleteHeader "Content-Length" $
                        req0 { rqContentLength = Nothing }
              is' <- Streams.map chunk $ rqBody req
              out <- eof >>= Streams.appendInputStream is'
              return (req, out)
          else return (req0, rqBody req0)
      where
        chunk s = S.concat [ S.pack $ printf "%x\r\n" (S.length s)
                           , s
                           , "\r\n"
                           ]
        eof = Streams.fromList ["0\r\n\r\n"]

    (v1,v2) = rqVersion req0
    crlf = char8 '\r' `mappend` char8 '\n'
    statusLine = mconcat [ fromShow $ rqMethod req0
                         , char8 ' '
                         , byteString $ rqURI req0
                         , byteString " HTTP/"
                         , fromShow v1
                         , char8 '.'
                         , fromShow v2
                         , crlf
                         ]

    oneHeader (k,v) = mconcat [ byteString $ original k
                              , byteString ": "
                              , byteString v
                              , crlf
                              ]


------------------------------------------------------------------------------
rGet :: Monad m => RequestBuilder m Request
rGet   = RequestBuilder State.get

rPut :: Monad m => Request -> RequestBuilder m ()
rPut s = RequestBuilder $ State.put s

rModify :: Monad m => (Request -> Request) -> RequestBuilder m ()
rModify f = RequestBuilder $ modify f


------------------------------------------------------------------------------
toByteString :: Builder -> ByteString
toByteString = S.concat . L.toChunks . toLazyByteString


------------------------------------------------------------------------------
fromShow :: Show a => a -> Builder
fromShow = stringUtf8 . show
