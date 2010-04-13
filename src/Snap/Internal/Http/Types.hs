{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Snap.Internal.Http.Types where

import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w,w2c)
import qualified Data.ByteString as S
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Time.Clock
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)

------------------------------------------------------------------------------
import           Data.CIByteString
import qualified Snap.Iteratee as I

------------------------------------------------------------------------------
type Enumerator a = I.Enumerator IO a

------------------------------------------------------------------------------
-- | 'Headers' is a type alias for a case-insensitive key-value mapping.
type Headers = Map CIByteString [ByteString]


-- | 'HasHeaders' is a typeclass for datatypes which contain HTTP headers.
class HasHeaders a where

    -- | Modify the datatype's headers.
    updateHeaders :: (Headers -> Headers) -> a -> a

    -- | Retrieve the headers from a datatype that has headers.
    headers       :: a -> Headers


-- | Add a header key-value-pair to the 'HasHeaders' datatype. If a header with
-- the same name already exists, the new value is appended to the headers list.
addHeader :: (HasHeaders a) => CIByteString -> ByteString -> a -> a
addHeader k v = updateHeaders $ Map.insertWith' (++) k [v]

-- | Set a header key-value-pair in a 'HasHeaders' datatype. If a header with
-- the same name already exists, it is overwritten with the new value.
setHeader :: (HasHeaders a) => CIByteString -> ByteString -> a -> a
setHeader k v = updateHeaders $ Map.insert k [v]

-- | Get a header value out of a 'HasHeaders' datatype.
getHeader :: (HasHeaders a) => CIByteString -> a -> Maybe [ByteString]
getHeader k a = Map.lookup k $ headers a

------------------------------------------------------------------------------

-- | 'Method' enumerates the HTTP method values (see
-- <http://tools.ietf.org/html/rfc2068.html#section-5.1.1>)
data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT
               deriving(Show,Read,Eq)


------------------------------------------------------------------------------
type HttpVersion = (Int,Int)

-- | A datatype representing an HTTP cookie.
data Cookie = Cookie {
      -- | The name of the cookie.
      cookieName    :: !ByteString

      -- | The cookie's string value.
    , cookieValue   :: !ByteString

      -- | The cookie's expiration value, if it has one.
    , cookieExpires :: !(Maybe UTCTime)

      -- | The cookie's \"domain\" value, if it has one.
    , cookieDomain  :: !(Maybe ByteString)

      -- | The cookie path.
    , cookiePath    :: !(Maybe ByteString)
} deriving (Eq, Show)

-- | 'Params' is a type alias for the HTTP parameters mapping. Each parameter
-- key maps to a list of ByteString values; if a parameter is specified
-- multiple times (e.g.: \"@GET /foo?param=bar1&param=bar2@\"), looking up
-- \"@param@\" in the mapping will give you @[\"bar1\", \"bar2\"]@.
type Params = Map ByteString [ByteString]

------------------------------------------------------------------------------
-- request type
------------------------------------------------------------------------------

-- | The 'Request' datatype contains all of the information about an incoming
-- HTTP request.
data Request = Request
    { -- | The server name of the request, as it came in from the request's
      -- @Host:@ header.
      rqServerName     :: !ByteString

      -- | Return the port number the HTTP server is listening on
    , rqServerPort     :: !Int

      -- | The remote IP address
    , rqRemoteAddr     :: !ByteString

      -- | The remote TCP port number.
    , rqRemotePort     :: !Int

      -- | The local IP address for this request.
    , rqLocalAddr      :: !ByteString

      -- | Return the port number the HTTP server is listening on
    , rqLocalPort      :: !Int

      -- | Returns the HTTP server's idea of its local hostname
    , rqLocalHostname  :: !ByteString

      -- | Returns @True@ if this is an @HTTPS@ session (currently always
      -- @False@).
    , rqIsSecure       :: !Bool
    , rqHeaders        :: Headers
    , rqBody           :: forall a . Enumerator a

      -- | Returns the @Content-Length@ of the HTTP request body.
    , rqContentLength  :: !(Maybe Int)

      -- | Returns the HTTP request method.
    , rqMethod         :: !Method

      -- | Returns the HTTP version used by the client.
    , rqVersion        :: !HttpVersion

      -- | Returns a list of the cookies that came in from the HTTP request
      -- headers.
    , rqCookies        :: [Cookie]
 
      -- | Handlers can (/will be; --ed/) be hung on a @URI@ \"entry point\";
      -- this is called the \"context path\". If a handler is hung on the
      -- context path @\"\/foo/\"@, and you request @\"/foo/bar\"@, the value
      -- of 'rqPathInfo' will be \"bar\".
    , rqPathInfo       :: !ByteString

      -- | The \"context path\" of the request; catenating 'rqContextPath' and
      -- 'rqPathInfo' should get you back to the original 'rqURI'.
    , rqContextPath    :: !ByteString

      -- | Returns the @URI@ requested by the client.
    , rqURI            :: !ByteString

      -- | Returns the HTTP query string for this 'Request'.
    , rqQueryString    :: !ByteString

      -- | Returns the 'Params' mapping for this 'Request'. \"Parameters\" are
      -- automatically decoded from the query string and @POST@ body and
      -- entered into this mapping.
    , rqParams         :: Params
    }

instance Show Request where
  show r = concat [ "Request <\n"
                  , body
                  , ">" ]
    where
      body = concat $ map (("    "++) . (++ "\n")) [
                      sname
                    , remote
                    , local
                    , beginheaders
                    , hdrs
                    , endheaders
                    , contentlength
                    , method
                    , version
                    , cookies
                    , pathinfo
                    , contextpath
                    , uri
                    , params
                    ]

      sname         = concat [ "server-name: ", toStr $ rqServerName r ]
      remote        = concat [ "remote: "
                             , toStr $ rqRemoteAddr r
                             , ":"
                             , show (rqRemotePort r)
                             ]
      local         = concat [ "local: "
                             , toStr $ rqLocalAddr r
                             , ":"
                             , show $ rqServerPort r
                             ]
      beginheaders  = "Headers:\n      ========================================"
      endheaders    = "  ========================================"
      hdrs          = "      " ++ show (rqHeaders r)
      contentlength = concat [ "content-length: "
                             , show $ rqContentLength r
                             ]
      method        = concat [ "method: "
                             , show $ rqMethod r
                             ]
      version       = concat [ "version: "
                             , show $ rqVersion r
                             ]
      cookies       = concat [ "cookies:\n"
                             , "      ========================================\n"
                             , "      " ++ (show $ rqCookies r)
                             , "\n      ========================================"
                             ]
      pathinfo      = concat [ "pathinfo: ", toStr $ rqPathInfo r ]
      contextpath   = concat [ "contextpath: ", toStr $ rqContextPath r ]
      uri           = concat [ "URI: ", toStr $ rqURI r ]
      params        = concat [ "params:\n"
                             , "      ========================================\n"
                             , "      " ++ (show $ rqParams r)
                             , "\n      ========================================"
                             ]


instance HasHeaders Request where
    headers           = rqHeaders
    updateHeaders f r = r { rqHeaders = f (rqHeaders r) }


instance HasHeaders Headers where
    headers       = id
    updateHeaders = id

------------------------------------------------------------------------------
-- response type
------------------------------------------------------------------------------

-- | The 'Response' datatype represents an HTTP response.
data Response = Response
    { rspHeaders       :: Headers
    , rspHttpVersion   :: !HttpVersion

      -- | We will need to inspect the content length no matter what, and
      --   looking up \"content-length\" in the headers and parsing the number
      --   out of the text will be too expensive
    , rspContentLength :: !(Maybe Int)
    , rspBody          :: forall a . Enumerator a

      -- | Return the HTTP status code
    , rspStatus        :: !Int

      -- | Return the HTTP status explanation string
    , rspStatusReason  :: !ByteString
    }

instance Show Response where
  show r = concat [ "Response <\n"
                  , body
                  , ">" ]
    where
      body = concat $ map (("    "++) . (++ "\n")) [
                         hdrs
                       , version
                       , status
                       , reason
                       ]

      hdrs    = concat [ "headers:\n"
                       , "      ==============================\n      "
                       , show $ rspHeaders r
                       , "\n      ==============================" ]
                 
      version = concat [ "version: ", show $ rspHttpVersion r ]
      status  = concat [ "status: ", show $ rspStatus r ]
      reason  = concat [ "reason: ", toStr $ rspStatusReason r ]
    

instance HasHeaders Response where
    headers = rspHeaders
    updateHeaders f r = r { rspHeaders = f (rspHeaders r) }



-- | Looks up the value(s) for the given named parameter. Parameters are
-- initially read from the request's query string and the POST body (if the
-- content-type is @application\/x-www-form-urlencoded@). Parameter values can
-- be modified within handlers using "rqModifyParams".
rqParam :: Request              -- ^ HTTP request
        -> ByteString           -- ^ parameter name to look up
        -> Maybe [ByteString]
rqParam rq k = Map.lookup k $ rqParams rq
{-# INLINE rqParam #-}


-- | Modify the parameters mapping (which is a @Map ByteString ByteString@) in
-- a "Request" using the given function.
rqModifyParams :: (Params -> Params) -> Request -> Request
rqModifyParams f r = r { rqParams = p }
  where
    p = f $ rqParams r
{-# INLINE rqModifyParams #-}


-- | Write a key-value pair to the parameters mapping within the given request.
rqSetParam :: ByteString        -- ^ parameter name
           -> [ByteString]      -- ^ parameter values
           -> Request           -- ^ request
           -> Request 
rqSetParam k v = rqModifyParams $ Map.insert k v
{-# INLINE rqSetParam #-}

------------------------------------------------------------------------------
-- responses
------------------------------------------------------------------------------

-- | An empty 'Response'.
emptyResponse       :: Response
emptyResponse       = Response Map.empty (1,1) Nothing return 200 "OK"

-- | Set an HTTP response body to the given 'Enumerator' value.
setResponseBody     :: (forall a . Enumerator a)  -- ^ new response body
                                                  -- enumerator
                    -> Response                   -- ^ response to modify
                    -> Response
setResponseBody e r = r { rspBody = e }
{-# INLINE setResponseBody #-}

-- | Set the HTTP response status.
setResponseStatus   :: Int        -- ^ HTTP response integer code
                    -> ByteString -- ^ HTTP response explanation
                    -> Response   -- ^ Response to be modified
                    -> Response
setResponseStatus s reason r = r { rspStatus=s, rspStatusReason=reason }
{-# INLINE setResponseStatus #-}


-- | Modify a response body.
modifyResponseBody  :: (forall a . Enumerator a -> Enumerator a)
                    -> Response
                    -> Response
modifyResponseBody f r = r { rspBody = f (rspBody r) }
{-# INLINE modifyResponseBody #-}


-- | Set the @Content-Type@ in the 'Response' headers.
setContentType      :: ByteString -> Response -> Response
setContentType ct = updateHeaders f
  where
    f = Map.insert "Content-Type" [ct]
{-# INLINE setContentType #-}


-- | Add an HTTP 'Cookie' to the 'Response' headers.
addCookie :: Cookie            -- ^ cookie value
          -> Response          -- ^ response to modify
          -> Response
addCookie (Cookie k v mbExpTime mbDomain mbPath) = updateHeaders f
  where
    f       = Map.insertWith' (++) "Set-Cookie" [cookie]
    cookie  = S.concat [k, "=", v, path, exptime, domain]
    path    = maybe "" (S.append "; path=") mbPath
    domain  = maybe "" (S.append "; domain=") mbDomain
    exptime = maybe "" (S.append "; expires=" . fmt) mbExpTime
    fmt     = fromStr . formatTime defaultTimeLocale "%a, %d-%b-%Y %H:%M:%S GMT"


-- | A note here: if you want to set the @Content-Length@ for the response,
-- Snap forces you to do it with this function rather than by setting it in the
-- headers; the @Content-Length@ in the headers will be ignored.
--
-- The reason for this is that Snap needs to look up the value of
-- @Content-Length@ for each request, and looking the string value up in the
-- headers and parsing the number out of the text will be too expensive.
--
-- If you don't set a content length in your response, HTTP keep-alive will be
-- disabled for HTTP\/1.0 clients, forcing a @Connection: close@. For HTTP\/1.1
-- clients, Snap will switch to the chunked transfer encoding if
-- @Content-Length@ is not specified.
setContentLength    :: Int -> Response -> Response
setContentLength l r = r { rspContentLength = Just l }
{-# INLINE setContentLength #-}


-- | Remove any @Content-Length@ set in the 'Response'.
clearContentLength :: Response -> Response
clearContentLength r = r { rspContentLength = Nothing }
{-# INLINE clearContentLength #-}

------------------------------------------------------------------------------
-- local definitions
fromStr :: String -> ByteString
fromStr = S.pack . map c2w
{-# INLINE fromStr #-}

------------------------------------------------------------------------------
-- private helper functions
toStr :: ByteString -> String
toStr = map w2c . S.unpack

