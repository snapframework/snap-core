{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}

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
-- Headers and HasHeaders typeclass
-- headers are case insensitive
type Headers = Map CIByteString [ByteString]

-- FIXME: should we be able to update headers in Requests?
class HasHeaders a where
    updateHeaders :: (Headers -> Headers) -> a -> a
    headers       :: a -> Headers

------------------------------------------------------------------------------
data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT
               deriving(Show,Read,Eq)


------------------------------------------------------------------------------
type HttpVersion = (Int,Int)

data Cookie = Cookie {
      cookieName    :: !ByteString
    , cookieValue   :: !ByteString
    , cookieExpires :: !(Maybe UTCTime)
    , cookieDomain  :: !(Maybe ByteString)
    , cookiePath    :: !(Maybe ByteString)
} deriving (Eq, Show)

type Params = Map ByteString [ByteString]

------------------------------------------------------------------------------
-- request type
------------------------------------------------------------------------------

-- | FIXME: describe Request

-- N.B.: "localAddr" is the machine address, "serverAddr" is the EXTERNAL
-- address
data Request = Request
    { rqServerName     :: !ByteString
    , rqServerPort     :: !Int
    , rqRemoteAddr     :: !ByteString
    , rqRemotePort     :: !Int
    , rqLocalAddr      :: !ByteString
    , rqLocalPort      :: !Int
    , rqLocalHostname  :: !ByteString
    , rqIsSecure       :: !Bool
    , rqHeaders        :: Headers
    , rqBody           :: forall a . Enumerator a
    , rqContentLength  :: !(Maybe Int)
    , rqMethod         :: !Method
    , rqVersion        :: !HttpVersion
    , rqCookies        :: [Cookie]
    , rqPathInfo       :: !ByteString
    , rqContextPath    :: !ByteString
    , rqURI            :: !ByteString
    , rqQueryString    :: !ByteString
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
    headers = rqHeaders
    updateHeaders f r = r { rqHeaders = f (rqHeaders r) }


------------------------------------------------------------------------------
-- response type
------------------------------------------------------------------------------
data Response = Response
    { rspHeaders       :: Headers
    , rspHttpVersion   :: !HttpVersion

      -- | We will need to inspect the content length no matter what, and
      --   looking up \"content-length\" in the headers and parsing the number
      --   out of the text will be too expensive
    , rspContentLength :: !(Maybe Int)
    , rspBody          :: forall a . Enumerator a
    , rspStatus        :: !Int
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

emptyResponse       :: Response
emptyResponse       = Response Map.empty (1,1) Nothing return 200 "OK"

setResponseBody     :: (forall a . Enumerator a) -> Response -> Response
setResponseBody e r = r { rspBody = e }
{-# INLINE setResponseBody #-}

setResponseStatus   :: Int -> ByteString -> Response -> Response
setResponseStatus s reason r = r { rspStatus=s, rspStatusReason=reason }
{-# INLINE setResponseStatus #-}

filterResponseBody  :: (forall a . Enumerator a -> Enumerator a)
                    -> Response
                    -> Response
filterResponseBody f r = r { rspBody = f (rspBody r) }
{-# INLINE filterResponseBody #-}


setContentType      :: ByteString -> Response -> Response
setContentType ct = updateHeaders f
  where
    f = Map.insert "Content-Type" [ct]
{-# INLINE setContentType #-}


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

------------------------------------------------------------------------------
-- local definitions
fromStr :: String -> ByteString
fromStr = S.pack . map c2w


------------------------------------------------------------------------------
-- private helper functions
toStr :: ByteString -> String
toStr = map w2c . S.unpack

