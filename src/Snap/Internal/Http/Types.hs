-- | An internal Snap module containing HTTP types.
--
-- /N.B./ this is an internal interface, please don't write user code that
-- depends on it. Most of these declarations (except for the
-- unsafe/encapsulation-breaking ones) are re-exported from "Snap.Types".

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Snap.Internal.Http.Types where

import           Control.Applicative hiding (empty)
import           Control.Monad (liftM, when)
import qualified Data.Attoparsec as Atto
import           Data.Attoparsec hiding (many, Result(..))
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w,w2c)
import qualified Data.ByteString.Nums.Careless.Hex as Cvt
import qualified Data.ByteString as S
import           Data.Char
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Serialize.Builder
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Word
import           Foreign hiding (new)
import           Foreign.C.String
import           Foreign.C.Types
import           Prelude hiding (take)
import           System.Locale (defaultTimeLocale)

------------------------------------------------------------------------------
import           Data.CIByteString
import qualified Snap.Iteratee as I


foreign import ccall unsafe "set_c_locale"
        set_c_locale :: IO ()

foreign import ccall unsafe "c_parse_http_time"
        c_parse_http_time :: CString -> IO CTime

foreign import ccall unsafe "c_format_http_time"
        c_format_http_time :: CTime -> CString -> IO ()

------------------------------------------------------------------------------
type Enumerator a = I.Enumerator IO a

------------------------------------------------------------------------------
-- | A type alias for a case-insensitive key-value mapping.
type Headers = Map CIByteString [ByteString]


-- | A typeclass for datatypes which contain HTTP headers.
class HasHeaders a where

    -- | Modify the datatype's headers.
    updateHeaders :: (Headers -> Headers) -> a -> a

    -- | Retrieve the headers from a datatype that has headers.
    headers       :: a -> Headers


-- | Adds a header key-value-pair to the 'HasHeaders' datatype. If a header with
-- the same name already exists, the new value is appended to the headers list.
addHeader :: (HasHeaders a) => CIByteString -> ByteString -> a -> a
addHeader k v = updateHeaders $ Map.insertWith' (++) k [v]

-- | Sets a header key-value-pair in a 'HasHeaders' datatype. If a header with
-- the same name already exists, it is overwritten with the new value.
setHeader :: (HasHeaders a) => CIByteString -> ByteString -> a -> a
setHeader k v = updateHeaders $ Map.insert k [v]

-- | Gets all of the values for a given header.
getHeaders :: (HasHeaders a) => CIByteString -> a -> Maybe [ByteString]
getHeaders k a = Map.lookup k $ headers a

-- | Gets a header value out of a 'HasHeaders' datatype. If many headers came in
-- with the same name, they will be catenated together.
getHeader :: (HasHeaders a) => CIByteString -> a -> Maybe ByteString
getHeader k a = liftM (S.intercalate " ") (Map.lookup k $ headers a)



------------------------------------------------------------------------------

-- | Enumerates the HTTP method values (see
-- <http://tools.ietf.org/html/rfc2068.html#section-5.1.1>).
data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT
               deriving(Show,Read,Ord,Eq)


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

-- | A type alias for the HTTP parameters mapping. Each parameter
-- key maps to a list of ByteString values; if a parameter is specified
-- multiple times (e.g.: \"@GET /foo?param=bar1&param=bar2@\"), looking up
-- \"@param@\" in the mapping will give you @[\"bar1\", \"bar2\"]@.
type Params = Map ByteString [ByteString]

------------------------------------------------------------------------------
-- request type
------------------------------------------------------------------------------

data SomeEnumerator = SomeEnumerator (forall a . Enumerator a)

-- | Contains all of the information about an incoming HTTP request.
data Request = Request
    { -- | The server name of the request, as it came in from the request's
      -- @Host:@ header.
      rqServerName     :: !ByteString

      -- | Returns the port number the HTTP server is listening on.
    , rqServerPort     :: !Int

      -- | The remote IP address.
    , rqRemoteAddr     :: !ByteString

      -- | The remote TCP port number.
    , rqRemotePort     :: !Int

      -- | The local IP address for this request.
    , rqLocalAddr      :: !ByteString

      -- | Returns the port number the HTTP server is listening on.
    , rqLocalPort      :: !Int

      -- | Returns the HTTP server's idea of its local hostname.
    , rqLocalHostname  :: !ByteString

      -- | Returns @True@ if this is an @HTTPS@ session (currently always
      -- @False@).
    , rqIsSecure       :: !Bool
    , rqHeaders        :: Headers
    , rqBody           :: IORef SomeEnumerator

      -- | Returns the @Content-Length@ of the HTTP request body.
    , rqContentLength  :: !(Maybe Int)

      -- | Returns the HTTP request method.
    , rqMethod         :: !Method

      -- | Returns the HTTP version used by the client.
    , rqVersion        :: !HttpVersion

      -- | Returns a list of the cookies that came in from the HTTP request
      -- headers.
    , rqCookies        :: [Cookie]


      -- | We'll be doing web components (or \"snaplets\") for version 0.2. The
      -- \"snaplet path\" refers to the place on the URL where your containing
      -- snaplet is hung. The value of 'rqSnapletPath' is either @\"\"@ (at the
      -- top-level context) or is a path beginning with a slash, but not ending
      -- with one.
      --
      -- An identity is that:
      --
      -- @rqURI r == 'S.concat' [rqSnapletPath r, rqContextPath r, rqPathInfo r]@
      --
      -- note that until we introduce snaplets in v0.2, 'rqSnapletPath' will be
      -- \"\"
    , rqSnapletPath    :: !ByteString

      -- | Handlers can (/will be; --ed/) be hung on a @URI@ \"entry point\";
      -- this is called the \"context path\". If a handler is hung on the
      -- context path @\"\/foo\/\"@, and you request @\"\/foo\/bar\"@, the value
      -- of 'rqPathInfo' will be @\"bar\"@.
    , rqPathInfo       :: !ByteString

      -- | The \"context path\" of the request; catenating 'rqContextPath', and
      -- 'rqPathInfo' should get you back to the original 'rqURI'. The
      -- 'rqContextPath' always begins and ends with a slash (@\"\/\"@)
      -- character, and represents the path (relative to your
      -- component\/snaplet) you took to get to your handler.
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
                    , snapletpath
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
      snapletpath   = concat [ "snapletpath: ", toStr $ rqSnapletPath r ]
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

data ResponseBody = Enum (forall a . Enumerator a) -- ^ output body is enumerator
                  | SendFile FilePath              -- ^ output body is sendfile()

rspBodyMap :: (forall a . Enumerator a -> Enumerator a)
           -> ResponseBody
           -> ResponseBody
rspBodyMap f b      = Enum $ f $ rspBodyToEnum b


rspBodyToEnum :: ResponseBody -> Enumerator a
rspBodyToEnum (Enum e) = e
rspBodyToEnum (SendFile fp) = I.enumFile fp


-- | Represents an HTTP response.
data Response = Response
    { rspHeaders       :: Headers
    , rspHttpVersion   :: !HttpVersion

      -- | We will need to inspect the content length no matter what, and
      --   looking up \"content-length\" in the headers and parsing the number
      --   out of the text will be too expensive.
    , rspContentLength :: !(Maybe Int)
    , rspBody          :: ResponseBody

      -- | Returns the HTTP status code.
    , rspStatus        :: !Int

      -- | Returns the HTTP status explanation string.
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



-- | Looks up the value(s) for the given named parameter. Parameters initially
-- come from the request's query string and any decoded POST body (if the
-- request's @Content-Type@ is @application\/x-www-form-urlencoded@). Parameter
-- values can be modified within handlers using "rqModifyParams".
rqParam :: ByteString           -- ^ parameter name to look up
        -> Request              -- ^ HTTP request
        -> Maybe [ByteString]
rqParam k rq = Map.lookup k $ rqParams rq
{-# INLINE rqParam #-}


-- | See 'rqParam'. Looks up a value for the given named parameter. If more
-- than one value was entered for the given parameter name, 'getParam' gloms
-- the values together with:
--
-- @    'S.intercalate' \" \"@
--
getParam :: ByteString          -- ^ parameter name to look up
         -> Request             -- ^ HTTP request
         -> Maybe ByteString
getParam k rq = liftM (S.intercalate " ") $ rqParam k rq


-- | Modifies the parameters mapping (which is a @Map ByteString ByteString@) in
-- a 'Request' using the given function.
rqModifyParams :: (Params -> Params) -> Request -> Request
rqModifyParams f r = r { rqParams = p }
  where
    p = f $ rqParams r
{-# INLINE rqModifyParams #-}


-- | Writes a key-value pair to the parameters mapping within the given request.
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
emptyResponse       = Response Map.empty (1,1) Nothing (Enum return) 200 "OK"

-- | Sets an HTTP response body to the given 'Enumerator' value.
setResponseBody     :: (forall a . Enumerator a)  -- ^ new response body
                                                  -- enumerator
                    -> Response                   -- ^ response to modify
                    -> Response
setResponseBody e r = r { rspBody = Enum e }
{-# INLINE setResponseBody #-}

-- | Sets the HTTP response status.
setResponseStatus   :: Int        -- ^ HTTP response integer code
                    -> ByteString -- ^ HTTP response explanation
                    -> Response   -- ^ Response to be modified
                    -> Response
setResponseStatus s reason r = r { rspStatus=s, rspStatusReason=reason }
{-# INLINE setResponseStatus #-}


-- | Modifies a response body.
modifyResponseBody  :: (forall a . Enumerator a -> Enumerator a)
                    -> Response
                    -> Response
modifyResponseBody f r = r { rspBody = rspBodyMap f (rspBody r) }
{-# INLINE modifyResponseBody #-}


-- | Sets the @Content-Type@ in the 'Response' headers.
setContentType      :: ByteString -> Response -> Response
setContentType = setHeader "Content-Type"
{-# INLINE setContentType #-}


-- | Adds an HTTP 'Cookie' to the 'Response' headers.
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


-- | Removes any @Content-Length@ set in the 'Response'.
clearContentLength :: Response -> Response
clearContentLength r = r { rspContentLength = Nothing }
{-# INLINE clearContentLength #-}


------------------------------------------------------------------------------
-- HTTP dates

{-
-- | Converts a 'ClockTime' into an HTTP timestamp.
formatHttpTime :: UTCTime -> ByteString
formatHttpTime = fromStr . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"

-- | Converts an HTTP timestamp into a 'UTCTime'.
parseHttpTime :: ByteString -> Maybe UTCTime
parseHttpTime s' =
    parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" s
  where
    s = toStr s'
-}

-- | Converts a 'CTime' into an HTTP timestamp.
formatHttpTime :: CTime -> IO ByteString
formatHttpTime t = allocaBytes 40 $ \ptr -> do
    c_format_http_time t ptr
    S.packCString ptr

-- | Converts an HTTP timestamp into a 'CTime'.
parseHttpTime :: ByteString -> IO CTime
parseHttpTime s = S.useAsCString s $ \ptr ->
    c_parse_http_time ptr


------------------------------------------------------------------------------
-- URL ENCODING
------------------------------------------------------------------------------

parseToCompletion :: Parser a -> ByteString -> Maybe a
parseToCompletion p s = toResult $ finish r
  where
    r = parse p s

    toResult (Atto.Done _ c) = Just c
    toResult _               = Nothing


pUrlEscaped :: Parser ByteString
pUrlEscaped = do
    sq <- nextChunk DL.empty
    return $ S.concat $ DL.toList sq

  where
    nextChunk :: DList ByteString -> Parser (DList ByteString)
    nextChunk s = (endOfInput *> pure s) <|> do
        c <- anyWord8
        case w2c c of
          '+' -> plusSpace s
          '%' -> percentEncoded s
          _   -> unEncoded c s

    percentEncoded :: DList ByteString -> Parser (DList ByteString)
    percentEncoded l = do
        hx <- take 2
        when (S.length hx /= 2 ||
               (not $ S.all (isHexDigit . w2c) hx)) $
             fail "bad hex in url"
          
        let code = (Cvt.hex hx) :: Word8
        nextChunk $ DL.snoc l (S.singleton code)

    unEncoded :: Word8 -> DList ByteString -> Parser (DList ByteString)
    unEncoded c l' = do
        let l = DL.snoc l' (S.singleton c)
        bs <- takeTill (flip elem (map c2w "%+"))
        if S.null bs
          then nextChunk l
          else nextChunk $ DL.snoc l bs

    plusSpace :: DList ByteString -> Parser (DList ByteString)
    plusSpace l = nextChunk (DL.snoc l (S.singleton $ c2w ' '))


-- | Decodes an URL-escaped string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>)
urlDecode :: ByteString -> Maybe ByteString
urlDecode = parseToCompletion pUrlEscaped


-- "...Only alphanumerics [0-9a-zA-Z], the special characters "$-_.+!*'(),"
-- [not including the quotes - ed], and reserved characters used for their
-- reserved purposes may be used unencoded within a URL."

-- | URL-escapes a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>)
urlEncode :: ByteString -> ByteString
urlEncode = toByteString . S.foldl' f empty
  where
    f b c =
        if c == c2w ' '
          then b `mappend` singleton (c2w '+')
          else if isKosher c
                 then b `mappend` singleton c
                 else b `mappend` hexd c

    isKosher w = any ($ c) [ isAlphaNum
                           , flip elem ['$', '-', '.', '!', '*'
                                       , '\'', '(', ')', ',' ]]
      where
        c = w2c w

hexd :: Word8 -> Builder
hexd c = singleton (c2w '%') `mappend` singleton hi `mappend` singleton low
  where
    d   = c2w . intToDigit
    low = d $ fromEnum $ c .&. 0xf
    hi  = d $ fromEnum $ (c .&. 0xf0) `shift` (-4)


finish :: Atto.Result a -> Atto.Result a
finish (Atto.Partial f) = flip feed "" $ f ""
finish x                = x

char :: Char -> Parser Word8
char = word8 . c2w

------------------------------------------------------------------------------
-- local definitions
fromStr :: String -> ByteString
fromStr = S.pack . map c2w
{-# INLINE fromStr #-}

------------------------------------------------------------------------------
-- private helper functions
toStr :: ByteString -> String
toStr = map w2c . S.unpack

