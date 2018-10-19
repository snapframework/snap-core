{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

------------------------------------------------------------------------------
-- | An internal Snap module containing HTTP types.
--
-- /N.B./ this is an internal interface, please don't write user code that
-- depends on it. Most of these declarations (except for the
-- unsafe/encapsulation-breaking ones) are re-exported from "Snap.Core".
--
module Snap.Internal.Http.Types where

------------------------------------------------------------------------------
import           Control.Monad              (unless)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder    (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import qualified Data.IntMap                as IM
import           Data.List                  hiding (take)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (Maybe (..), fromMaybe, maybe)
import           Data.Monoid                (mconcat)
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.Word                  (Word64)
import           Foreign.C.Types            (CTime (..))
import           Prelude                    (Bool (..), Eq (..), FilePath, IO, Int, Integral (..), Monad (..), Num ((-)), Ord (..), Ordering (..), Read (..), Show (..), String, fmap, fromInteger, fromIntegral, id, not, otherwise, truncate, ($), (.))
#ifdef PORTABLE
import           Prelude                    (realToFrac, ($!))
#endif
import           System.IO                  (IOMode (ReadMode), SeekMode (AbsoluteSeek), hSeek, withBinaryFile)
import           System.IO.Streams          (InputStream, OutputStream)
import qualified System.IO.Streams          as Streams
import           System.IO.Unsafe           (unsafePerformIO)

------------------------------------------------------------------------------
#ifdef PORTABLE
import           Data.Time.Clock.POSIX
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.Locale.Compat    (defaultTimeLocale)
import           Data.Time.LocalTime
#else
import qualified Data.ByteString.Unsafe     as S
import           Data.Time.Format           ()
import           Foreign.C.String           (CString)
import           Foreign.Marshal.Alloc      (mallocBytes)
#endif

------------------------------------------------------------------------------
import           Snap.Types.Headers         (Headers)
import qualified Snap.Types.Headers         as H


#ifndef PORTABLE

------------------------------------------------------------------------------
-- foreign imports from cbits
foreign import ccall unsafe "set_c_locale"
        set_c_locale :: IO ()

foreign import ccall unsafe "c_parse_http_time"
        c_parse_http_time :: CString -> IO CTime

foreign import ccall unsafe "c_format_http_time"
        c_format_http_time :: CTime -> CString -> IO ()

foreign import ccall unsafe "c_format_log_time"
        c_format_log_time :: CTime -> CString -> IO ()

#endif


------------------------------------------------------------------------------
-- | A typeclass for datatypes which contain HTTP headers.
class HasHeaders a where
    -- | Modify the datatype's headers.
    updateHeaders :: (Headers -> Headers) -> a -> a

    -- | Retrieve the headers from a datatype that has headers.
    headers       :: a -> Headers


------------------------------------------------------------------------------
-- | Adds a header key-value-pair to the 'HasHeaders' datatype. If a header
-- with the same name already exists, the new value is appended to the headers
-- list.
--
-- Example:
--
-- @
-- ghci> import qualified "Snap.Types.Headers" as H
-- ghci> 'addHeader' \"Host\" "localhost" H.'empty'
-- H {unH = [("host","localhost")]}
-- ghci> 'addHeader' \"Host\" "127.0.0.1" it
-- H {unH = [("host","localhost,127.0.0.1")]}
-- @
addHeader :: (HasHeaders a) => CI ByteString -> ByteString -> a -> a
addHeader k v = updateHeaders $ H.insert k v


------------------------------------------------------------------------------
-- | Sets a header key-value-pair in a 'HasHeaders' datatype. If a header with
-- the same name already exists, it is overwritten with the new value.
--
-- Example:
--
-- @
-- ghci> import qualified "Snap.Types.Headers" as H
-- ghci> 'setHeader' \"Host\" "localhost" H.'empty'
-- H {unH = [(\"host\",\"localhost\")]}
-- ghci> setHeader \"Host\" "127.0.0.1" it
-- H {unH = [("host","127.0.0.1")]}
-- @
setHeader :: (HasHeaders a) => CI ByteString -> ByteString -> a -> a
setHeader k v = updateHeaders $ H.set k v


------------------------------------------------------------------------------
-- | Gets a header value out of a 'HasHeaders' datatype.
--
-- Example:
--
-- @
-- ghci> import qualified "Snap.Types.Headers" as H
-- ghci> 'getHeader' \"Host\" $ 'setHeader' \"Host\" "localhost" H.'empty'
-- Just "localhost"
-- @
getHeader :: (HasHeaders a) => CI ByteString -> a -> Maybe ByteString
getHeader k a = H.lookup k $ headers a


------------------------------------------------------------------------------
-- | Lists all the headers out of a 'HasHeaders' datatype. If many
-- headers came in with the same name, they will be catenated together.
--
-- Example:
--
-- @
-- ghci> import qualified "Snap.Types.Headers" as H
-- ghci> 'listHeaders' $ 'setHeader' \"Host\" "localhost" H.'empty'
-- [("host","localhost")]
-- @
listHeaders :: (HasHeaders a) => a -> [(CI ByteString, ByteString)]
listHeaders = H.toList . headers


------------------------------------------------------------------------------
-- | Clears a header value from a 'HasHeaders' datatype.
--
-- Example:
--
-- @
-- ghci> import qualified "Snap.Types.Headers" as H
-- ghci> 'deleteHeader' \"Host\" $ 'setHeader' \"Host\" "localhost" H.'empty'
-- H {unH = []}
-- @
deleteHeader :: (HasHeaders a) => CI ByteString -> a -> a
deleteHeader k = updateHeaders $ H.delete k


------------------------------------------------------------------------------
-- | Enumerates the HTTP method values (see
-- <http://tools.ietf.org/html/rfc2068.html#section-5.1.1>).
data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT |
               PATCH | Method ByteString
               deriving(Show, Read)

instance Eq Method where
    a == b =
        normalizeMethod a `eq` normalizeMethod b
      where
        GET       `eq` GET       = True
        HEAD      `eq` HEAD      = True
        POST      `eq` POST      = True
        PUT       `eq` PUT       = True
        DELETE    `eq` DELETE    = True
        TRACE     `eq` TRACE     = True
        OPTIONS   `eq` OPTIONS   = True
        CONNECT   `eq` CONNECT   = True
        PATCH     `eq` PATCH     = True
        Method x1 `eq` Method y1 = x1 == y1
        _         `eq` _         = False

instance Ord Method where
        compare a b =
            check (normalizeMethod a) (normalizeMethod b)
          where
            check   GET          GET           = EQ
            check   HEAD         HEAD          = EQ
            check   POST         POST          = EQ
            check   PUT          PUT           = EQ
            check   DELETE       DELETE        = EQ
            check   TRACE        TRACE         = EQ
            check   OPTIONS      OPTIONS       = EQ
            check   CONNECT      CONNECT       = EQ
            check   PATCH        PATCH         = EQ
            check   (Method  x1) (Method   y1) = compare x1 y1
            check   x            y             = compare (tag x) (tag y)

            tag :: Method -> Int
            tag (GET{})     = 0
            tag (HEAD{})    = 1
            tag (POST{})    = 2
            tag (PUT{})     = 3
            tag (DELETE{})  = 4
            tag (TRACE{})   = 5
            tag (OPTIONS{}) = 6
            tag (CONNECT{}) = 7
            tag (PATCH{})   = 8
            tag (Method{})  = 9

-- | Equate the special case constructors with their corresponding
-- @Method name@ variant.
{-# INLINE normalizeMethod #-}
normalizeMethod :: Method -> Method
normalizeMethod m@(Method name) = case name of
                                    "GET"     -> GET
                                    "HEAD"    -> HEAD
                                    "POST"    -> POST
                                    "PUT"     -> PUT
                                    "DELETE"  -> DELETE
                                    "TRACE"   -> TRACE
                                    "OPTIONS" -> OPTIONS
                                    "CONNECT" -> CONNECT
                                    "PATCH"   -> PATCH
                                    _         -> m
normalizeMethod m               = m


------------------------------------------------------------------------------
-- | Represents a (major, minor) version of the HTTP protocol.
type HttpVersion = (Int,Int)


------------------------------------------------------------------------------
-- | A datatype representing an HTTP cookie.
data Cookie = Cookie {
      -- | The name of the cookie.
      cookieName     :: !ByteString

      -- | The cookie's string value.
    , cookieValue    :: !ByteString

      -- | The cookie's expiration value, if it has one.
    , cookieExpires  :: !(Maybe UTCTime)

      -- | The cookie's \"domain\" value, if it has one.
    , cookieDomain   :: !(Maybe ByteString)

      -- | The cookie path.
    , cookiePath     :: !(Maybe ByteString)

      -- | Tag as secure cookie?
    , cookieSecure   :: !Bool

      -- | HTTP only?
    , cookieHttpOnly :: !Bool
} deriving (Eq, Show)


------------------------------------------------------------------------------
-- | A type alias for the HTTP parameters mapping. Each parameter
-- key maps to a list of 'ByteString' values; if a parameter is specified
-- multiple times (e.g.: \"@GET /foo?param=bar1&param=bar2@\"), looking up
-- \"@param@\" in the mapping will give you @[\"bar1\", \"bar2\"]@.
type Params = Map ByteString [ByteString]


------------------------------------------------------------------------------
-- request type
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Contains all of the information about an incoming HTTP request.
data Request = Request
    { -- | The server name of the request, as it came in from the request's
      -- @Host:@ header.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> :{
      -- ghci| rq <- T.buildRequest $ do
      -- ghci|         T.get "\/foo\/bar" M.empty
      -- ghci|         T.setHeader "host" "example.com"
      -- ghci| :}
      -- ghci> rqHostName rq
      -- "example.com"
      -- @
      rqHostName      :: ByteString

      -- | The remote IP address.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqClientAddr \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- "127.0.0.1"
      -- @
    , rqClientAddr    :: ByteString

      -- | The remote TCP port number.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqClientPort \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- "60000"
      -- @
    , rqClientPort    :: {-# UNPACK #-} !Int

      -- | The local IP address for this request.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqServerAddr \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- "127.0.0.1"
      -- @
    , rqServerAddr    :: ByteString

      -- | Returns the port number the HTTP server is listening on. This may be
      -- useless from the perspective of external requests, e.g. if the server
      -- is running behind a proxy.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqServerPort \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- 8080
      -- @
    , rqServerPort    :: {-# UNPACK #-} !Int

      -- | Returns the HTTP server's idea of its local hostname, including
      -- port. This is as configured with the @Config@ object at startup.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqLocalHostname \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- "localhost"
      -- @
    , rqLocalHostname :: ByteString

      -- | Returns @True@ if this is an HTTPS session.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqIsSecure \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- False
      -- @
    , rqIsSecure      :: !Bool

      -- | Contains all HTTP 'Headers' associated with this request.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqHeaders \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- H {unH = [("host","localhost")]}
      -- @
    , rqHeaders       :: Headers

      -- | Actual body of the request.
    , rqBody          :: InputStream ByteString

      -- | Returns the @Content-Length@ of the HTTP request body.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqContentLength \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- Nothing
      -- @
    , rqContentLength :: !(Maybe Word64)

      -- | Returns the HTTP request method.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqMethod \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- GET
      -- @
    , rqMethod        :: !Method

      -- | Returns the HTTP version used by the client.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqVersion \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- (1,1)
      -- @
    , rqVersion       :: {-# UNPACK #-} !HttpVersion

      -- | Returns a list of the cookies that came in from the HTTP request
      -- headers.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqCookies \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- []
      -- @
    , rqCookies       :: [Cookie]

      -- | Handlers can be hung on a @URI@ \"entry point\"; this is called the
      -- \"context path\". If a handler is hung on the context path
      -- @\"\/foo\/\"@, and you request @\"\/foo\/bar\"@, the value of
      -- 'rqPathInfo' will be @\"bar\"@.
      --
      -- The following identity holds:
      --
      -- > rqURI r == S.concat [ rqContextPath r
      -- >                     , rqPathInfo r
      -- >                     , let q = rqQueryString r
      -- >                       in if S.null q
      -- >                            then ""
      -- >                            else S.append "?" q
      -- >                     ]
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqPathInfo \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- "foo/bar"
      -- @
    , rqPathInfo      :: ByteString

      -- | The \"context path\" of the request; catenating 'rqContextPath',
      -- and 'rqPathInfo' should get you back to the original 'rqURI'
      -- (ignoring query strings). The 'rqContextPath' always begins and ends
      -- with a slash (@\"\/\"@) character, and represents the path (relative
      -- to your component\/snaplet) you took to get to your handler.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqContextPath \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- "/"
      -- @
    , rqContextPath   :: ByteString

      -- | Returns the @URI@ requested by the client.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rqURI \`fmap\` T.buildRequest (T.get "\/foo\/bar" M.empty)
      -- "foo/bar"
      -- @
    , rqURI           :: ByteString

      -- | Returns the HTTP query string for this 'Request'.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> rq <- T.buildRequest (T.get "\/foo\/bar" (M.fromList [("name", ["value"])]))
      -- ghci> rqQueryString rq
      -- "name=value"
      -- @
    , rqQueryString   :: ByteString

      -- | Returns the parameters mapping for this 'Request'. \"Parameters\"
      -- are automatically decoded from the URI's query string and @POST@ body
      -- and entered into this mapping. The 'rqParams' value is thus a union of
      -- 'rqQueryParams' and 'rqPostParams'.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> :{
      -- ghci| rq <- T.buildRequest $ do
      -- ghci|         T.postUrlEncoded "\/foo\/bar" $ M.fromList [("baz", ["qux"])]
      -- ghci|         T.setQueryStringRaw "baz=quux"
      -- ghci| :}
      -- ghci> rqParams rq
      -- fromList [("baz",["qux","quux"])]
      -- @
    , rqParams        :: Params

      -- | The parameter mapping decoded from the URI's query string.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> :{
      -- ghci| rq <- T.buildRequest $ do
      -- ghci|         T.postUrlEncoded "\/foo\/bar" $ M.fromList [("baz", ["qux"])]
      -- ghci|         T.setQueryStringRaw "baz=quux"
      -- ghci| :}
      -- ghci> rqQueryParams rq
      -- fromList [("baz",["quux"])]
      -- @
    , rqQueryParams   :: Params

      -- | The parameter mapping decoded from the POST body. Note that Snap
      -- only auto-decodes POST request bodies when the request's
      -- @Content-Type@ is @application\/x-www-form-urlencoded@.
      -- For @multipart\/form-data@ use 'Snap.Util.FileUploads.handleFileUploads'
      -- to decode the POST request and fill this mapping.
      --
      -- Example:
      --
      -- @
      -- ghci> :set -XOverloadedStrings
      -- ghci> import qualified "Snap.Test" as T
      -- ghci> import qualified "Data.Map" as M
      -- ghci> :{
      -- ghci| rq <- T.buildRequest $ do
      -- ghci|         T.postUrlEncoded "\/foo\/bar" $ M.fromList [("baz", ["qux"])]
      -- ghci|         T.setQueryStringRaw "baz=quux"
      -- ghci| :}
      -- ghci> rqPostParams rq
      -- fromList [("baz",["qux"])]
      -- @
    , rqPostParams    :: Params
    }


------------------------------------------------------------------------------
instance Show Request where
  show r = concat [ method, " ", uri, " HTTP/", version, "\n"
                  , hdrs, "\n\n"
                  , "sn=\"", sname, "\" c=", clntAddr, " s=", srvAddr
                  , " ctx=", contextpath, " clen=", contentlength, secure
                  , params, cookies
                  ]
    where
      method        = show $ rqMethod r
      uri           = S.unpack $ rqURI r
      version       = let (mj, mn) = rqVersion r in show mj ++ "." ++ show mn
      hdrs          = intercalate "\n" $ map showHdr (H.toList $ rqHeaders r)
      showHdr (a,b) = (S.unpack $ CI.original a) ++ ": " ++ S.unpack b
      sname         = S.unpack $ rqLocalHostname r
      clntAddr      = concat [S.unpack $ rqClientAddr r, ":", show $ rqClientPort r]
      srvAddr       = concat [S.unpack $ rqServerAddr r, ":", show $ rqServerPort r]
      contextpath   = S.unpack $ rqContextPath r
      contentlength = maybe "n/a" show (rqContentLength r)
      secure        = if rqIsSecure r then " secure" else ""

      params        = showFlds "\nparams: " ", " $
                      map (\ (a,b) -> S.unpack a ++ ": " ++ show b)
                      (Map.toAscList $ rqParams r)
      cookies       = showFlds "\ncookies: " "\n         " $
                      map show (rqCookies r)

      showFlds header delim lst
                    = if not . null $ lst then header ++ (intercalate delim lst)
                      else "" :: String

------------------------------------------------------------------------------
instance HasHeaders Request where
    headers           = rqHeaders
    updateHeaders f r = r { rqHeaders = f (rqHeaders r) }


------------------------------------------------------------------------------
instance HasHeaders Headers where
    headers       = id
    updateHeaders = id

------------------------------------------------------------------------------
-- response type
------------------------------------------------------------------------------

type StreamProc = OutputStream Builder -> IO (OutputStream Builder)
data ResponseBody = Stream (StreamProc)
                      -- ^ output body is a function that writes to a 'Builder'
                      -- stream

                  | SendFile FilePath (Maybe (Word64, Word64))
                      -- ^ output body is sendfile(), optional second argument
                      --   is a byte range to send


------------------------------------------------------------------------------
rspBodyMap :: (StreamProc -> StreamProc) -> ResponseBody -> ResponseBody
rspBodyMap f b = Stream $ f $ rspBodyToEnum b


------------------------------------------------------------------------------
rspBodyToEnum :: ResponseBody -> StreamProc
rspBodyToEnum (Stream e) = e

rspBodyToEnum (SendFile fp Nothing) = \out ->
    Streams.withFileAsInput fp $ \is -> do
        is' <- Streams.mapM (return . byteString) is
        Streams.connect is' out
        return out

rspBodyToEnum (SendFile fp (Just (start, end))) = \out ->
    withBinaryFile fp ReadMode $ \handle -> do
        unless (start == 0) $ hSeek handle AbsoluteSeek $ toInteger start
        is  <- Streams.handleToInputStream handle
        is' <- Streams.takeBytes (fromIntegral $ end - start) is >>=
               Streams.mapM (return . byteString)
        Streams.connect is' out
        return out


------------------------------------------------------------------------------
-- | Represents an HTTP response.
data Response = Response
    { rspHeaders            :: Headers
    , rspCookies            :: Map ByteString Cookie

      -- | We will need to inspect the content length no matter what, and
      --   looking up \"content-length\" in the headers and parsing the number
      --   out of the text will be too expensive.
    , rspContentLength      :: !(Maybe Word64)
    , rspBody               :: ResponseBody

      -- | Returns the HTTP status code.
      --
      -- Example:
      --
      -- @
      -- ghci> rspStatus 'emptyResponse'
      -- 200
      -- @
    , rspStatus             :: !Int

      -- | Returns the HTTP status explanation string.
      --
      -- Example:
      --
      -- @
      -- ghci> rspStatusReason 'emptyResponse'
      -- "OK"
      -- @
    , rspStatusReason       :: !ByteString

      -- | If true, we are transforming the request body with
      -- 'transformRequestBody'
    , rspTransformingRqBody :: !Bool
    }


------------------------------------------------------------------------------
instance Show Response where
  show r = concat [ statusline
                  , hdrs
                  , contentLength
                  , "\r\n"
                  , body
                  ]
    where
      statusline = concat [ "HTTP/1.1 "
                          , show $ rspStatus r
                          , " "
                          , S.unpack $ rspStatusReason r
                          , "\r\n" ]

      hdrs = concatMap showHdr $ H.toList $ renderCookies r
             $ rspHeaders $ clearContentLength r

      contentLength = maybe "" (\l -> concat ["Content-Length: ", show l, "\r\n"] ) (rspContentLength r)

      showHdr (k,v) = concat [ S.unpack (CI.original k), ": ", S.unpack v, "\r\n" ]

      -- io-streams are impure, so we're forced to use 'unsafePerformIO'.
      body = unsafePerformIO $ do
        (os, grab) <- Streams.listOutputStream
        let f = rspBodyToEnum $ rspBody r
        _ <- f os
        fmap (L.unpack . toLazyByteString . mconcat) grab



------------------------------------------------------------------------------
instance HasHeaders Response where
    headers = rspHeaders
    updateHeaders f r = r { rspHeaders = f (rspHeaders r) }


------------------------------------------------------------------------------
-- | Looks up the value(s) for the given named parameter. Parameters initially
-- come from the request's query string and any decoded POST body (if the
-- request's @Content-Type@ is @application\/x-www-form-urlencoded@).
-- Parameter values can be modified within handlers using "rqModifyParams".
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.Map" as M
-- ghci> :{
-- ghci| rq <- T.buildRequest $ do
-- ghci|         T.postUrlEncoded "\/foo\/bar" $ M.fromList [("baz", ["qux"])]
-- ghci|         T.setQueryStringRaw "baz=quux"
-- ghci| :}
-- ghci> 'rqParam' "baz" rq
-- Just ["qux","quux"]
-- @
rqParam :: ByteString           -- ^ parameter name to look up
        -> Request              -- ^ HTTP request
        -> Maybe [ByteString]
rqParam k rq = Map.lookup k $ rqParams rq
{-# INLINE rqParam #-}


------------------------------------------------------------------------------
-- | Looks up the value(s) for the given named parameter in the POST parameters
-- mapping.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.Map" as M
-- ghci> :{
-- ghci| rq <- T.buildRequest $ do
-- ghci|         T.postUrlEncoded "\/foo\/bar" $ M.fromList [("baz", ["qux"])]
-- ghci|         T.setQueryStringRaw "baz=quux"
-- ghci| :}
-- ghci> 'rqPostParam' "baz" rq
-- Just ["qux"]
-- @
rqPostParam :: ByteString           -- ^ parameter name to look up
            -> Request              -- ^ HTTP request
            -> Maybe [ByteString]
rqPostParam k rq = Map.lookup k $ rqPostParams rq
{-# INLINE rqPostParam #-}


------------------------------------------------------------------------------
-- | Looks up the value(s) for the given named parameter in the query
-- parameters mapping.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.Map" as M
-- ghci> :{
-- ghci| rq <- T.buildRequest $ do
-- ghci|         T.postUrlEncoded "\/foo\/bar" $ M.fromList [("baz", ["qux"])]
-- ghci|         T.setQueryStringRaw "baz=quux"
-- ghci| :}
-- ghci> 'rqQueryParam' "baz" rq
-- Just ["quux"]
-- @
rqQueryParam :: ByteString           -- ^ parameter name to look up
             -> Request              -- ^ HTTP request
             -> Maybe [ByteString]
rqQueryParam k rq = Map.lookup k $ rqQueryParams rq
{-# INLINE rqQueryParam #-}


------------------------------------------------------------------------------
-- | Modifies the parameters mapping (which is a @Map ByteString ByteString@)
-- in a 'Request' using the given function.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.Map" as M
-- ghci> :{
-- ghci| rq <- T.buildRequest $ do
-- ghci|         T.postUrlEncoded "\/foo\/bar" $ M.fromList [("baz", ["qux"])]
-- ghci|         T.setQueryStringRaw "baz=quux"
-- ghci| :}
-- ghci> 'rqParams' rq
-- fromList [("baz",["qux","quux"])]
-- ghci> 'rqParams' $ 'rqModifyParams' (M.delete "baz") rq
-- fromList []
-- @
rqModifyParams :: (Params -> Params) -> Request -> Request
rqModifyParams f r = r { rqParams = p }
  where
    p = f $ rqParams r
{-# INLINE rqModifyParams #-}


------------------------------------------------------------------------------
-- | Writes a key-value pair to the parameters mapping within the given
-- request.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.Map" as M
-- ghci> :{
-- ghci| rq <- T.buildRequest $ do
-- ghci|         T.postUrlEncoded "\/foo\/bar" $ M.fromList [("baz", ["qux"])]
-- ghci|         T.setQueryStringRaw "baz=quux"
-- ghci| :}
-- ghci> 'rqParams' rq
-- fromList [("baz",["qux","quux"])]
-- ghci> 'rqParams' $ 'rqSetParam' "baz" ["corge"] rq
-- fromList [("baz", ["corge"])]
-- @
rqSetParam :: ByteString        -- ^ parameter name
           -> [ByteString]      -- ^ parameter values
           -> Request           -- ^ request
           -> Request
rqSetParam k v = rqModifyParams $ Map.insert k v
{-# INLINE rqSetParam #-}


                                ---------------
                                -- responses --
                                ---------------

------------------------------------------------------------------------------
-- | An empty 'Response'.
--
-- Example:
--
-- @
-- ghci> 'emptyResponse'
-- HTTP\/1.1 200 OK
--
--
-- @
emptyResponse :: Response
emptyResponse = Response H.empty Map.empty Nothing
                         (Stream (return . id))
                         200 "OK" False


------------------------------------------------------------------------------
-- | Sets an HTTP response body to the given stream procedure.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "System.IO.Streams" as Streams
-- ghci> import qualified "Data.ByteString.Builder" as Builder
-- ghci> :{
-- ghci| let r = 'setResponseBody'
-- ghci|         (\out -> do
-- ghci|             Streams.write (Just $ Builder.'byteString' \"Hello, world!\") out
-- ghci|             return out)
-- ghci|         'emptyResponse'
-- ghci| :}
-- ghci> r
-- HTTP\/1.1 200 OK
--
-- Hello, world!
-- @
setResponseBody     :: (OutputStream Builder -> IO (OutputStream Builder))
                                   -- ^ new response body
                    -> Response    -- ^ response to modify
                    -> Response
setResponseBody e r = r { rspBody = Stream e }
{-# INLINE setResponseBody #-}


------------------------------------------------------------------------------
-- | Sets the HTTP response status. Note: normally you would use
-- 'setResponseCode' unless you needed a custom response explanation.
--
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> setResponseStatus 500 \"Internal Server Error\" 'emptyResponse'
-- HTTP\/1.1 500 Internal Server Error
--
--
-- @
setResponseStatus   :: Int        -- ^ HTTP response integer code
                    -> ByteString -- ^ HTTP response explanation
                    -> Response   -- ^ Response to be modified
                    -> Response
setResponseStatus s reason r = r { rspStatus=s, rspStatusReason=reason }
{-# INLINE setResponseStatus #-}


------------------------------------------------------------------------------
-- | Sets the HTTP response code.
--
-- Example:
--
-- @
-- ghci> setResponseCode 404 'emptyResponse'
-- HTTP\/1.1 404 Not Found
--
--
-- @
setResponseCode   :: Int        -- ^ HTTP response integer code
                  -> Response   -- ^ Response to be modified
                  -> Response
setResponseCode s r = setResponseStatus s reason r
  where
    reason = fromMaybe "Unknown" (IM.lookup s statusReasonMap)
{-# INLINE setResponseCode #-}


------------------------------------------------------------------------------
-- | Modifies a response body.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "System.IO.Streams" as Streams
-- ghci> import qualified "Data.ByteString.Builder" as Builder
-- ghci> :{
-- ghci| let r = 'setResponseBody'
-- ghci|         (\out -> do
-- ghci|             Streams.write (Just $ Builder.'byteString' \"Hello, world!\") out
-- ghci|             return out)
-- ghci|         'emptyResponse'
-- ghci| :}
-- ghci> r
-- HTTP\/1.1 200 OK
--
-- Hello, world!
-- ghci> :{
-- ghci| let r' = 'modifyResponseBody'
-- ghci|          (\f out -> do
-- ghci|              out' <- f out
-- ghci|              Streams.write (Just $ Builder.'byteString' \"\\nBye, world!\") out'
-- ghci|              return out') r
-- ghci| :}
-- ghci> r'
-- HTTP\/1.1 200 OK
--
-- Hello, world!
-- Bye, world!
-- @
modifyResponseBody  :: ((OutputStream Builder -> IO (OutputStream Builder)) ->
                        (OutputStream Builder -> IO (OutputStream Builder)))
                    -> Response
                    -> Response
modifyResponseBody f r = r { rspBody = rspBodyMap f (rspBody r) }
{-# INLINE modifyResponseBody #-}


------------------------------------------------------------------------------
-- | Sets the @Content-Type@ in the 'Response' headers.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> setContentType \"text\/html\" 'emptyResponse'
-- HTTP\/1.1 200 OK
-- content-type: text\/html
--
--
-- @
setContentType      :: ByteString -> Response -> Response
setContentType = setHeader "Content-Type"
{-# INLINE setContentType #-}


------------------------------------------------------------------------------
-- | Convert 'Cookie' into 'ByteString' for output.
--
-- TODO: Remove duplication. This function is copied from
-- snap-server/Snap.Internal.Http.Server.Session.
cookieToBS :: Cookie -> ByteString
cookieToBS (Cookie k v mbExpTime mbDomain mbPath isSec isHOnly) = cookie
  where
    cookie = S.concat [k, "=", v, path, exptime, domain, secure, hOnly]
    path = maybe "" (S.append "; path=") mbPath
    domain = maybe "" (S.append "; domain=") mbDomain
    exptime = maybe "" (S.append "; expires=" . fmt) mbExpTime
    secure = if isSec then "; Secure" else ""
    hOnly = if isHOnly then "; HttpOnly" else ""

    -- TODO: 'formatHttpTime' uses "DD MMM YYYY" instead of "DD-MMM-YYYY",
    -- unlike the code in 'Snap.Internal.Http.Server.Session'. Is this form
    -- allowed?
    fmt = unsafePerformIO . formatHttpTime . toCTime

    toCTime :: UTCTime -> CTime
    toCTime = fromInteger . truncate . utcTimeToPOSIXSeconds

------------------------------------------------------------------------------
-- | Render cookies from a given 'Response' to 'Headers'.
--
-- TODO: Remove duplication. This function is copied from
-- snap-server/Snap.Internal.Http.Server.Session.
renderCookies :: Response -> Headers -> Headers
renderCookies r hdrs
    | null cookies = hdrs
    | otherwise = foldl' (\m v -> H.unsafeInsert "set-cookie" v m) hdrs cookies

  where
    cookies = fmap cookieToBS . Map.elems $ rspCookies r

------------------------------------------------------------------------------
-- | Adds an HTTP 'Cookie' to 'Response' headers.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> let cookie = 'Cookie' \"name\" \"value\" Nothing Nothing Nothing False False
-- ghci> 'getResponseCookie' \"name\" $ 'addResponseCookie' cookie 'emptyResponse'
-- Just (Cookie {cookieName = \"name\", cookieValue = \"value\", ...})
-- @
addResponseCookie :: Cookie            -- ^ cookie value
                  -> Response          -- ^ response to modify
                  -> Response
addResponseCookie ck@(Cookie k _ _ _ _ _ _) r = r { rspCookies = cks' }
  where
    cks'= Map.insert k ck $ rspCookies r
{-# INLINE addResponseCookie #-}


------------------------------------------------------------------------------
-- | Gets an HTTP 'Cookie' with the given name from 'Response' headers.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> 'getResponseCookie' \"cookie-name\" 'emptyResponse'
-- Nothing
-- @
getResponseCookie :: ByteString            -- ^ cookie name
                  -> Response              -- ^ response to query
                  -> Maybe Cookie
getResponseCookie cn r = Map.lookup cn $ rspCookies r
{-# INLINE getResponseCookie #-}


-- | Returns a list of 'Cookie's present in 'Response'
--
-- Example:
--
-- @
-- ghci> 'getResponseCookies' 'emptyResponse'
-- []
-- @
getResponseCookies :: Response              -- ^ response to query
                   -> [Cookie]
getResponseCookies = Map.elems . rspCookies
{-# INLINE getResponseCookies #-}


------------------------------------------------------------------------------
-- | Deletes an HTTP 'Cookie' from the 'Response' headers. Please note
-- this does not necessarily erase the cookie from the client browser.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> let cookie = 'Cookie' \"name\" \"value\" Nothing Nothing Nothing False False
-- ghci> let rsp    = 'addResponseCookie' cookie 'emptyResponse'
-- ghci> 'getResponseCookie' \"name\" rsp
-- Just (Cookie {cookieName = \"name\", cookieValue = \"value\", ...})
-- ghci> 'getResponseCookie' \"name\" $ 'deleteResponseCookie' \"name\" rsp
-- Nothing
-- @
deleteResponseCookie :: ByteString        -- ^ cookie name
                     -> Response          -- ^ response to modify
                     -> Response
deleteResponseCookie cn r = r { rspCookies = cks' }
  where
    cks'= Map.delete cn $ rspCookies r
{-# INLINE deleteResponseCookie #-}


------------------------------------------------------------------------------
-- | Modifies an HTTP 'Cookie' with given name in 'Response' headers.
-- Nothing will happen if a matching 'Cookie' can not be found in 'Response'.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import "Data.Monoid"
-- ghci> let cookie = 'Cookie' \"name\" \"value\" Nothing Nothing Nothing False False
-- ghci> let rsp    = 'addResponseCookie' cookie 'emptyResponse'
-- ghci> 'getResponseCookie' \"name\" rsp
-- Just (Cookie {cookieName = \"name\", cookieValue = \"value\", ...})
-- ghci> let f ck@('Cookie' { cookieName = name }) = ck { cookieName = name <> \"\'\"}
-- ghci> let rsp' = 'modifyResponseCookie' \"name\" f rsp
-- ghci> 'getResponseCookie' \"name\'\" rsp\'
-- Just (Cookie {cookieName = \"name\'\", ...})
-- ghci> 'getResponseCookie' \"name\" rsp\'
-- Just (Cookie {cookieName = \"name\", ...})
-- @
modifyResponseCookie :: ByteString          -- ^ cookie name
                     -> (Cookie -> Cookie)  -- ^ modifier function
                     -> Response            -- ^ response to modify
                     -> Response
modifyResponseCookie cn f r = maybe r modify $ getResponseCookie cn r
  where
    modify ck = addResponseCookie (f ck) r
{-# INLINE modifyResponseCookie #-}


------------------------------------------------------------------------------
-- | A note here: if you want to set the @Content-Length@ for the response,
-- Snap forces you to do it with this function rather than by setting it in
-- the headers; the @Content-Length@ in the headers will be ignored.
--
-- The reason for this is that Snap needs to look up the value of
-- @Content-Length@ for each request, and looking the string value up in the
-- headers and parsing the number out of the text will be too expensive.
--
-- If you don't set a content length in your response, HTTP keep-alive will be
-- disabled for HTTP\/1.0 clients, forcing a @Connection: close@. For
-- HTTP\/1.1 clients, Snap will switch to the chunked transfer encoding if
-- @Content-Length@ is not specified.
--
-- Example:
--
-- @
-- ghci> setContentLength 400 'emptyResponse'
-- HTTP\/1.1 200 OK
-- Content-Length: 400
--
--
-- @
setContentLength    :: Word64 -> Response -> Response
setContentLength !l r = r { rspContentLength = Just l }
{-# INLINE setContentLength #-}


------------------------------------------------------------------------------
-- | Removes any @Content-Length@ set in the 'Response'.
--
-- Example:
--
-- @
-- ghci> clearContentLength $ 'setContentLength' 400 'emptyResponse'
-- HTTP\/1.1 200 OK
--
--
-- @
clearContentLength :: Response -> Response
clearContentLength r = r { rspContentLength = Nothing }
{-# INLINE clearContentLength #-}


                               ----------------
                               -- HTTP dates --
                               ----------------

------------------------------------------------------------------------------
-- | Convert a 'CTime' into an HTTP timestamp.
--
-- Example:
--
-- @
-- ghci> 'formatHttpTime' . 'fromIntegral' $ 10
-- \"Thu, 01 Jan 1970 00:00:10 GMT\"
-- @
formatHttpTime :: CTime -> IO ByteString


------------------------------------------------------------------------------
-- | Convert a 'CTime' into common log entry format.
formatLogTime :: CTime -> IO ByteString


------------------------------------------------------------------------------
-- | Converts an HTTP timestamp into a 'CTime'.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> 'parseHttpTime' \"Thu, 01 Jan 1970 00:00:10 GMT\"
-- 10
-- @
parseHttpTime :: ByteString -> IO CTime

#ifdef PORTABLE

------------------------------------------------------------------------------
-- local definitions
fromStr :: String -> ByteString
fromStr = S.pack                -- only because we know there's no unicode
{-# INLINE fromStr #-}


------------------------------------------------------------------------------
formatHttpTime = return . format . toUTCTime
  where
    format :: UTCTime -> ByteString
    format = fromStr . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"

    toUTCTime :: CTime -> UTCTime
    toUTCTime = posixSecondsToUTCTime . realToFrac


------------------------------------------------------------------------------
formatLogTime ctime = do
  t <- utcToLocalZonedTime $ toUTCTime ctime
  return $! format t

  where
    format :: ZonedTime -> ByteString
    format = fromStr . formatTime defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z"

    toUTCTime :: CTime -> UTCTime
    toUTCTime = posixSecondsToUTCTime . realToFrac


------------------------------------------------------------------------------
parseHttpTime = return . toCTime . prs . S.unpack
  where
    prs :: String -> Maybe UTCTime
    prs = parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

    toCTime :: Maybe UTCTime -> CTime
    toCTime (Just t) = fromInteger $ truncate $ utcTimeToPOSIXSeconds t
    toCTime Nothing  = fromInteger 0

#else

------------------------------------------------------------------------------
formatLogTime t = do
    ptr <- mallocBytes 40
    c_format_log_time t ptr
    S.unsafePackMallocCString ptr


------------------------------------------------------------------------------
formatHttpTime t = do
    ptr <- mallocBytes 40
    c_format_http_time t ptr
    S.unsafePackMallocCString ptr


------------------------------------------------------------------------------
parseHttpTime s = S.unsafeUseAsCString s $ \ptr ->
    c_parse_http_time ptr

#endif


------------------------------------------------------------------------------
-- | Adapted from:
--
-- <https://www.iana.org/assignments/http-status-codes/http-status-codes.txt>
statusReasonMap :: IM.IntMap ByteString
statusReasonMap = IM.fromList [
        (100, "Continue"),
        (101, "Switching Protocols"),
        (102, "Processing"),
        (103, "Early Hints"),
        -- 104-199 Unassigned
        (200, "OK"),
        (201, "Created"),
        (202, "Accepted"),
        (203, "Non-Authoritative Information"),
        (204, "No Content"),
        (205, "Reset Content"),
        (206, "Partial Content"),
        (207, "Multi-Status"),
        (208, "Already Reported"),
        -- 209-225 Unassigned
        (226, "IM Used"),
        -- 227-299 Unassigned,
        (300, "Multiple Choices"),
        (301, "Moved Permanently"),
        (302, "Found"),
        (303, "See Other"),
        (304, "Not Modified"),
        (305, "Use Proxy"),
        (306, "(Unused)"),
        (307, "Temporary Redirect"),
        (308, "Permanent Redirect"),
        -- 309-399 Unassigned
        (400, "Bad Request"),
        (401, "Unauthorized"),
        (402, "Payment Required"),
        (403, "Forbidden"),
        (404, "Not Found"),
        (405, "Method Not Allowed"),
        (406, "Not Acceptable"),
        (407, "Proxy Authentication Required"),
        (408, "Request Timeout"),
        (409, "Conflict"),
        (410, "Gone"),
        (411, "Length Required"),
        (412, "Precondition Failed"),
        (413, "Payload Too Large"),
        (414, "URI Too Long"),
        (415, "Unsupported Media Type"),
        (416, "Range Not Satisfiable"),
        (417, "Expectation Failed"),
        -- 418-420 Unassigned
        (421, "Misdirected Request"),
        (422, "Unprocessable Entity"),
        (423, "Locked"),
        (424, "Failed Dependency"),
        (425, "Too Early"),
        (426, "Upgrade Required"),
        -- 427 Unassigned
        (428, "Precondition Required"),
        (429, "Too Many Requests"),
        -- 430 Unassigned
        (431, "Request Header Fields Too Large"),
        -- 432-450 Unassigned
        (451, "Unavailable For Legal Reasons"),
        -- 452-499 Unassigned
        (500, "Internal Server Error"),
        (501, "Not Implemented"),
        (502, "Bad Gateway"),
        (503, "Service Unavailable"),
        (504, "Gateway Timeout"),
        (505, "HTTP Version Not Supported"),
        (506, "Variant Also Negotiates"),
        (507, "Insufficient Storage"),
        (508, "Loop Detected"),
        -- 509 Unassigned
        (510, "Not Extended"),
        (511, "Network Authentication Required")
        -- 512-599 Unassigned
    ]

------------------------------------------------------------------------------
-- Deprecated functions

-- | See 'rqClientAddr'.
rqRemoteAddr :: Request -> ByteString
rqRemoteAddr = rqClientAddr
{-# DEPRECATED rqRemoteAddr "(snap-core >= 1.0.0.0) please use 'rqClientAddr', this will be removed in 1.1.*" #-}

-- | See 'rqClientPort'.
rqRemotePort :: Request -> Int
rqRemotePort = rqClientPort
{-# DEPRECATED rqRemotePort "(snap-core >= 1.0.0.0) please use 'rqClientPort', this will be removed in 1.1.*" #-}
