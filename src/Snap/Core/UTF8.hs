{-|

This module contains the core type definitions, class instances, and functions
for HTTP as well as the 'Snap' monad, which is used for web handlers.

-}
module Snap.Core.UTF8
  (
    -- * The Snap Monad
    S.Snap
  , S.runSnap
  , S.MonadSnap(..)
  , S.NoHandlerException(..)

    -- ** Functions for control flow and early termination
  , S.bracketSnap
  , S.finishWith
  , S.catchFinishWith
  , S.pass
  , S.terminateConnection

    -- *** Escaping HTTP
  , S.EscapeHttpHandler
  , S.escapeHttp

    -- ** Routing
  , path
  , S.pathArg
  , dir
  , S.ifTop
  , route
  , routeLocal

    -- ** Access to state
  , S.getRequest
  , S.getsRequest
  , S.getResponse
  , S.getsResponse
  , S.putRequest
  , S.putResponse
  , S.modifyRequest
  , S.modifyResponse
  , S.localRequest
  , S.withRequest
  , S.withResponse

    -- ** Logging
  , logError

    -- ** Grabbing/transforming request bodies
  , S.runRequestBody
  , getRequestBody
  , readRequestBody
  , S.transformRequestBody

    -- * HTTP Datatypes and Functions
    -- $httpDoc
    --
  , S.Request
  , S.Response
  , S.Headers
  , S.HasHeaders(..)
  , S.Params
  , S.Method(..)
  , S.Cookie(..)
  , S.HttpVersion

    -- ** Headers
  , addHeader
  , setHeader
  , getHeader
  , getHeaders
  , listHeaders
  , deleteHeader
  , S.ipHeaderFilter
  , ipHeaderFilter'

    -- ** Requests
  , rqServerName
  , S.rqServerPort
  , rqRemoteAddr
  , S.rqRemotePort
  , rqLocalAddr
  , rqLocalHostname
  , S.rqIsSecure
  , S.rqContentLength
  , S.rqMethod
  , S.rqVersion
  , S.rqCookies
  , rqPathInfo
  , rqContextPath
  , rqURI
  , rqQueryString
  , S.rqParams
  , S.rqQueryParams
  , S.rqPostParams
  , rqParam
  , rqPostParam
  , rqQueryParam
  , getParam
  , getPostParam
  , getQueryParam
  , S.getParams
  , S.getPostParams
  , S.getQueryParams
  , S.rqModifyParams
  , rqSetParam

    -- ** Responses
  , S.emptyResponse
  , S.setResponseCode
  , setResponseStatus
  , S.rspStatus
  , rspStatusReason
  , setContentType
  , S.addResponseCookie
  , getResponseCookie
  , S.getResponseCookies
  , deleteResponseCookie
  , modifyResponseCookie
  , expireCookie
  , getCookie
  , readCookie
  , S.setContentLength
  , S.clearContentLength
  , redirect
  , redirect'
  , S.setBufferingMode
  , S.getBufferingMode

    -- *** Response I/O
  , S.setResponseBody
  , S.modifyResponseBody
  , S.addToOutput
  , S.writeBuilder
  , S.writeBS
  , S.writeLazyText
  , S.writeText
  , S.writeLBS
  , S.sendFile
  , S.sendFilePartial

    -- ** Timeouts
  , S.setTimeout
  , S.extendTimeout
  , S.modifyTimeout
  , S.getTimeoutAction
  , S.getTimeoutModifier

    -- * Iteratee
  , S.Enumerator
  , S.SomeEnumerator(..)

    -- * HTTP utilities
  , formatHttpTime
  , parseHttpTime
  , parseUrlEncoded
  , buildUrlEncoded
  , printUrlEncoded
  , urlEncode
  , urlEncodeBuilder
  , urlDecode
  ) where

------------------------------------------------------------------------------
import           Control.Arrow
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Snap.Internal.Exceptions as S
import qualified Snap.Internal.Http.Types as S
import qualified Snap.Internal.Instances as S
import qualified Snap.Internal.Parsing as S
import qualified Snap.Internal.Routing as S
import qualified Snap.Internal.Types as S
import qualified Snap.Iteratee as S
import qualified Snap.Types.Headers as S
------------------------------------------------------------------------------

-- $httpDoc
-- HTTP-related datatypes: 'Request', 'Response', 'Cookie', etc.

enc = encodeUtf8
dec = decodeUtf8

-- I think these are all the functions that need to have UTF8 versions:

------------------------------------------------------------------------------
-- | 
path :: S.MonadSnap m => Text -> m a -> m a
path p = S.path (enc p)

dir :: S.MonadSnap m => Text -> m a -> m a
dir d = S.dir (enc d)

route :: S.MonadSnap m => [(Text, m a)] -> m a
route pairs = S.route (map (first enc) pairs)

routeLocal :: S.MonadSnap m => [(Text, m a)] -> m a
routeLocal pairs = S.routeLocal (map (first enc) pairs)

logError :: S.MonadSnap m => Text -> m ()
logError err = S.logError (enc err)

getRequestBody = undefined
readRequestBody = undefined
addHeader = undefined
setHeader = undefined
getHeader = undefined
getHeaders = undefined
listHeaders = undefined
deleteHeader = undefined
ipHeaderFilter' = undefined
rqServerName = undefined
rqRemoteAddr = undefined
rqLocalAddr = undefined
rqLocalHostname = undefined
rqPathInfo = undefined
rqContextPath = undefined
rqURI = undefined
rqQueryString = undefined
rqParam = undefined
rqPostParam = undefined
rqQueryParam = undefined
getParam = undefined
getPostParam = undefined
getQueryParam = undefined
rqSetParam = undefined
setResponseStatus = undefined
rspStatusReason = undefined
setContentType = undefined
getResponseCookie = undefined
deleteResponseCookie = undefined
modifyResponseCookie = undefined
expireCookie = undefined
getCookie = undefined
readCookie = undefined
redirect = undefined
redirect' = undefined
formatHttpTime = undefined
parseHttpTime = undefined
parseUrlEncoded = undefined
buildUrlEncoded = undefined
printUrlEncoded = undefined
urlEncode = undefined
urlEncodeBuilder = undefined
urlDecode = undefined
