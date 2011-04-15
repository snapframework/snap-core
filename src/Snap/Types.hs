{-|

This module contains the core type definitions, class instances, and functions
for HTTP as well as the 'Snap' monad, which is used for web handlers.

-}
module Snap.Types
  (
    -- * The Snap Monad
    Snap
  , runSnap
  , MonadSnap(..)
  , NoHandlerException(..)

    -- ** Functions for control flow and early termination
  , bracketSnap
  , finishWith
  , catchFinishWith
  , pass

    -- ** Routing
  , method
  , methods
  , path
  , pathArg
  , dir
  , ifTop
  , route
  , routeLocal

    -- ** Access to state
  , getRequest
  , getResponse
  , putRequest
  , putResponse
  , modifyRequest
  , modifyResponse
  , localRequest
  , withRequest
  , withResponse

    -- ** Logging
  , logError

    -- ** Grabbing/transforming request bodies
  , runRequestBody
  , getRequestBody
  , transformRequestBody

    -- * HTTP Datatypes and Functions
    -- $httpDoc
    --
  , Request
  , Response
  , Headers
  , HasHeaders(..)
  , Params
  , Method(..)
  , Cookie(..)
  , HttpVersion

    -- ** Headers
  , addHeader
  , setHeader
  , getHeader
  , deleteHeader
  , ipHeaderFilter
  , ipHeaderFilter'

    -- ** Requests
  , rqServerName
  , rqServerPort
  , rqRemoteAddr
  , rqRemotePort
  , rqLocalAddr
  , rqLocalHostname
  , rqIsSecure
  , rqContentLength
  , rqMethod
  , rqVersion
  , rqCookies
  , rqPathInfo
  , rqContextPath
  , rqURI
  , rqQueryString
  , rqParams
  , rqParam
  , getParam
  , getParams
  , rqModifyParams
  , rqSetParam

    -- ** Responses
  , emptyResponse
  , setResponseCode
  , setResponseStatus
  , rspStatus
  , rspStatusReason
  , setContentType
  , addCookie
  , addResponseCookie
  , getResponseCookie
  , getResponseCookies
  , deleteResponseCookie
  , modifyResponseCookie
  , getCookie
  , setContentLength
  , clearContentLength
  , redirect
  , redirect'

    -- *** Response I/O
  , setResponseBody
  , modifyResponseBody
  , addToOutput
  , writeBuilder
  , writeBS
  , writeLazyText
  , writeText
  , writeLBS
  , sendFile
  , sendFilePartial

    -- ** Timeouts
  , setTimeout
  , getTimeoutAction

    -- * Iteratee
  , Enumerator
  , SomeEnumerator(..)

    -- * HTTP utilities
  , formatHttpTime
  , parseHttpTime
  , urlEncode
  , urlDecode
  ) where

------------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Instances ()
import           Snap.Internal.Routing
import           Snap.Internal.Types
import           Snap.Iteratee (Enumerator)
------------------------------------------------------------------------------

-- $httpDoc
-- HTTP-related datatypes: 'Request', 'Response', 'Cookie', etc.
