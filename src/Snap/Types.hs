{-|

This module contains the core type definitions, class instances, and functions
for HTTP as well as the 'Snap' monad, which is used for web handlers.

-}
module Snap.Types
  ( 
    -- * The Snap Monad
    Snap
  , runSnap
  , NoHandlerException(..)

    -- ** Functions for control flow and early termination
  , finishWith
  , pass

    -- ** Routing
  , method
  , path
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

    -- ** Grabbing request bodies
  , runRequestBody
  , getRequestBody
  , unsafeDetachRequestBody
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
  , rqModifyParams
  , rqSetParam

    -- ** Responses
  , emptyResponse
  , setResponseStatus
  , rspStatus
  , rspStatusReason
  , setContentType
  , addCookie
  , setContentLength
  , clearContentLength

    -- *** Response I/O
  , setResponseBody
  , modifyResponseBody
  , addToOutput
  , writeBS
  , writeLazyText
  , writeText
  , writeLBS
  , sendFile

    -- * Iteratee
  , Enumerator

    -- * HTTP utilities
  , formatHttpTime
  , parseHttpTime 
  , urlEncode
  , urlDecode
  ) where

------------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Routing
import           Snap.Internal.Types
------------------------------------------------------------------------------

-- $httpDoc
-- HTTP-related datatypes: 'Request', 'Response', 'Cookie', etc.
