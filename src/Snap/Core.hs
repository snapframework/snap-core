{-|

This module contains the core type definitions, class instances, and functions
for HTTP as well as the 'Snap' monad, which is used for web handlers.

-}
module Snap.Core
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

    -- *** Escaping HTTP
  , EscapeHttpHandler
  , EscapeSnap(..)
  , escapeHttp
  , terminateConnection

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
  , getsRequest
  , getResponse
  , getsResponse
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
  , readRequestBody
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
  , listHeaders
  , deleteHeader
  , ipHeaderFilter
  , ipHeaderFilter'

    -- ** Requests
  , rqHeaders
  , rqHostName
  , rqClientAddr
  , rqClientPort
  , rqServerAddr
  , rqServerPort
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
  , rqQueryParams
  , rqPostParams
  , rqParam
  , rqPostParam
  , rqQueryParam
  , getParam
  , getPostParam
  , getQueryParam
  , getParams
  , getPostParams
  , getQueryParams
  , rqModifyParams
  , rqSetParam

    -- *** Deprecated functions
  , rqRemoteAddr
  , rqRemotePort

    -- ** Responses
  , emptyResponse
  , setResponseCode
  , setResponseStatus
  , rspStatus
  , rspStatusReason
  , setContentType
  , addResponseCookie
  , getResponseCookie
  , getResponseCookies
  , deleteResponseCookie
  , modifyResponseCookie
  , expireCookie
  , getCookie
  , readCookie
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
  , extendTimeout
  , modifyTimeout
  , getTimeoutModifier

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
import           Snap.Internal.Core       (EscapeHttpHandler, EscapeSnap (..), MonadSnap (..), NoHandlerException (..), Snap, addToOutput, bracketSnap, catchFinishWith, dir, escapeHttp, expireCookie, extendTimeout, finishWith, getCookie, getParam, getParams, getPostParam, getPostParams, getQueryParam, getQueryParams, getRequest, getResponse, getTimeoutModifier, getsRequest, getsResponse, ifTop, ipHeaderFilter, ipHeaderFilter', localRequest, logError, method, methods, modifyRequest, modifyResponse, modifyTimeout, pass, path, pathArg, putRequest, putResponse, readCookie, readRequestBody, redirect, redirect', runRequestBody, runSnap, sendFile, sendFilePartial, setTimeout, terminateConnection, transformRequestBody, withRequest, withResponse, writeBS, writeBuilder, writeLBS, writeLazyText, writeText)
import           Snap.Internal.Http.Types (Cookie (..), HasHeaders (..), HttpVersion, Method (..), Params, Request (rqClientAddr, rqClientPort, rqContentLength, rqContextPath, rqCookies, rqHeaders, rqHostName, rqIsSecure, rqLocalHostname, rqMethod, rqParams, rqPathInfo, rqPostParams, rqQueryParams, rqQueryString, rqServerAddr, rqServerPort, rqURI, rqVersion), Response (rspStatus, rspStatusReason), addHeader, addResponseCookie, clearContentLength, deleteHeader, deleteResponseCookie, emptyResponse, formatHttpTime, getHeader, getResponseCookie, getResponseCookies, listHeaders, modifyResponseBody, modifyResponseCookie, parseHttpTime, rqModifyParams, rqParam, rqPostParam, rqQueryParam, rqRemoteAddr, rqRemotePort, rqSetParam, setContentLength, setContentType, setHeader, setResponseBody, setResponseCode, setResponseStatus)
import           Snap.Internal.Instances  ()
import           Snap.Internal.Parsing    (buildUrlEncoded, parseUrlEncoded, printUrlEncoded, urlDecode, urlEncode, urlEncodeBuilder)
import           Snap.Internal.Routing    (route, routeLocal)
import           Snap.Types.Headers       (Headers)
------------------------------------------------------------------------------

-- $httpDoc
-- HTTP-related datatypes: 'Request', 'Response', 'Cookie', etc.
