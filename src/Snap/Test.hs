-- | The Snap.Test module contains primitives and combinators for testing Snap
-- applications.
module Snap.Test
  ( -- * Combinators and types for testing Snap handlers.

    -- ** Types
    RequestBuilder
  , MultipartParams
  , MultipartParam(..)
  , FileData      (..)
  , RequestType   (..)

    -- ** Building Requests and testing handlers
  , buildRequest
  , runHandler
  , runHandlerM
  , evalHandler
  , evalHandlerM

    -- *** Convenience functions for generating common types of HTTP requests
  , get
  , postUrlEncoded
  , postMultipart
  , put
  , postRaw
  , delete

    -- *** Precise control over building Requests
  , addHeader
  , setContentType
  , setHeader
  , setHttpVersion
  , setQueryString
  , setQueryStringRaw
  , setRequestPath
  , setRequestType
  , setSecure

   -- * HUnit Assertions
  , assertSuccess
  , assert404
  , assertRedirectTo
  , assertRedirect
  , assertBodyContains

   -- * Getting response bodies
  , getResponseBody

   -- * Dumping HTTP Responses
  , dumpResponse
  , responseToString
  )
  where

import           Snap.Internal.Test.Assertions
import           Snap.Internal.Test.RequestBuilder


