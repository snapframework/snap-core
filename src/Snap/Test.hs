-- | The Snap.Test module contains primitives and combinators for testing Snap
-- applications.
module Snap.Test 
  ( -- * Combinators and types for creating test Requests

    -- ** Types
    HasBody(..)
  , RequestBuilder

    -- ** Building Requests and testing handlers
  , buildRequest
  , runHandler

    -- ** Precise control over building Requests
  , addHeader
  , addParam
  , formUrlEncoded
  , get
  , multipartEncoded
  , postMultipart
  , postUrlEncoded 
  , setFileParams
  , setHeader
  , setMethod
  , setParams
  , setRequestBody
  , setURI
  , useHttps

   -- * HUnit Assertions
  , assertSuccess
  , assert404
  , assertRedirectTo
  , assertRedirect
  , assertBodyContains

  )
  where

import           Snap.Internal.Test.Assertions
import           Snap.Internal.Test.RequestBuilder

  
