module Snap.Test 
  (
    module Snap.Internal.Test.Assertions 
  , runHandler
  , setMethod
  , addParam
  , setParams
  , setFileParams
  , setRequestBody
  , setHeader
  , addHeader
  , formUrlEncoded
  , multipartEncoded
  , useHttps
  , get
  , postUrlEncoded 
  , postMultipart
  )
  where

import           Snap.Internal.Test.Assertions
import           Snap.Internal.Test.RequestBuilder

  
