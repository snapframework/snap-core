module Snap.Test.RequestBuilder.Tests 
  (
  tests
  ) where


import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertEqual, assertBool)

import           Snap.Internal.Http.Types (Request(..), Method(..))
import           Snap.Internal.Test.RequestBuilder

tests :: [Test]
tests = [
          testHttpMethodModifier
        ]

  
testHttpMethodModifier :: Test
testHttpMethodModifier = testCase "test/requestBuilder/httpMethod" $ do
  request <- buildRequest $ do
               httpMethod PUT
  assertEqual "RequestBuilder method not working" PUT (rqMethod request)

