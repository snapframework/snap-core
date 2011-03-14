{-# LANGUAGE OverloadedStrings #-}
module Snap.Test.Assertions.Tests 
  (
  tests
  )
  where

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Snap.Internal.Test.RequestBuilder
import           Snap.Types
import           Snap.Internal.Test.Assertions

tests :: [Test]
tests = [
          testAssertSuccess
        , testAssert404
        , testAssertRedirect
        , testAssertRedirectTo
        , testAssertBodyContains
        ]

testAssertSuccess :: Test
testAssertSuccess = testCase "test/assertions/assertSuccess" $ do
  let action = method GET $ path "/success" $ do
                 modifyResponse (setResponseCode 200)

  response <- runHandler action $ do
                get "/success" []
  assertSuccess response

testAssert404 :: Test
testAssert404 = testCase "test/assertions/assert404" $ do
  let action = method GET $ path "/404" $ do
                 modifyResponse (setResponseCode 404)

  response <- runHandler action $ do
                get "/404" []
  
  response' <- runHandler action $ do
                 postUrlEncoded "/not-existing" []

  assert404 response
  assert404 response'

testAssertRedirect :: Test
testAssertRedirect = testCase "test/assertions/assertRedirect" $ do
  let action = method GET $ path "/redirection" $ do
                 redirect "http://www.google.com"
  
  response <- runHandler action $ do
                get "/redirection" []

  assertRedirect response

testAssertRedirectTo :: Test
testAssertRedirectTo = testCase "test/assertions/assertRedirectTo" $ do
  let action = method GET $ path "/redirection" $ do
                 redirect "http://www.google.com"

  response <- runHandler action $ do
                get "/redirection" []

  assertRedirectTo "http://www.google.com" response 

testAssertBodyContains :: Test
testAssertBodyContains = testCase "test/assertions/assertBodyContains" $ do
  let action = method GET $ path "/body-contains" $ do
                 writeBS "This is a match we have to check out" 
                 modifyResponse (setResponseCode 200)

  response <- runHandler action $ do
                get "/body-contains" []

  assertBodyContains "^This.*match.*out$" response


