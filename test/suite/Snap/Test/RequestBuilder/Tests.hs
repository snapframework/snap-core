{-# LANGUAGE OverloadedStrings          #-}
module Snap.Test.RequestBuilder.Tests 
  (
  tests
  ) where

import qualified Data.Map as Map

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertEqual, assertBool)

import           Snap.Internal.Http.Types (Request(..), Method(..))
import           Snap.Internal.Test.RequestBuilder

tests :: [Test]
tests = [
          testHttpMethod
        , testSetParam
        , testSetParams
        , testHttpBody
        , testHttpHeader
        ]

  
testHttpMethod :: Test
testHttpMethod = testCase "test/requestBuilder/httpMethod" $ do
  request <- buildRequest $ do
               httpMethod PUT
  assertEqual "RequestBuilder httpMethod not working" PUT (rqMethod request)

testSetParam :: Test
testSetParam = testCase "test/requestBuilder/setParam" $ do
  request <- buildRequest $ do
               setParam "name" "John"
               setParam "age"  "26"
  assertEqual "RequestBuilder setParam not working" 
              (Map.fromList [("name", ["John"]), ("age", ["26"])]) 
              (rqParams request)

testSetParams :: Test
testSetParams = testCase "test/requestBuilder/setParams" $ do
  request <- buildRequest $ do
              setParams [("name", "John"), ("age", "26")]
  assertEqual "RequestBuilder setParams not working" 
              (Map.fromList [("name", ["John"]), ("age", ["26"])]) 
              (rqParams request)

testHttpBody :: Test
testHttpBody = testCase "test/requestBuilder/httpBody" $ do
  request <- buildRequest $ do
               httpBody "Hello World"
  body <- getBody request
  assertEqual "RequestBuilder httpBody not working" 
              "Hello World"
              body

testHttpHeader :: Test
testHttpHeader = testCase "test/requestBuilder/httpHeader" $ do
  request <- buildRequest $ do
               httpHeader "Accepts" "application/json"
  assertEqual "RequestBuilder httpHeader not working" 
              (Just ["application/json"])
              (Map.lookup "Accepts" (rqHeaders request))

