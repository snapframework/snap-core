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
          testSetMethod
        , testSetParam
        , testSetParams
        , testSetBody
        , testSetHeader
        ]

  
testSetMethod :: Test
testSetMethod = testCase "test/requestBuilder/setMethod" $ do
  request <- buildRequest $ do
               setMethod PUT
  assertEqual "RequestBuilder setMethod not working" PUT (rqMethod request)

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

testSetBody :: Test
testSetBody = testCase "test/requestBuilder/setBody" $ do
  request <- buildRequest $ do
               setBody "Hello World"
  body <- getBody request
  assertEqual "RequestBuilder setBody not working" 
              "Hello World"
              body

testSetHeader :: Test
testSetHeader = testCase "test/requestBuilder/setHeader" $ do
  request <- buildRequest $ do
               setHeader "Accepts" "application/json"
  assertEqual "RequestBuilder setHeader not working" 
              (Just ["application/json"])
              (Map.lookup "Accepts" (rqHeaders request))

