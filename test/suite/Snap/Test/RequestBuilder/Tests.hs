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
        , testBuildQueryString
        , testFormUrlEncoded
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
               setMethod PUT
               setBody "Hello World"
  body <- getBody request
  assertEqual "RequestBuilder setBody not working with PUT method" 
              "Hello World"
              body

testSetHeader :: Test
testSetHeader = testCase "test/requestBuilder/setHeader" $ do
  request <- buildRequest $ do
               setHeader "Accepts" "application/json"
  assertEqual "RequestBuilder setHeader not working" 
              (Just ["application/json"])
              (Map.lookup "Accepts" (rqHeaders request))


testBuildQueryString :: Test
testBuildQueryString = testCase "test/requestBuilder/buildQueryString" $ do
  let qs1 = buildQueryString $ 
              Map.fromList [("name", ["John"]), ("age", ["25"])]
  assertEqual "buildQueryString not working"
              "age=25&name=John"
              qs1

  let qs2 = buildQueryString $
              Map.fromList [ ("spaced param", ["Wild%!Char'ters"])
                           , ("!what\"", ["doyou-think?"])
                           ]
  assertEqual "buildQueryString not working"
              "!what%22=doyou-think%3f&spaced+param=Wild%25!Char'ters"
              qs2

testFormUrlEncoded :: Test
testFormUrlEncoded = testCase "test/requestBuilder/formUrlEncoded" $ do
  request1 <- buildRequest $ do
                formUrlEncoded
  assertEqual "RequestBuilder formUrlEncoded not working"
              (Just ["x-www-form-urlencoded"])
              (Map.lookup "Content-Type" (rqHeaders request1))

  request2 <- buildRequest $ do
                setMethod GET
                formUrlEncoded
                setParams [("name", "John"), ("age", "21")]
  assertEqual "RequestBuilder formUrlEncoded invalid query string" 
              "age=21&name=John"
              (rqQueryString request2)

  request3 <- buildRequest $ do
                setMethod POST
                formUrlEncoded
                setParams [("name", "John"), ("age", "21")]
  body3    <- getBody request3
  assertEqual "RequestBuilder formUrlEncoded invalid query string on body"
              "age=21&name=John"
              body3

            

