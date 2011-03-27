{-# LANGUAGE OverloadedStrings          #-}
module Snap.Test.RequestBuilder.Tests 
  (
  tests
  ) where

import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Control.Monad.Trans (liftIO)

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertEqual, assertBool)

import           Snap.Internal.Http.Types (Request(..))
import qualified Snap.Internal.Http.Types as T
import           Snap.Types hiding (setHeader, addHeader)
import           Snap.Internal.Test.RequestBuilder

tests :: [Test]
tests = [
          testSetRequestType
        , testAddParam
        , testSetParams
        , testSetRequestBody
        , testSetHeader
        , testAddHeader
        , testBuildQueryString
        , testFormUrlEncoded
        , testBuildMultipartString 
        , testMultipartEncoded 
        , testUseHttps
        , testSetURI
        , testRunHandler
        ]

testSetRequestType :: Test
testSetRequestType = testCase "test/requestBuilder/setRequestType" $ do 
    request <- buildRequest $ do
                 setRequestType GetRequest
    assertEqual "RequestBuilder setRequestType not working" GET (rqMethod request)

testAddParam :: Test
testAddParam = testCase "test/requestBuilder/addParam" $ do
  request <- buildRequest $ do
               addParam "name" "John"
               addParam "age"  "26"
  assertEqual "RequestBuilder addParam not working" 
              (Map.fromList [("name", ["John"]), ("age", ["26"])]) 
              (rqParams request)

testSetParams :: Test
testSetParams = testCase "test/requestBuilder/setParams" $ do
  request <- buildRequest $ do
              setParams [("name", "John"), ("age", "26")]
  assertEqual "RequestBuilder setParams not working" 
              (Map.fromList [("name", ["John"]), ("age", ["26"])]) 
              (rqParams request)

testSetRequestBody :: Test
testSetRequestBody = testCase "test/requestBuilder/setBody" $ do
  request <- buildRequest $ do
               -- setRequestBody sets the PUT Method on the Request
               setRequestBody "Hello World"
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

testAddHeader :: Test
testAddHeader = testCase "test/requestBuilder/addHeader" $ do
  request <- buildRequest $ do
               addHeader "X-Forwaded-For" "127.0.0.1"
               addHeader "X-Forwaded-For" "192.168.1.0"
  assertEqual "RequestBuilder addHeader not working"
              (Just ["192.168.1.0", "127.0.0.1"])
              (Map.lookup "X-Forwaded-For" (rqHeaders request))

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
                formUrlEncoded GET
  assertEqual "RequestBuilder formUrlEncoded not working"
              (Just ["x-www-form-urlencoded"])
              (Map.lookup "Content-Type" (rqHeaders request1))

  request2 <- buildRequest $ do
                formUrlEncoded GET
                setParams [("name", "John"), ("age", "21")]
  assertEqual "RequestBuilder formUrlEncoded invalid query string" 
              "age=21&name=John"
              (rqQueryString request2)

  request3 <- buildRequest $ do
                formUrlEncoded POST
                setParams [("name", "John"), ("age", "21")]
  body3    <- getBody request3
  assertEqual "RequestBuilder formUrlEncoded invalid query string on body"
              "age=21&name=John"
              body3

testBuildMultipartString :: Test
testBuildMultipartString = testCase "test/requestBuilder/buildMultipartString" $ do
  let params = Map.fromList [
                 ("name", ["John"])
               , ("age" , ["25"])
               ]

  let fParams1 = Map.fromList [
                   ("photo", [ ("photo1.jpg", "Image Content")
                             , ("photo2.png", "Some Content")
                             ]
                   )
                 , ("document", [("document.pdf", "Some Content")])
                 ]

  let result1 = buildMultipartString 
                  "Boundary" 
                  ""  
                  params
                  Map.empty
  assertEqual "buildMultipartString not working with simple params"
              "--Boundary\r\n\
              \Content-Disposition: form-data; name=\"age\"\r\n\
              \\r\n\
              \25\r\n\
              \--Boundary\r\n\
              \Content-Disposition: form-data; name=\"name\"\r\n\
              \\r\n\
              \John\r\n\
              \--Boundary--"
              result1

  let result2 = buildMultipartString
                  "Boundary"
                  "FileBoundary"
                  Map.empty
                  fParams1
    
  assertEqual "buildMultipartString not working with files"
              "--Boundary\r\n\
              \Content-Disposition: form-data; name=\"document\"; filename=\"document.pdf\"\r\n\
              \Content-Type: application/pdf\r\n\
              \\r\n\
              \Some Content\r\n\
              \--Boundary\r\n\
              \Content-Disposition: form-data; name=\"photo\"\r\n\
              \Content-Type: multipart/mixed; boundary=FileBoundary\r\n\
              \\r\n\
              \--FileBoundary\r\n\
              \Content-Disposition: photo; filename=\"photo1.jpg\"\r\n\
              \Content-Type: image/jpeg\r\n\
              \\r\n\
              \Image Content\r\n\
              \--FileBoundary\r\n\
              \Content-Disposition: photo; filename=\"photo2.png\"\r\n\
              \Content-Type: image/png\r\n\
              \\r\n\
              \Some Content\r\n\
              \--FileBoundary--\r\n\
              \--Boundary--"
              result2


testMultipartEncoded :: Test
testMultipartEncoded = testCase "test/requestBuilder/multipartEncoded" $ do
  request <- buildRequest $ do
               multipartEncoded []
               setParams [("name", "John"), ("age", "21")]

  let contentType = fromJust $ T.getHeader "Content-Type" request
  let boundary    = S.tail $ S.dropWhile (/= '=') contentType 
  
  body    <- getBody request
  assertEqual "RequestBuilder multipartEncoded not working"
              (buildMultipartString boundary "" (rqParams request) Map.empty)
              body
            
testUseHttps :: Test
testUseHttps = testCase "test/requestBuilder/useHttps" $ do
  request <- buildRequest $ do
               useHttps
  assertBool "RequestBuilder useHttps not working" (rqIsSecure request)


testSetURI :: Test
testSetURI = testCase "test/requestBuilder/setURI" $ do
  request1 <- buildRequest $ do
               setURI "/users"

  assertEqual "RequestBuilder setURI is not working" 
              "/users" 
              (rqURI request1)

  request2 <- buildRequest $ do
                formUrlEncoded GET
                setURI "/users"
                addParam "name" "John"
                addParam "age"  "25"

  assertEqual "RequestBuilder setURI is not working with a Query String"
              "/users?age=25&name=John"
              (rqURI request2)

testRunHandler :: Test
testRunHandler = testCase "test/requestBuilder/runHandler" $ do
  let handler = method POST $ path "/my-handler" $ do
                  name <- getParam "name"
                  liftIO $ assertEqual "runHandler not working"
                                       (Just "John")
                                       name
                  modifyResponse (setResponseCode 200)
  
  response <- runHandler handler $ do
                postUrlEncoded "/my-handler"
                               [("name", "John")]

  assertEqual "runHandler not working" 200 (rspStatus response)

