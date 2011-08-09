{-# LANGUAGE OverloadedStrings #-}

module Snap.Test.RequestBuilder.Tests
  ( tests
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import           Data.IORef
import qualified Data.Map as Map
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertEqual, assertBool)
import           Text.Regex.Posix ((=~))
------------------------------------------------------------------------------
import           Snap.Internal.Http.Types (Request(..))
import qualified Snap.Internal.Http.Types as T
import           Snap.Test
import           Snap.Iteratee
import           Snap.Types hiding (setHeader, addHeader, setContentType)
import           Snap.Util.FileUploads

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testSetRequestType
        , testSetQueryString
        , testSetQueryStringRaw
        , testHeaders
        , testMisc
        , testMultipart
        , testPost
        , testToString
        , testAssert404
        , testAssertBodyContains
        , testAssertRedirect
        ]

{-
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
-}

------------------------------------------------------------------------------
testSetRequestType :: Test
testSetRequestType = testCase "test/requestBuilder/setRequestType" $ do
    request1 <- buildRequest $ setRequestType GetRequest
    assertEqual "setRequestType/1" GET (rqMethod request1)

    request2 <- buildRequest $ setRequestType DeleteRequest
    assertEqual "setRequestType/2" DELETE (rqMethod request2)

    request3 <- buildRequest $ setRequestType $ RequestWithRawBody PUT "foo"
    assertEqual "setRequestType/3/Method" PUT (rqMethod request3)

    rqBody3  <- getRqBody request3
    assertEqual "setRequestType/3/Body" "foo" rqBody3

    request4 <- buildRequest $ setRequestType rt4
    assertEqual "setRequestType/4/Method" POST (rqMethod request4)
    -- will test correctness of multipart generation code in another test

    request5 <- buildRequest $ setRequestType $
                UrlEncodedPostRequest $ Map.fromList [("foo", ["foo"])]
    assertEqual "setRequestType/5/Method" POST (rqMethod request5)

  where
    rt4 = MultipartPostRequest [ ("foo", FormData ["foo"])
                               , ("bar", Files [fd4])
                               ]

    fd4 = FileData "bar.txt" "text/plain" "bar"


------------------------------------------------------------------------------
testSetQueryString :: Test
testSetQueryString = testCase "test/requestBuilder/testSetQueryString" $ do
    request <- buildRequest $ get "/" params
    assertEqual "setQueryString" params $ rqParams request
    assertEqual "queryString" "bar=bar&foo=foo&foo=foo2" $
                rqQueryString request

  where
    params = Map.fromList [ ("foo", ["foo", "foo2"])
                          , ("bar", ["bar"]) ]


------------------------------------------------------------------------------
testSetQueryStringRaw :: Test
testSetQueryStringRaw = testCase "test/requestBuilder/testSetQueryStringRaw" $ do
    request <- buildRequest $ do
                   get "/" Map.empty
                   setQueryStringRaw "foo=foo&foo=foo2&bar=bar"
    assertEqual "setQueryStringRaw" params $ rqParams request

  where
    params = Map.fromList [ ("foo", ["foo", "foo2"])
                          , ("bar", ["bar"]) ]


------------------------------------------------------------------------------
testHeaders :: Test
testHeaders = testCase "test/requestBuilder/testHeaders" $ do
    request <- buildRequest $ do
                   get "/" Map.empty
                   setHeader "foo" "foo"
                   addHeader "bar" "bar"
                   addHeader "bar" "bar2"
                   setContentType "image/gif"  -- this should get deleted
    assertEqual "setHeader" (Just "foo") $ getHeader "foo" request
    assertEqual "addHeader" (Just ["bar","bar2"]) $ T.getHeaders "bar" request
    assertEqual "contentType" Nothing $ T.getHeader "Content-Type" request
    assertEqual "contentLength" Nothing $ rqContentLength request
    assertEqual "contentLengthHdr" Nothing $ getHeader "Content-Length" request

    request2 <- buildRequest $ put "/" "text/zzz" "zzz"
    assertEqual "contentType2" (Just "text/zzz") $
                T.getHeader "Content-Type" request2
    assertEqual "contentLength" (Just 3) $ rqContentLength request2
    assertEqual "contentLengthHdr" (Just "3") $
                getHeader "Content-Length" request2


------------------------------------------------------------------------------
testMisc :: Test
testMisc = testCase "test/requestBuilder/testMisc" $ do
    request <- buildRequest $ do
        get "/" Map.empty
        setSecure True
        setRequestPath "/foo/bar"

    assertEqual "secure" True $ rqIsSecure request
    assertEqual "rqPathInfo" "/foo/bar" $ rqPathInfo request
    assertEqual "rqURI" "/foo/bar" $ rqURI request
    assertEqual "rqContextPath" "" $ rqContextPath request
    assertEqual "rqVersion" (1,1) $ rqVersion request

    body <- getRqBody request
    assertEqual "body" "" body

    request2 <- buildRequest $ do
        postRaw "/" "text/zzz" "zzz"
        setHttpVersion (1,0)

    body2 <- getRqBody request2
    assertEqual "body2" "zzz" body2

    assertEqual "contentType2" (Just "text/zzz") $
                T.getHeader "Content-Type" request2
    assertEqual "postRaw" POST $ rqMethod request2
    assertEqual "rqVersion2" (1,0) $ rqVersion request2

    request3 <- buildRequest $ do
        delete "/" Map.empty
        setSecure True
        setRequestPath "/foo/bar"

    assertEqual "secure" True $ rqIsSecure request3
    assertEqual "method" DELETE $ rqMethod request3
    assertEqual "rqPathInfo" "/foo/bar" $ rqPathInfo request3
    assertEqual "rqURI" "/foo/bar" $ rqURI request3
    assertEqual "rqContextPath" "" $ rqContextPath request3
    assertEqual "rqVersion" (1,1) $ rqVersion request3


------------------------------------------------------------------------------
testMultipart :: Test
testMultipart = testCase "test/requestBuilder/testMultipart" $ do
    request  <- buildRequest rq
    rbody    <- getRqBody request
    assertEqual "content-length" (Just (S.length rbody)) $
                rqContentLength request

    response <- runHandler rq handler
    body     <- getResponseBody response
    assertEqual "body" "OK" body

  where
    partHandler (PartInfo field fn ct) = do
        body <- liftM S.concat consume
        return (field, fn, ct, body)

    expectedParts = [ ("bar", Just "bar1.txt", "text/plain", "bar")
                    , ("bar", Just "bar2.txt", "text/zzz", "bar2")
                    , ("baz", Just "baz.gif", "text/gif", "baz") ]

    handler = do
        parts <- handleMultipart defaultUploadPolicy partHandler
        fooParam <- getParam "foo"
        liftIO $ assertEqual "param" (Just "oof") fooParam

        quuxParams <- getParam "quux"
        liftIO $ assertEqual "quux" (Just "quux1 quux2") quuxParams
        liftIO $ assertEqual "parts" expectedParts parts
        writeBS "OK"

    rq = postMultipart "/" rt

    rt = [ ("foo", FormData ["oof"])
         , ("bar", Files [fb1, fb2])
         , ("baz", Files [fz])
         , ("zzz", Files [])
         , ("zz0", FormData [])
         , ("quux", FormData ["quux1", "quux2"])
         ]

    fb1 = FileData "bar1.txt" "text/plain" "bar"
    fb2 = FileData "bar2.txt" "text/zzz" "bar2"
    fz = FileData "baz.gif" "text/gif" "baz"


------------------------------------------------------------------------------
testPost :: Test
testPost = testCase "test/requestBuilder/testPost" $ do
    request <- buildRequest $ do
        postUrlEncoded "/" $ Map.fromList [("foo", ["foo1", "foo2"])]

    body <- getRqBody request
    assertEqual "body" "foo=foo1&foo=foo2" body
    assertEqual "len" (Just (S.length body)) $ rqContentLength request
    assertEqual "contentType" (Just "application/x-www-form-urlencoded") $
                getHeader "Content-Type" request


------------------------------------------------------------------------------
testToString :: Test
testToString = testCase "test/requestBuilder/testToString" $ do
    rsp  <- runHandler rq h
    http <- responseToString rsp
    body <- getResponseBody rsp

    assertSuccess rsp
    assertEqual "HTTP body" "" body
    assertBool "HTTP header" $ http =~ headRE
    assertBool "HTTP date"   $ http =~ dateRE
  where
    rq     = get "/" Map.empty
    h      = return ()
    headRE = "HTTP/1.1 200 OK" :: ByteString
    dateRE = S.concat [ "Date: [a-zA-Z]+, [0-9]+ [a-zA-Z]+ "
                      , "[0-9]+ [0-9]+:[0-9]+:[0-9]+ GMT"
                      ]


------------------------------------------------------------------------------
testAssert404 :: Test
testAssert404 = testCase "test/requestBuilder/testAssert404" $ do
    rsp <- runHandler (get "/" Map.empty) mzero
    assert404 rsp


------------------------------------------------------------------------------
testAssertBodyContains :: Test
testAssertBodyContains =
    testCase "test/requestBuilder/testAssertBodyContains" $ do
        rsp <- runHandler (get "/" Map.empty) $ do
                   writeBS "RESPONSE IS OK"
        assertBodyContains "NSE IS" rsp


------------------------------------------------------------------------------
testAssertRedirect :: Test
testAssertRedirect = testCase "test/requestBuilder/testAssertRedirect" $ do
    rsp <- runHandler (get "/" Map.empty) $ redirect "/bar"
    assertRedirectTo "/bar" rsp
    assertRedirect rsp


{-

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
               setRequestBody PUT "Hello World"
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
               addHeader "X-Forwarded-For" "127.0.0.1"
               addHeader "X-Forwarded-For" "192.168.1.0"
  assertEqual "RequestBuilder addHeader not working"
              (Just ["192.168.1.0", "127.0.0.1"])
              (Map.lookup "X-Forwarded-For" (rqHeaders request))

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

-}



------------------------------------------------------------------------------
getRqBody :: Request -> IO ByteString
getRqBody rq = do
    (SomeEnumerator enum) <- readIORef ref
    run_ $ enum $$ liftM S.concat consume
  where
    ref = rqBody rq
