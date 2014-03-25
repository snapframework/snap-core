{-# LANGUAGE OverloadedStrings #-}

module Snap.Test.Tests
  ( tests
  ) where

------------------------------------------------------------------------------
import           Control.Monad                     (MonadPlus (mzero), liftM)
import           Control.Monad.IO.Class            (liftIO)
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Char8             as S (concat, isSuffixOf, length)
import qualified Data.Map                          as Map (empty, fromList)
import           Snap.Core                         (Cookie (Cookie), Method (DELETE, GET, Method, PATCH, POST, PUT), Request (rqContentLength, rqContextPath, rqIsSecure, rqMethod, rqParams, rqPathInfo, rqPostParams, rqQueryParams, rqQueryString, rqURI, rqVersion), extendTimeout, getHeader, getParam, logError, redirect, runSnap, writeBS)
import           Snap.Internal.Http.Types          (Request (..))
import qualified Snap.Internal.Http.Types          as T (getHeader)
import           Snap.Internal.Test.RequestBuilder (FileData (FileData), MultipartParam (Files, FormData), RequestType (DeleteRequest, GetRequest, MultipartPostRequest, RequestWithRawBody, UrlEncodedPostRequest), addCookies, addHeader, buildRequest, delete, evalHandler, get, postMultipart, postRaw, postUrlEncoded, put, requestToString, responseToString, runHandler, setContentType, setHeader, setHttpVersion, setQueryStringRaw, setRequestPath, setRequestType, setSecure)
import           Snap.Test                         (assert404, assertBodyContains, assertRedirect, assertRedirectTo, assertSuccess, getResponseBody)
import           Snap.Test.Common                  (coverShowInstance, expectExceptionH)
import           Snap.Util.FileUploads             (PartInfo (PartInfo), defaultUploadPolicy, handleMultipart)
import qualified System.IO.Streams                 as Streams (fromList, toList)
import           Test.Framework                    (Test)
import           Test.Framework.Providers.HUnit    (testCase)
import           Test.HUnit                        (assertBool, assertEqual)
import           Text.Regex.Posix                  ((=~))
------------------------------------------------------------------------------

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testDefaultBuild
        , testRequestToString
        , testSetRequestType
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
        , testTrivials
        ]


------------------------------------------------------------------------------
testDefaultBuild :: Test
testDefaultBuild = testCase "test/requestBuilder/defaultBuild" $ do
    req <- buildRequest $ setRequestType GetRequest
    Streams.toList (rqBody req) >>= assertEqual "body" []
    assertEqual "pathInfo" "" $ rqPathInfo req
    assertEqual "ctx" "/" $ rqContextPath req
    assertEqual "uri" "/" $ rqURI req
    assertEqual "qs" "" $ rqQueryString req
    assertEqual "p1" Map.empty $ rqParams req
    assertEqual "p2" Map.empty $ rqQueryParams req
    assertEqual "p3" Map.empty $ rqPostParams req


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

    request6 <- buildRequest $ setRequestType $
                RequestWithRawBody (Method "MOVE") "foo"
    assertEqual "setRequestType/6/Method" (Method "MOVE") (rqMethod request6)

    request7 <- buildRequest $ setRequestType $ RequestWithRawBody PATCH "bar"
    assertEqual "setRequestType/7/Method" PATCH (rqMethod request7)

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
                   postUrlEncoded "/" $ Map.fromList [("foo", ["foo0"])]
                   addCookies [c1, c2]
                   setQueryStringRaw "foo=foo&foo=foo2&bar=bar"
    assertEqual "setQueryStringRaw" params $ rqParams request
    assertEqual "cookie" (Just "k=v; k2=v2") $ getHeader "cookie" request

  where
    c1     = Cookie "k" "v" Nothing Nothing Nothing False False
    c2     = Cookie "k2" "v2" Nothing Nothing Nothing False False
    params = Map.fromList [ ("foo", ["foo0", "foo", "foo2"])
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
    assertEqual "addHeader" (Just "bar,bar2") $ T.getHeader "bar" request
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
    assertEqual "rqPathInfo" "foo/bar" $ rqPathInfo request
    assertEqual "rqURI" "/foo/bar" $ rqURI request
    assertEqual "rqContextPath" "/" $ rqContextPath request
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
    assertEqual "rqPathInfo" "foo/bar" $ rqPathInfo request3
    assertEqual "rqURI" "/foo/bar" $ rqURI request3
    assertEqual "rqContextPath" "/" $ rqContextPath request3
    assertEqual "rqVersion" (1,1) $ rqVersion request3


------------------------------------------------------------------------------
testMultipart :: Test
testMultipart = testCase "test/requestBuilder/testMultipart" $ do
    request0        <- buildRequest rq
    (request,rbody) <- peekRqBody request0
    assertEqual "content-length" (Just (fromIntegral $ S.length rbody)) $
                rqContentLength request

    (_,response) <- runSnap handler (const $ return $! ())
                                    (const $ return $! ())
                                    request
    body     <- getResponseBody response
    assertEqual "body" "OK" body

  where
    partHandler (PartInfo field fn ct) stream = do
        body <- liftM S.concat $ Streams.toList stream
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
    assertEqual "len" (Just (fromIntegral $ S.length body))
                      (rqContentLength request)
    assertEqual "contentType" (Just "application/x-www-form-urlencoded") $
                getHeader "Content-Type" request


------------------------------------------------------------------------------
testToString :: Test
testToString = testCase "test/requestBuilder/testToString" $ do
    rsp  <- runHandler rq h
    http <- responseToString rsp
    body <- getResponseBody rsp
    out2 <- evalHandler rq h

    assertSuccess rsp
    assertEqual "Close" (Just "close") $ getHeader "connection" rsp
    assertEqual "HTTP body" "zzz" body
    assertBool "HTTP header" $ http =~ headRE
    assertBool "HTTP date"   $ http =~ dateRE
    assertEqual "monadic result" 42 out2
  where
    rq     = do postRaw "/" "text/zzz" "zzz"
                setHttpVersion (1,0)
    h      = do writeBS "zzz"
                logError "zzz"
                extendTimeout 5
                return (42 :: Int)
    headRE = "HTTP/1.1 200 OK" :: ByteString
    dateRE = S.concat [ "date: [a-zA-Z]+, [0-9]+ [a-zA-Z]+ "
                      , "[0-9]+ [0-9]+:[0-9]+:[0-9]+ GMT"
                      ]


------------------------------------------------------------------------------
testRequestToString :: Test
testRequestToString = testCase "test/requestBuilder/reqToString" $ do
    req1 <- buildRequest $ setRequestType GetRequest
    s1   <- requestToString req1
    assertBool "HTTP header" $ s1 =~ headRE

    req2 <- buildRequest $ do postRaw "/" "text/zzz" "zzz"
                              setHttpVersion (1,0)
    s2   <- requestToString req2
    assertBool "HTTP header2" $ s2 =~ postHeadRE
    assertBool "HTTP cl" $ s2 =~ ("content-length: 3" :: ByteString)

    req3 <- buildRequest $ do postRaw "/" "text/zzz" "zzz"
                              setHeader "transfer-encoding" "chunked"
    s3   <- requestToString req3
    assertBool "HTTP chunked" $ "3\r\nzzz\r\n0\r\n\r\n" `S.isSuffixOf` s3

  where
    headRE = "^GET / HTTP/1.1\r\n" :: ByteString
    postHeadRE = "^POST / HTTP/1.0\r\n" :: ByteString


------------------------------------------------------------------------------
testAssert404 :: Test
testAssert404 = testCase "test/requestBuilder/testAssert404" $ do
    rsp <- runHandler (get "/" Map.empty) mzero
    assert404 rsp
    expectExceptionH $ assertSuccess rsp
    expectExceptionH $ assertBodyContains "fjlkdja" rsp
    expectExceptionH $ assertRedirectTo "/zzzzz" rsp
    expectExceptionH $ assertRedirect rsp

    rsp2 <- runHandler (get "/" Map.empty) (return ())
    assertSuccess rsp2
    expectExceptionH $ assert404 rsp2



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
    expectExceptionH $ assertRedirectTo "/zzzz" rsp


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "requestBuilder/trivials" $ do
    coverShowInstance (FormData [])
    coverShowInstance (Files [])
    coverShowInstance (FileData "" "" "")
    coverShowInstance GetRequest


------------------------------------------------------------------------------
getRqBody :: Request -> IO ByteString
getRqBody = liftM S.concat . Streams.toList . rqBody


peekRqBody :: Request -> IO (Request, ByteString)
peekRqBody rq = do
    l <- Streams.toList $ rqBody rq
    b <- Streams.fromList l

    return (rq { rqBody = b }, S.concat l)
