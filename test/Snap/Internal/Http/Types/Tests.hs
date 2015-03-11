{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Snap.Internal.Http.Types.Tests ( tests ) where
------------------------------------------------------------------------------
import           Control.Parallel.Strategies    (rdeepseq, using)
import           Data.ByteString.Builder        (byteString)
import qualified Data.ByteString.Char8          as S (concat)
import           Data.ByteString.Lazy.Char8     ()
import           Data.List                      (sort)
import qualified Data.Map                       as Map (empty, insert, lookup)
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.Clock                (UTCTime (UTCTime))
import           Snap.Internal.Http.Types       (Cookie (Cookie), HasHeaders (headers, updateHeaders), Method (CONNECT, DELETE, GET, HEAD, Method, OPTIONS, PATCH, POST, PUT, TRACE), Request (rqCookies, rqIsSecure, rqContentLength, rqParams), Response (rspContentLength, rspStatus, rspStatusReason), addHeader, addResponseCookie, cookieToBS, deleteResponseCookie, emptyResponse, formatLogTime, getHeader, getResponseCookie, getResponseCookies, listHeaders, modifyResponseBody, modifyResponseCookie, rqModifyParams, rqParam, rqSetParam, setContentLength, setContentType, setResponseBody, setResponseCode, setResponseStatus)
import           Snap.Internal.Parsing          (urlDecode)
import qualified Snap.Test                      as Test (buildRequest, get, getResponseBody)
import qualified Snap.Types.Headers             as H (lookup, set)
import qualified System.IO.Streams              as Streams (write)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertBool, assertEqual)
import           Text.Regex.Posix               ((=~))

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testTypes
        , testCookies
        , testCookieToBS
        , testUrlDecode
        , testFormatLogTime
        , testAddHeader
        , testHeaderOrd
        ]


------------------------------------------------------------------------------
mkRq :: IO Request
mkRq = Test.buildRequest $ Test.get "/" Map.empty


------------------------------------------------------------------------------
testHeaderOrd :: Test
testHeaderOrd = testCase "httpTypes/methodOrd" $ do
    let methods = [GET, HEAD, POST, PUT, DELETE, TRACE, OPTIONS, CONNECT,
                   PATCH, Method "Foo"]
    mapM_ (\m -> assertEqual "method" (compare m m) EQ) methods
    mapM_ (\m -> assertEqual "ord" (compare GET m) LT) $ tail methods
    assertEqual "ord2" LT (compare (Method "a") (Method "b"))

------------------------------------------------------------------------------
testFormatLogTime :: Test
testFormatLogTime = testCase "httpTypes/formatLogTime" $ do
    b <- formatLogTime 3804938

    let re = S.concat [ "^[0-9]{1,2}/[A-Za-z]{3}/[0-9]{4}:[0-9]{2}:[0-9]{2}"
                      , ":[0-9]{2} (-|\\+)[0-9]{4}$" ]

    assertBool "formatLogTime" $ b =~ re


------------------------------------------------------------------------------
testAddHeader :: Test
testAddHeader = testCase "httpTypes/addHeader" $ do
    defReq <- mkRq

    let req = addHeader "foo" "bar" $
              addHeader "foo" "baz" defReq


    let x = getHeader "foo" req
    assertEqual "addHeader x 2" (Just "baz,bar") x
    assertEqual "listHeaders" [ ("foo","baz,bar")
                              , ("Host", "localhost") ] $
                sort $ listHeaders req

    let hdrs = updateHeaders (H.set "zzz" "bbb") $ headers req
    assertEqual "listHeaders 2"
                [ ("foo", "baz,bar")
                , ("Host", "localhost")
                , ("zzz", "bbb") ]
                (sort (listHeaders $ headers hdrs))


------------------------------------------------------------------------------
testUrlDecode :: Test
testUrlDecode = testCase "httpTypes/urlDecode" $ do
    assertEqual "bad hex" Nothing $ urlDecode "%qq"


------------------------------------------------------------------------------
testTypes :: Test
testTypes = testCase "httpTypes/show" $ do
    defReq <- mkRq

    let req = rqModifyParams (Map.insert "zzz" ["bbb"]) $
              updateHeaders (H.set "zzz" "bbb") $
              rqSetParam "foo" ["bar"] $
              defReq

    let req2 = (addHeader "zomg" "1234" req)
               { rqCookies = [ cook, cook2 ]
               , rqIsSecure = True
               , rqContentLength = Just 10
               }

    let !a = show req `using` rdeepseq
    let !_ = show req2 `using` rdeepseq

    -- we don't care about the show instance really, we're just trying to shut
    -- up hpc
    assertBool "show" $ a /= b
    assertEqual "rqParam" (Just ["bar"]) (rqParam "foo" req)
    assertEqual "lookup" (Just ["bbb"]) (Map.lookup "zzz" $ rqParams req)
    assertEqual "lookup 2" (Just "bbb") (H.lookup "zzz" $ headers req)

    assertEqual "response status" 555 $ rspStatus resp
    assertEqual "response status reason" "bogus" $ rspStatusReason resp
    assertEqual "content-length" (Just 4) $ rspContentLength resp

    bd <- Test.getResponseBody resp
    assertEqual "response body" "PING" $ bd

    let !_ = show GET
    let !_ = GET == POST
    let !_ = headers $ headers defReq
    let !_ = show resp2 `using` rdeepseq

    assertEqual "999" "Unknown" (rspStatusReason resp3)

  where
    enum os = Streams.write (Just $ byteString "PING") os >> return os

    resp = addResponseCookie cook $
           setContentLength 4 $
           modifyResponseBody id $
           setResponseBody enum $
           setContentType "text/plain" $
           setResponseStatus 555 "bogus" $
           emptyResponse
    !b = show resp `using` rdeepseq

    resp2 = addResponseCookie cook2 resp

    resp3 = setResponseCode 999 resp2

    utc   = UTCTime (ModifiedJulianDay 55226) 0
    cook  = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/") False False
    cook2 = Cookie "zoo" "baz" (Just utc) (Just ".foo.com") (Just "/") False False


------------------------------------------------------------------------------
testCookieToBS :: Test
testCookieToBS = testCase "httpTypes/cookieToBS" $ do
    let [b0, b1, b2] = map cookieToBS [cookie0, cookie1, cookie2]
    assertEqual "cookie0" "foo=bar; HttpOnly" b0
    assertEqual "cookie1" "foo=bar; Secure" b1
    assertEqual "cookie2" "foo=bar; path=/; expires=Sat, 30 Jan 2010 00:00:00 GMT; domain=.foo.com; HttpOnly" b2
  where
    utc   = UTCTime (ModifiedJulianDay 55226) 0
    cookie0  = Cookie "foo" "bar" Nothing Nothing Nothing False True
    cookie1  = Cookie "foo" "bar" Nothing Nothing Nothing True False
    cookie2  = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/") False True


testCookies :: Test
testCookies = testCase "httpTypes/cookies" $ do
    assertEqual "cookie" (Just cook) rCook
    assertEqual "cookie2" (Just cook2) rCook2
    assertEqual "cookie3" (Just cook3) rCook3
    assertEqual "empty response cookie3" (Just cook3) rCook3e
    assertEqual "removed cookie" Nothing nilCook
    assertEqual "multiple cookies" [cook, cook2] cks
    assertEqual "cookie modification" (Just cook3) rCook3Mod
    assertEqual "modify nothing" Nothing (getResponseCookie "boo" resp5)

    return ()

  where
    resp = addResponseCookie cook $
           setContentType "text/plain" $
           emptyResponse

    f !_ = cook3

    resp' = deleteResponseCookie "foo" resp
    resp'' = modifyResponseCookie "foo" f resp
    resp2 = addResponseCookie cook2 resp
    resp3 = addResponseCookie cook3 resp2
    resp4 = addResponseCookie cook3 emptyResponse
    resp5 = modifyResponseCookie "boo" id emptyResponse

    utc   = UTCTime (ModifiedJulianDay 55226) 0
    cook  = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/") False True
    cook2 = Cookie "zoo" "baz" (Just utc) (Just ".foo.com") (Just "/") True False
    cook3 = Cookie "boo" "baz" Nothing Nothing Nothing False False

    rCook = getResponseCookie "foo" resp
    nilCook = getResponseCookie "foo" resp'
    rCook2 = getResponseCookie "zoo" resp2
    rCook3 = getResponseCookie "boo" resp3
    rCook3e = getResponseCookie "boo" resp4
    rCook3Mod = getResponseCookie "boo" resp''

    cks = getResponseCookies resp2
