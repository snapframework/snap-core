{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Internal.Http.Types.Tests
  ( tests ) where

import           Control.Monad
import           Control.Parallel.Strategies
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Lazy.Char8 ()
import           Data.IORef
import           Data.List (sort)
import qualified Data.Map as Map
import           Data.Time.Calendar
import           Data.Time.Clock
import           Prelude hiding (take)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
import           Text.Regex.Posix

import           Snap.Internal.Http.Types
import           Snap.Iteratee


tests :: [Test]
tests = [ testTypes
        , testCookies
        , testUrlDecode
        , testFormatLogTime
        , testAddHeader ]


mkRq :: IO Request
mkRq = do
    enum <- newIORef (SomeEnumerator $ enumBS "")
    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty


testFormatLogTime :: Test
testFormatLogTime = testCase "formatLogTime" $ do
    b <- formatLogTime 3804938

    let re = ("^[0-9]{1,2}/[A-Za-z]{3}/[0-9]{4}:[0-9]{2}:[0-9]{2}:[0-9]{2} (-|\\+)[0-9]{4}$"
                  :: ByteString)

    assertBool "formatLogTime" $ b =~ re


testAddHeader :: Test
testAddHeader = testCase "addHeader" $ do
    defReq <- mkRq

    let req = addHeader "foo" "bar" $
              addHeader "foo" "baz" defReq


    let x = getHeader "foo" req
    assertEqual "addHeader x 2" (Just "bar baz") x


testUrlDecode :: Test
testUrlDecode = testCase "urlDecode" $ do
    assertEqual "bad hex" Nothing $ urlDecode "%qq"


testTypes :: Test
testTypes = testCase "show" $ do
    defReq <- mkRq

    let req = rqModifyParams (Map.insert "zzz" ["bbb"]) $
              updateHeaders (Map.insert "zzz" ["bbb"]) $
              rqSetParam "foo" ["bar"] $
              defReq

    let req2 = (addHeader "zomg" "1234" req) { rqCookies = [ cook, cook2 ] }

    let !a = show req `using` rdeepseq
    let !_ = show req2 `using` rdeepseq

    -- we don't care about the show instance really, we're just trying to shut
    -- up hpc
    assertBool "show" $ a /= b
    assertEqual "rqParam" (Just ["bar"]) (rqParam "foo" req)
    assertEqual "lookup" (Just ["bbb"]) (Map.lookup "zzz" $ rqParams req)
    assertEqual "lookup 2" (Just ["bbb"]) (Map.lookup "zzz" $ headers req)

    assertEqual "response status" 555 $ rspStatus resp
    assertEqual "response status reason" "bogus" $ rspStatusReason resp
    assertEqual "content-length" (Just 4) $ rspContentLength resp
    -- run response body
    let benum = rspBodyToEnum $ rspBody resp
    bd <- runIteratee consume >>= run_ . benum
    assertEqual "response body" "PING" $ S.concat bd

    let !_ = show GET
    let !_ = GET == POST
    let !_ = headers $ headers defReq

    let !_ = show resp2 `using` rdeepseq


    return ()

  where
    resp = addResponseCookie cook $
           setContentLength 4 $
           modifyResponseBody id $
           setResponseBody (enumBS "PING") $
           setContentType "text/plain" $
           setResponseStatus 555 "bogus" $
           emptyResponse
    !b = show resp `using` rdeepseq

    resp2 = addResponseCookie cook2 resp

    utc   = UTCTime (ModifiedJulianDay 55226) 0
    cook  = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/")
    cook2 = Cookie "zoo" "baz" (Just utc) (Just ".foo.com") (Just "/")


testCookies :: Test
testCookies = testCase "cookies" $ do
    assertEqual "cookie" (Just cook) rCook
    assertEqual "cookie2" (Just cook2) rCook2
    assertEqual "cookie3" (Just cook3) rCook3
    assertEqual "empty response cookie3" (Just cook3) rCook3e
    assertEqual "removed cookie" Nothing nilCook
    assertEqual "multiple cookies" [cook, cook2] cks
    assertEqual "cookie modification" (Just cook3) rCook3Mod

    return ()

  where
    resp = addResponseCookie cook $
           setContentType "text/plain" $
           emptyResponse

    f _ = cook3

    resp' = deleteResponseCookie "foo" resp
    resp'' = modifyResponseCookie "foo" f resp
    resp2 = addResponseCookie cook2 resp
    resp3 = addResponseCookie cook3 resp2
    resp4 = addResponseCookie cook3 emptyResponse

    utc   = UTCTime (ModifiedJulianDay 55226) 0
    cook  = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/")
    cook2 = Cookie "zoo" "baz" (Just utc) (Just ".foo.com") (Just "/")
    cook3 = Cookie "boo" "baz" Nothing Nothing Nothing

    rCook = getResponseCookie "foo" resp
    nilCook = getResponseCookie "foo" resp'
    rCook2 = getResponseCookie "zoo" resp2
    rCook3 = getResponseCookie "boo" resp3
    rCook3e = getResponseCookie "boo" resp4
    rCook3Mod = getResponseCookie "boo" resp''

    cks = getResponseCookies resp2




