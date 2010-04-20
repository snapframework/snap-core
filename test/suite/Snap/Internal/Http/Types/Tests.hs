{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Internal.Http.Types.Tests
  ( tests ) where

import           Control.Parallel.Strategies
import           Data.ByteString.Lazy.Char8 ()
import           Data.Iteratee (stream2stream, run)
import qualified Data.Map as Map
import           Data.Time.Calendar
import           Data.Time.Clock
import           Prelude hiding (take)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)


import           Snap.Internal.Http.Types
import           Snap.Iteratee (enumBS, fromWrap)

tests :: [Test]
tests = [ testTypes ]

zomgRq :: Request
zomgRq = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 return Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty


testTypes :: Test
testTypes = testCase "show" $ do
    -- we don't care about the show instance really, we're just trying to shut
    -- up hpc
    assertBool "show" $ a /= b
    assertEqual "rqParam" (Just ["bar"]) (rqParam "foo" req)
    assertEqual "lookup" (Just ["bbb"]) (Map.lookup "zzz" $ rqParams req)
    assertEqual "lookup 2" (Just ["bbb"]) (Map.lookup "zzz" $ headers req)
    assertEqual "cookie" (Just ["foo=bar; path=/; expires=Sat, 30-Jan-2010 00:00:00 GMT; domain=.foo.com"]) cook'

    assertEqual "response status" 555 $ rspStatus resp
    assertEqual "response status reason" "bogus" $ rspStatusReason resp
    assertEqual "content-length" (Just 4) $ rspContentLength resp
    -- run response body
    bd <- rspBody resp stream2stream >>= run
    assertEqual "response body" "PING" (fromWrap bd)

    let !_ = show GET
    let !_ = GET == POST
    let !_ = headers $ headers zomgRq


    return ()

  where
    resp = addCookie cook $
           setContentLength 4 $
           modifyResponseBody id $
           setResponseBody (enumBS "PING") $
           setContentType "text/plain" $
           setResponseStatus 555 "bogus" $
           emptyResponse
    !a = show req `using` rdeepseq
    !b = show resp `using` rdeepseq

    req = rqModifyParams (Map.insert "zzz" ["bbb"]) $
          updateHeaders (Map.insert "zzz" ["bbb"]) $
          rqSetParam "foo" ["bar"] $
          zomgRq

    utc = UTCTime (ModifiedJulianDay 55226) 0
    cook = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/")
    cook' = Map.lookup "Set-Cookie" $ headers resp

