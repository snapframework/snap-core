{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Internal.Http.Types.Tests
  ( tests ) where

import           Control.Parallel.Strategies
import qualified Data.Map as Map
import           Prelude hiding (take)
import           Test.Framework 
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)


import           Snap.Internal.Http.Types


tests :: [Test]
tests = [ testTypes ]

zomgRq :: Request
zomgRq = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 return Nothing GET (1,1) [] "/" "/" "/" "" Map.empty


testTypes :: Test
testTypes = testCase "show" $ do
    -- we don't care about the show instance really, we're just trying to shut
    -- up hpc
    let req = rqModifyParams (Map.insert "zzz" ["bbb"]) $
              updateHeaders (Map.insert "zzz" ["bbb"]) $
              rqSetParam "foo" ["bar"] $
              zomgRq
    let resp = emptyResponse
    let !a = show req `using` rdeepseq
    let !b = show resp `using` rdeepseq

    assertBool "show" $ a /= b

    assertEqual "lookup" (Map.lookup "zzz" $ rqParams req) (Just ["bbb"])
    assertEqual "lookup 2" (Map.lookup "zzz" $ headers req) (Just ["bbb"])

