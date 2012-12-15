{-# LANGUAGE OverloadedStrings #-}

module Snap.Types.Headers.Tests (tests) where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (assertBool, assertEqual)
------------------------------------------------------------------------------
import           Snap.Test.Common
import qualified Snap.Types.Headers             as H


tests :: [Test]
tests = [ testTrivials ]


testTrivials :: Test
testTrivials = testCase "types/headers/show" $ do
    let h = H.empty
    assertBool "null" $ H.null h
    assertEqual "lookupWithDefault" "ok" $ H.lookupWithDefault "ok" "foo" h
    assertEqual "fold" () $ H.fold (\x _ _ -> x) () $ H.insert "ok" "ok" h
    assertBool "member" $ not $ H.member "foo" h
    coverShowInstance h
