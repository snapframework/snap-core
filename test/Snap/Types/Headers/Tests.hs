{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Snap.Types.Headers.Tests (tests) where
------------------------------------------------------------------------------
import           Data.CaseInsensitive           as CI
import           Data.List                      (sort)
import qualified Data.Set                       as Set
import           Snap.Test.Common               (coverShowInstance)
import qualified Snap.Types.Headers             as H
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertBool, assertEqual)
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testTrivials
        , testToFrom
        , testFolds
        ]


------------------------------------------------------------------------------
testToFrom :: Test
testToFrom = testCase "types/headers/toFrom" $ do
    let h = H.unsafeFromCaseFoldedList [("foo", "bar")]
    assertEqual "toFrom1" [("foo", "bar")] $ H.toList h
    assertEqual "toFrom2" [("foo", "bar")]
                $ H.unsafeToCaseFoldedList $ H.fromList [("Foo", "bar")]
    assertBool "member" $ H.member "Foo" h
    assertBool "not member" $ not $ H.member "Fooooo" h

    let h' = H.set "foo" "zzz" . H.set "zzz" "qqq" $ h
    assertEqual "set/lookup" (Just "zzz") $ H.lookup "Foo" h'
    assertEqual "set/lookupD1" "zzz" $ H.lookupWithDefault "000" "Foo" h'
    assertEqual "set/lookupD2" "000" $ H.lookupWithDefault "000" "Zoo" h'

    assertEqual "toSort" [("a", "b"), ("foo","zzz"), ("zzz","qqq")] $ sort
                    $ H.unsafeToCaseFoldedList $ H.unsafeInsert "a" "b" h'


------------------------------------------------------------------------------
testFolds :: Test
testFolds = testCase "types/headers/folds" $ do
    let h  = H.fromList [("foo", "bar"), ("bar", "baz")]
    let r1 = Set.toAscList $ H.foldl' ffl Set.empty h
    let r2 = Set.toAscList $ H.foldr  ffr Set.empty h
    let r3 = Set.toAscList $ H.foldedFoldl' fl Set.empty h
    let r4 = Set.toAscList $ H.foldedFoldr  fr Set.empty h

    let r  = ["bar", "baz", "foo"]
    assertEqual "r1" r r1
    assertEqual "r2" r r2
    assertEqual "r3" r r3
    assertEqual "r4" r r4

  where
    fl !s !k v  = Set.insert k $ Set.insert v s
    fr !k v !s  = Set.insert k $ Set.insert v s
    ffl !s !k v = Set.insert (CI.foldedCase k) $ Set.insert v s
    ffr !k v !s = Set.insert (CI.foldedCase k) $ Set.insert v s

------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "types/headers/show" $ do
    let h = H.empty
    assertBool "null" $ H.null h
    assertEqual "lookupWithDefault" "ok" $ H.lookupWithDefault "ok" "foo" h
    assertEqual "foldr" () $ H.foldr (\_ _ x -> x) () $ H.insert "ok" "ok" h
    assertEqual "foldl'" () $ H.foldl' (\x _ _ -> x) () $ H.insert "ok" "ok" h
    assertEqual "foldedFoldr" () $
                H.foldedFoldr (\_ _ x -> x) () $ H.unsafeInsert "ok" "ok" h
    assertEqual "foldedFoldl'" () $
                H.foldedFoldl' (\x _ _ -> x) () $ H.unsafeInsert "ok" "ok" h
    assertBool "member" $ not $ H.member "foo" h
    coverShowInstance h
