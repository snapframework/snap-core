{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Snap.Internal.Parsing.Tests ( tests ) where
------------------------------------------------------------------------------
import qualified Data.ByteString.Char8                as S
import qualified Data.Map                             as Map
import           Data.Word                            (Word8)
import           System.Random
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test, path)
import           Test.QuickCheck
------------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Parsing
import qualified Snap.Internal.Parsing.FastSet        as F
import           Snap.Test.Common
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testAvPairs
        , testCookie
        , testFastSet
        , testHeaderParse
        , testQuotedString
        , testTrivialFastSet
        , testUnsafeFromHex
        , testUnsafeFromInt
        , testUrlEncoded
        ]


------------------------------------------------------------------------------
testAvPairs :: Test
testAvPairs = testCase "parsing/avpairs" $ do
    let x = fullyParse txt pAvPairs
    assertEqual "avpairs" (Right [ ("foo" , "bar")
                                 , ("bar" , "baz")
                                 , ("quux", ""   )
                                 ]) x
  where
    txt = "foo = bar; bar = baz; quux"


------------------------------------------------------------------------------
testCookie :: Test
testCookie =
    testCase "parsing/parseCookie" $ do
        assertEqual "cookie parsing" (Just [cv]) cv2

  where
    cv  = Cookie nm v Nothing Nothing Nothing False False
    cv2 = parseCookie ct

    nm     = "foo"
    v      = "bar"

    ct = S.concat [ nm , "=" , v ]


------------------------------------------------------------------------------
testHeaderParse :: Test
testHeaderParse = testCase "parsing/headers" $ do
    let e = fullyParse txt pHeaders
    assertEqual "parse" (Right [("foo", "bar baz quux")]) e

    let f = fullyParse bad pHeaders
    assertEqual "bad parse" (Right []) f

  where
    txt = S.concat [ "foo: bar\r\n"
                   , "  baz\r\n"
                   , "  quux\r\n"
                   ]

    bad = "%&^%&^*^(*&^*&^*%*&%^^#$"


------------------------------------------------------------------------------
testQuotedString :: Test
testQuotedString = testCase "parsing/quoted-string" $ do
    let e = fullyParse txt pQuotedString
    assertEqual "q-s" (Right "foo\"bar\"baz") e

  where
    txt = "\"foo\\\"bar\\\"baz\""


------------------------------------------------------------------------------
-- older random didn't have a Word8 instance.....
data WrappedWord8 = W { unW :: Word8 }

instance Show WrappedWord8 where
    show (W w) = show w

instance Random WrappedWord8 where
    randomR (W a, W b) g = case randomR (fromIntegral a :: Int,
                                         fromIntegral b :: Int) g
                           of (x, g') -> (W $! fromIntegral x, g')
    random = randomR (W minBound, W maxBound)

instance Arbitrary WrappedWord8 where
    arbitrary = choose (W minBound, W maxBound)


------------------------------------------------------------------------------
testFastSet :: Test
testFastSet = testProperty "parsing/fastset" prop
  where
    prop :: [WrappedWord8] -> Bool
    prop l0 = all (`F.memberWord8` set) l
      where
        set = F.fromList l
        l   = map unW l0

------------------------------------------------------------------------------
testTrivialFastSet :: Test
testTrivialFastSet = testCase "parsing/fastset/trivial" $ do
    coverEqInstance set
    coverOrdInstance set
    coverShowInstance set
    coverShowInstance setBig

    assertBool "ok" $ all (not . (`F.memberWord8` set2)) [2..maxBound]
  where
    set = F.charClass "0a-c"
    setBig = F.fromList [0..235]
    set2 = F.fromList [1]


------------------------------------------------------------------------------
testUnsafeFromHex :: Test
testUnsafeFromHex = testCase "parsing/unsafeFromHex" $ do
    expectExceptionH $ return $! ((unsafeFromHex "zz") :: Int)
    let x = unsafeFromHex "a"
    assertEqual "a" (10 :: Int) x


------------------------------------------------------------------------------
testUnsafeFromInt :: Test
testUnsafeFromInt = testCase "parsing/unsafeFromNat" $ do
    expectExceptionH $ return $! ((unsafeFromNat "zz") :: Int)
    let x = unsafeFromNat "10"
    assertEqual "10" (10 :: Int) x


------------------------------------------------------------------------------
testUrlEncoded :: Test
testUrlEncoded = testCase "parsing/urlEncoded" $ do
    let x = parseUrlEncoded "foo=h%20i&bar=baz+baz&baz=quux&baz=zzz&%zz"
    assertEqual "map" (Map.fromList [ ("foo", ["h i"])
                                    , ("bar", ["baz baz"])
                                    , ("baz", ["quux", "zzz"]) ]) x
