{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Internal.Parsing.Tests ( tests ) where
------------------------------------------------------------------------------
import           Control.Applicative            (many)
import           Data.Attoparsec.Char8          (IResult (..), char, string)
import qualified Data.ByteString.Char8          as S
import qualified Data.Map                       as Map
import           Data.Word                      (Word8)
import           System.Random
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test, path)
import           Test.QuickCheck
------------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Parsing
import           Snap.Test.Common
------------------------------------------------------------------------------

tests :: [Test]
tests = [ testAvPairs
        , testCookie
        , testHeaderParse
        , testQuotedString
        , testUnsafeFromHex
        , testUnsafeFromInt
        , testUrlEncoded
        , testFailParse
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
data WrappedWord8 = W { _unW :: Word8 }

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


------------------------------------------------------------------------------
testFailParse :: Test
testFailParse = testCase "parsing/failParse" $ do
    let (Left a) = fullyParse "foo" (string "bar")
    let (Left b) = fullyParse "foo" (fail "bar")
    let (Left c) = fullyParse "aaaaa" (many (char 'a') >> string "b")
    let f  = const $ Partial f
    let (Left d) = fullyParse' (const f) (const f) "aaaaa" (string "b")
    let e@(Just "aaaa") = parseToCompletion (many (char 'a')) "aaaa"
    let (Done z (0::Int)) = finish $! Partial $! \s ->
                             s `seq` Partial (\t -> t `seq` Done t 0)

    return $! a `seq` b `seq` c `seq` d `seq` e `seq` z `seq` ()
