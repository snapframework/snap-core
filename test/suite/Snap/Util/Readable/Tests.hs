{-# LANGUAGE OverloadedStrings #-}

module Snap.Util.Readable.Tests (tests) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8          (ByteString)
import           Data.Text                      (Text)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (assertEqual)
------------------------------------------------------------------------------
import           Snap.Test.Common
import           Snap.Util.Readable


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testReads ]


------------------------------------------------------------------------------
testReads :: Test
testReads = testCase "util/readable/reads" $ do
    fromBS "ok" >>= assertEqual "bs" ("ok" :: ByteString)
    fromBS "ok" >>= assertEqual "txt" ("ok" :: Text)
    fromBS "1.0" >>= assertEqual "dbl" (1.0 :: Double)
    fromBS "1" >>= assertEqual "int" (1 :: Int)
    fromBS "1" >>= assertEqual "integer" (1 :: Integer)
    expectExceptionH ((fromBS "10dsjfk") :: IO Int)

    expectExceptionH ((fromBS "z") :: IO Int)
    expectExceptionH ((fromBS "z") :: IO Integer)
    expectExceptionH ((fromBS "z") :: IO Double)
