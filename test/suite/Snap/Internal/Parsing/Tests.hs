{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Internal.Parsing.Tests
  ( tests ) where


import qualified Data.ByteString as S
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import           Snap.Internal.Http.Types
import           Snap.Internal.Parsing


tests :: [Test]
tests = [ testCookie ]


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
