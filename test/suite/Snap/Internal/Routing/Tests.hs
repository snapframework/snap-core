{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Routing.Tests
  ( tests ) where

import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)


import           Snap.Internal.Http.Types
import           Snap.Internal.Routing
import           Snap.Internal.Types
import           Snap.Iteratee hiding (head)

tests :: [Test]
tests = [ testRouting1
        , testRouting2
        , testRouting3
        , testRouting4
        , testRouting5
        , testRouting6
        , testRouting7
        , testRouting8
        , testRouting9
        , testRouting10 ]

expectException :: IO a -> IO ()
expectException m = do
    e <- try m
    case e of
      Left (z::SomeException)  -> (show z) `seq` return ()
      Right _ -> assertFailure "expected exception, didn't get one"


mkRequest :: ByteString -> IO Request
mkRequest uri = do
    enum <- newIORef $ SomeEnumerator return

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                     enum Nothing GET (1,1) [] "" uri "/"
                     (B.concat ["/",uri]) "" Map.empty

go :: Snap a -> ByteString -> IO a
go m s = do
    req <- mkRequest s
    run $ evalSnap m $ req


routes :: Snap ByteString
routes = route [ ("foo"             , topFoo    )
               , ("foo/bar"         , fooBar    )
               , ("foo/bar/baz"     , fooBarBaz )
               , ("foo/:id"         , fooCapture)
               , ("bar/:id"         , fooCapture)
               , ("bar/quux"        , barQuux   )
               , ("bar"             , bar       ) ]


topFoo, fooBar, fooCapture, fooBarBaz, bar, barQuux :: Snap ByteString

topFoo = return "topFoo"
fooBar = return "fooBar"
fooCapture = liftM (head . fromJust . rqParam "id") getRequest
fooBarBaz = liftM rqPathInfo getRequest
barQuux = return "barQuux"
bar     = return "bar"

testRouting1 :: Test
testRouting1 = testCase "routing1" $ do
    r1 <- go routes "foo"
    assertEqual "/foo" "topFoo" r1


testRouting2 :: Test
testRouting2 = testCase "routing2" $ do
    r2 <- go routes "foo/baz"
    assertEqual "/foo/baz" "baz" r2

testRouting3 :: Test
testRouting3 = testCase "routing3" $ do
    expectException $ go routes "/xsaxsaxsax"

testRouting4 :: Test
testRouting4 = testCase "routing4" $ do
    r3 <- go routes "foo/bar"
    assertEqual "/foo/bar" "fooBar" r3

testRouting5 :: Test
testRouting5 = testCase "routing5" $ do
    r4 <- go routes "foo/bar/baz/quux"
    assertEqual "/foo/bar/baz/quux" "quux" r4

testRouting6 :: Test
testRouting6 = testCase "routing6" $ do
    r5 <- go routes "foo/bar/sproing"
    assertEqual "/foo/bar/sproing" "fooBar" r5

testRouting7 :: Test
testRouting7 = testCase "routing7" $ do
    r <- go routes "bar"
    assertEqual "/bar" "bar" r

testRouting8 :: Test
testRouting8 = testCase "routing8" $ do
    r2 <- go routes "bar/quux"
    assertEqual "/bar/quux" "barQuux" r2

testRouting9 :: Test
testRouting9 = testCase "routing9" $ do
    r3 <- go routes "bar/whatever"
    assertEqual "/bar/whatever" "whatever" r3

testRouting10 :: Test
testRouting10 = testCase "routing10" $ do
    r4 <- go routes "bar/quux/whatever"
    assertEqual "/bar/quux/whatever" "barQuux" r4

