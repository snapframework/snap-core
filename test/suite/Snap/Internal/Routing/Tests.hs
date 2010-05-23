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
        , testRouting10
        , testRouting11
        , testRouting12
        , testRouting13
        , testRouting14
        , testRouting15
        , testRouting16
        , testRouting17
        , testRouting18
        , testRouting19
        , testRouting20
        , testRouting21
        , testRouting22
        , testRouting23
        , testRouting24
        , testRouting25
        , testRouteLocal ]

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
    run $ evalSnap m (const $ return ()) req


routes :: Snap ByteString
routes = route [ ("foo"          , topFoo    )
               , ("foo/bar"      , fooBar    )
               , ("foo/bar/baz"  , fooBarBaz )
               , ("foo/:id"      , fooCapture)
               , ("bar/:id"      , fooCapture)
               , ("bar/quux"     , barQuux   )
               , ("bar"          , bar       )
               , ("z/:a/:b/:c/d" , zabc      ) ]

routesLocal :: Snap ByteString
routesLocal = routeLocal [ ("foo/bar/baz"  , fooBarBaz )
                         , ("bar"          , pass ) ]

routes2 :: Snap ByteString
routes2 = route [ (""    , topTop )
                , ("foo" , topFoo ) ]

routes3 :: Snap ByteString
routes3 = route [ (":foo" , topCapture )
                , (""     , topTop     ) ]

routes4 :: Snap ByteString
routes4 = route [ (":foo" , pass       )
                , (":foo" , topCapture ) ]

routes5 :: Snap ByteString
routes5 = route [ ("" , pass       )
                , ("" , topTop ) ]

routes6 :: Snap ByteString
routes6 = route [ (":a/:a" , dblA ) ]

routes7 :: Snap ByteString
routes7 = route [ ("foo/:id"       , fooCapture )
                , ("foo/:id/:id2"  , fooCapture2)
                , ("fooo/:id/:id2" , fooCapture2)
                , ("foooo/bar/baz" , bar        )
                , (""              , topTop     ) ]


topTop, topFoo, fooBar, fooCapture, fooBarBaz, bar, barQuux :: Snap ByteString
dblA, zabc, topCapture, fooCapture2 :: Snap ByteString

dblA = do
    ma <- getParam "a"

    unless (ma == Just "a a") pass
    return "ok"


zabc = do
    ma <- getParam "a"
    mb <- getParam "b"
    mc <- getParam "c"

    unless (   ma == Just "a"
            && mb == Just "b"
            && mc == Just "c" ) pass

    return "ok"


topCapture = do
    mp <- getParam "foo"
    maybe pass return mp

topTop = return "topTop"
topFoo = return "topFoo"
fooBar = return "fooBar"
fooCapture = liftM (head . fromJust . rqParam "id") getRequest
fooCapture2 = liftM (head . fromJust . rqParam "id2") getRequest
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

testRouting11 :: Test
testRouting11 = testCase "routing11" $ do
    r1 <- go routes2 ""
    assertEqual "/" "topTop" r1

testRouting12 :: Test
testRouting12 = testCase "routing12" $ do
    r1 <- go routes2 "foo"
    assertEqual "/foo" "topFoo" r1

testRouting13 :: Test
testRouting13 = testCase "routing13" $ do
    r1 <- go routes3 "zzzz"
    assertEqual "/zzzz" "zzzz" r1

testRouting14 :: Test
testRouting14 = testCase "routing14" $ do
    r1 <- go routes3 ""
    assertEqual "/" "topTop" r1

testRouting15 :: Test
testRouting15 = testCase "routing15" $ do
    r1 <- go routes4 "zzzz"
    assertEqual "/zzzz" "zzzz" r1

testRouting16 :: Test
testRouting16 = testCase "routing16" $ do
    r1 <- go routes5 ""
    assertEqual "/" "topTop" r1

testRouting17 :: Test
testRouting17 = testCase "routing17" $ do
    r1 <- go routes "z/a/b/c/d"
    assertEqual "/z/a/b/c/d" "ok" r1

testRouting18 :: Test
testRouting18 = testCase "routing18" $ do
    r1 <- go routes6 "a/a"
    assertEqual "/a/a" "ok" r1

testRouting19 :: Test
testRouting19 = testCase "routing19" $ do
    r1 <- go routes7 "foo"
    assertEqual "/foo" "topTop" r1

testRouting20 :: Test
testRouting20 = testCase "routing20" $ do
    r1 <- go routes7 "foo/baz"
    assertEqual "/foo/baz" "baz" r1

testRouting21 :: Test
testRouting21 = testCase "routing21" $ do
    r1 <- go routes7 "foo/baz/quux"
    assertEqual "/foo/baz/quux" "quux" r1

testRouting22 :: Test
testRouting22 = testCase "routing22" $ do
    r1 <- go routes7 "fooo/baz"
    assertEqual "/fooo/baz" "topTop" r1

testRouting23 :: Test
testRouting23 = testCase "routing23" $ do
    r1 <- go routes7 "fooo/baz/quux"
    assertEqual "/fooo/baz/quux" "quux" r1

testRouting24 :: Test
testRouting24 = testCase "routing24" $ do
    r1 <- go routes7 "foooo/bar/bax"
    assertEqual "/foooo/bar/bax" "topTop" r1

testRouting25 :: Test
testRouting25 = testCase "routing25" $ do
    r1 <- go routes7 "foooo/bar/baz"
    assertEqual "/foooo/bar/baz" "bar" r1

testRouteLocal :: Test
testRouteLocal = testCase "routeLocal" $ do
    r4 <- go routesLocal "foo/bar/baz/quux"
    assertEqual "/foo/bar/baz/quux" "foo/bar/baz/quux" r4
    expectException $ go routesLocal "bar"
