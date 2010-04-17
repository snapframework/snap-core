{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Util.FileServe.Tests
  ( tests ) where

import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as Map
import           Data.Maybe
import           Prelude hiding (take)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import           Snap.Internal.Http.Types
import           Snap.Internal.Types
import           Snap.Util.FileServe
import           Snap.Iteratee

tests :: [Test]
tests = [ testFs ]

expectException :: IO a -> IO ()
expectException m = do
    e <- try m
    case e of
      Left (z::SomeException)  -> (show z) `seq` return ()
      Right _ -> assertFailure "expected exception, didn't get one"


getBody :: Response -> IO L.ByteString
getBody r = liftM fromWrap (rspBody r stream2stream >>= run)


go :: Snap a -> ByteString -> IO Response
go m s = liftM snd (run $ runSnap m $ mkRequest s)

goIfModifiedSince :: Snap a -> ByteString -> ByteString -> IO Response
goIfModifiedSince m s lm = liftM snd (run $ runSnap m r)
  where
    r = setHeader "if-modified-since" lm $ mkRequest s


mkRequest :: ByteString -> Request
mkRequest uri = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                        return Nothing GET (1,1) [] uri "/"
                        (B.concat ["/",uri]) "" Map.empty

fs :: Snap ()
fs = fileServe "data/fileServe"

testFs :: Test
testFs = testCase "fileServe" $ do
    r1 <- go fs "foo.bin"
    b1 <- getBody r1

    assertEqual "foo.bin" "FOO\n" b1
    assertEqual "foo.bin content-type"
                (Just "application/octet-stream")
                (getHeader "content-type" r1)

    assertEqual "foo.bin size" (Just 4) (rspContentLength r1)

    assertBool "last-modified header" (isJust $ getHeader "last-modified" r1)
    let !lm = fromJust $ getHeader "last-modified" r1

    -- check last modified stuff
    r2 <- goIfModifiedSince fs "foo.bin" lm
    assertEqual "foo.bin 304" 304 $ rspStatus r2

    r3 <- goIfModifiedSince fs "foo.bin" "Wed, 15 Nov 1995 04:58:08 GMT"
    assertEqual "foo.bin 200" 200 $ rspStatus r3
    b3 <- getBody r3
    assertEqual "foo.bin 2" "FOO\n" b3

    r4 <- go fs "foo.txt"
    b4 <- getBody r4

    assertEqual "foo.txt" "FOO\n" b4
    assertEqual "foo.txt content-type"
                (Just "text/plain")
                (getHeader "content-type" r4)
    
    r5 <- go fs "foo.html"
    b5 <- getBody r5

    assertEqual "foo.html" "FOO\n" b5
    assertEqual "foo.html content-type"
                (Just "text/html")
                (getHeader "content-type" r5)
    

    expectException $ go fs "jfldksjflksd"
    expectException $ go fs "dummy/../foo.txt"
    expectException $ go fs "/etc/password"

