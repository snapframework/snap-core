{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Util.FileServe.Tests
  ( tests ) where

import           Blaze.ByteString.Builder
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Prelude hiding (take)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import           Snap.Internal.Http.Types
import           Snap.Internal.Types
import           Snap.Util.FileServe
import           Snap.Iteratee

tests :: [Test]
tests = [ testFs
        , testFsCfg
        , testFsSingle
        , testRangeOK
        , testRangeBad
        , testMultiRange
        , testIfRange ]


expect404 :: IO Response -> IO ()
expect404 m = do
    r <- m
    assertBool "expected 404" (rspStatus r == 404)


expect302 :: ByteString -> IO Response -> IO ()
expect302 p m = do
    r <- m
    assertBool "expected 302" (rspStatus r == 302)
    assertEqual "redir location"
                (Just p)
                (getHeader "location" r)


getBody :: Response -> IO L.ByteString
getBody r = do
    let benum = rspBodyToEnum $ rspBody r
    liftM (toLazyByteString . mconcat) (runIteratee consume >>= run_ . benum)


runIt :: Snap a -> Request -> Iteratee ByteString IO (Request, Response)
runIt m rq = runSnap m d d rq
  where
    d = const $ return ()

go :: Snap a -> ByteString -> IO Response
go m s = do
    rq <- mkRequest s
    liftM snd (run_ $ runIt m rq)


goIfModifiedSince :: Snap a -> ByteString -> ByteString -> IO Response
goIfModifiedSince m s lm = do
    rq <- mkRequest s
    let r = setHeader "if-modified-since" lm rq
    liftM snd (run_ $ runIt m r)


goIfRange :: Snap a -> ByteString -> (Int,Int) -> ByteString -> IO Response
goIfRange m s (start,end) lm = do
    rq <- mkRequest s
    let r = setHeader "if-range" lm $
            setHeader "Range"
                       (S.pack $ "bytes=" ++ show start ++ "-" ++ show end)
                       rq
    liftM snd (run_ $ runIt m r)


goRange :: Snap a -> ByteString -> (Int,Int) -> IO Response
goRange m s (start,end) = do
    rq' <- mkRequest s
    let rq = setHeader "Range"
                       (S.pack $ "bytes=" ++ show start ++ "-" ++ show end)
                       rq'
    liftM snd (run_ $ runIt m rq)


goMultiRange :: Snap a -> ByteString -> (Int,Int) -> (Int,Int) -> IO Response
goMultiRange m s (start,end) (start2,end2) = do
    rq' <- mkRequest s
    let rq = setHeader "Range"
                       (S.pack $ "bytes=" ++ show start ++ "-" ++ show end
                                 ++ "," ++ show start2 ++ "-" ++ show end2)
                       rq'
    liftM snd (run_ $ runIt m rq)


goRangePrefix :: Snap a -> ByteString -> Int -> IO Response
goRangePrefix m s start = do
    rq' <- mkRequest s
    let rq = setHeader "Range"
                       (S.pack $ "bytes=" ++ show start ++ "-")
                       rq'
    liftM snd (run_ $ runIt m rq)


goRangeSuffix :: Snap a -> ByteString -> Int -> IO Response
goRangeSuffix m s end = do
    rq' <- mkRequest s
    let rq = setHeader "Range"
                       (S.pack $ "bytes=-" ++ show end)
                       rq'
    liftM snd (run_ $ runIt m rq)


mkRequest :: ByteString -> IO Request
mkRequest uri = do
    enum <- newIORef $ SomeEnumerator returnI
    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                     enum Nothing GET (1,1) [] "" uri "/"
                     (S.concat ["/",uri]) "" Map.empty


fs :: Snap ()
fs = do
    x <- serveDirectory "data/fileServe"
    return $! x `seq` ()


fsSingle :: Snap ()
fsSingle = do
    x <- serveFile "data/fileServe/foo.html"
    return $! x `seq` ()


fsCfg :: DirectoryConfig Snap -> Snap ()
fsCfg cfg = do
    x <- serveDirectoryWith cfg "data/fileServe"
    return $! x `seq` ()


testFs :: Test
testFs = testCase "fileServe/multi" $ do
    r1 <- go fs "foo.bin"
    b1 <- getBody r1

    assertEqual "foo.bin" "FOO\n" b1
    assertEqual "foo.bin content-type"
                (Just "application/octet-stream")
                (getHeader "content-type" r1)

    assertEqual "foo.bin size" (Just 4) (rspContentLength r1)

    assertBool "last-modified header" (isJust $ getHeader "last-modified" r1)
    assertEqual "accept-ranges header" (Just "bytes")
                                       (getHeader "accept-ranges" r1)

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
    
    r6 <- go fs "foo.bin.bin.bin"
    b6 <- getBody r6

    assertEqual "foo.bin.bin.bin" "FOO\n" b6
    assertEqual "foo.bin.bin.bin content-type"
                (Just "application/octet-stream")
                (getHeader "content-type" r6)

    expect404 $ go fs "jfldksjflksd"
    expect404 $ go fs "dummy/../foo.txt"
    expect404 $ go fs "/etc/password"

    coverMimeMap


testFsSingle :: Test
testFsSingle = testCase "fileServe/Single" $ do
    r1 <- go fsSingle "foo.html"
    b1 <- getBody r1

    assertEqual "foo.html" "FOO\n" b1
    assertEqual "foo.html content-type"
                (Just "text/html")
                (getHeader "content-type" r1)

    assertEqual "foo.html size" (Just 4) (rspContentLength r1)


testFsCfg :: Test
testFsCfg = testCase "fileServe/Cfg" $ do

    let cfgA = DirectoryConfig {
        indexFiles      = [],
        indexGenerator  = const pass,
        dynamicHandlers = Map.empty,
        mimeTypes       = defaultMimeTypes
        }

    -- Named file in the root directory
    rA1 <- go (fsCfg cfgA) "foo.bin"
    bA1 <- getBody rA1

    assertEqual "A1" "FOO\n" bA1
    assertEqual "A1 content-type"
                (Just "application/octet-stream")
                (getHeader "content-type" rA1)

    -- Missing file in the root directory
    expect404 $ go (fsCfg cfgA) "bar.bin"

    -- Named file in a subdirectory
    rA2 <- go (fsCfg cfgA) "mydir2/foo.txt"
    bA2 <- getBody rA2

    assertEqual "A2" "FOO\n" bA2
    assertEqual "A2 content-type"
                (Just "text/plain")
                (getHeader "content-type" rA2)

    -- Missing file in a subdirectory
    expect404 $ go (fsCfg cfgA) "mydir2/bar.txt"

    -- Request for directory with no trailing slash
    expect302 "/mydir1/" $ go (fsCfg cfgA) "mydir1"

    -- Request for directory with trailing slash, no index
    expect404 $ go (fsCfg cfgA) "mydir1/"
    expect404 $ go (fsCfg cfgA) "mydir2/"

    -- Request file with trailing slash
    expect404 $ go (fsCfg cfgA) "foo.html/"
    expect404 $ go (fsCfg cfgA) "mydir2/foo.txt/"

    let cfgB = DirectoryConfig {
        indexFiles      = ["index.txt", "altindex.html"],
        indexGenerator  = const pass,
        dynamicHandlers = Map.empty,
        mimeTypes       = defaultMimeTypes
        }

    -- Request for root directory with index
    rB1 <- go (fsCfg cfgB) "mydir1/"
    bB1 <- getBody rB1

    assertEqual "B1" "INDEX\n" bB1
    assertEqual "B1 content-type"
                (Just "text/plain")
                (getHeader "content-type" rB1)

    -- Request for root directory with alternate index
    rB2 <- go (fsCfg cfgB) "mydir3/"
    bB2 <- getBody rB2

    assertEqual "B2" "ALTINDEX\n" bB2
    assertEqual "B2 content-type"
                (Just "text/html")
                (getHeader "content-type" rB2)

    -- Request for root directory with no index
    expect404 $ go (fsCfg cfgB) "mydir2/"

    let printName c = writeBS $ snd $ S.breakEnd (=='/') $ S.pack c

    let cfgC = DirectoryConfig {
        indexFiles      = ["index.txt", "altindex.html"],
        indexGenerator  = printName,
        dynamicHandlers = Map.empty,
        mimeTypes       = defaultMimeTypes
        }

    -- Request for root directory with index
    rC1 <- go (fsCfg cfgC) "mydir1/"
    bC1 <- getBody rC1

    assertEqual "C1" "INDEX\n" bC1
    assertEqual "C1 content-type"
                (Just "text/plain")
                (getHeader "content-type" rC1)

    -- Request for root directory with generated index
    rC2 <- go (fsCfg cfgC) "mydir2/"
    bC2 <- getBody rC2

    assertEqual "C2" "mydir2" bC2

    let cfgD = DirectoryConfig {
        indexFiles      = [],
        indexGenerator  = const pass,
        dynamicHandlers = Map.fromList [ (".txt", printName) ],
        mimeTypes       = defaultMimeTypes
        }

    -- Request for file with dynamic handler
    rD1 <- go (fsCfg cfgD) "mydir2/foo.txt"
    bD1 <- getBody rD1

    assertEqual "D1" "foo.txt" bD1

    -- Request for directory with autogen index
    rE1 <- go (fsCfg fancyDirectoryConfig) "mydir2/"
    bE1 <- S.concat `fmap` L.toChunks `fmap` getBody rE1

    assertBool "autogen-sub-index" $
        "Directory Listing: /mydir2/" `S.isInfixOf` bE1
    assertBool "autogen-sub-parent" $
        "<a href='../'" `S.isInfixOf` bE1
    assertBool "autogen-sub-file" $
        "<a href='foo.txt'" `S.isInfixOf` bE1


testRangeOK :: Test
testRangeOK = testCase "fileServe/range/ok" $ do
    r1 <- goRange fsSingle "foo.html" (1,2)
    assertEqual "foo.html 206" 206 $ rspStatus r1
    b1 <- getBody r1

    assertEqual "foo.html partial" "OO" b1
    assertEqual "foo.html partial size" (Just 2) (rspContentLength r1)
    assertEqual "foo.html content-range"
                (Just "bytes 1-2/4")
                (getHeader "Content-Range" r1)

    r2 <- goRangeSuffix fsSingle "foo.html" 3
    assertEqual "foo.html 206" 206 $ rspStatus r2
    b2 <- getBody r2
    assertEqual "foo.html partial suffix" "OO\n" b2

    r3 <- goRangePrefix fsSingle "foo.html" 2
    assertEqual "foo.html 206" 206 $ rspStatus r3
    b3 <- getBody r3
    assertEqual "foo.html partial prefix" "O\n" b3


testMultiRange :: Test
testMultiRange = testCase "fileServe/range/multi" $ do
    r1 <- goMultiRange fsSingle "foo.html" (1,2) (3,3)

    -- we don't support multiple ranges so it's ok for us to return 200 here;
    -- test this behaviour
    assertEqual "foo.html 200" 200 $ rspStatus r1
    b1 <- getBody r1

    assertEqual "foo.html" "FOO\n" b1


testRangeBad :: Test
testRangeBad = testCase "fileServe/range/bad" $ do
    r1 <- goRange fsSingle "foo.html" (1,17)
    assertEqual "bad range" 416 (rspStatus r1)
    assertEqual "bad range content-range"
                (Just "bytes */4")
                (getHeader "Content-Range" r1)
    assertEqual "bad range content-length" (Just 0) (rspContentLength r1)
    b1 <- getBody r1
    assertEqual "bad range empty body" "" b1

    r2 <- goRangeSuffix fsSingle "foo.html" 4893
    assertEqual "bad suffix range" 416 $ rspStatus r2


coverMimeMap :: (Monad m) => m ()
coverMimeMap = Prelude.mapM_ f $ Map.toList defaultMimeTypes
  where
    f (!k,!v) = return $ case k `seq` v `seq` () of () -> ()


testIfRange :: Test
testIfRange = testCase "fileServe/range/if-range" $ do
    r <- goIfRange fs "foo.bin" (1,2) "Wed, 15 Nov 1995 04:58:08 GMT"
    assertEqual "foo.bin 200" 200 $ rspStatus r
    b <- getBody r
    assertEqual "foo.bin" "FOO\n" b

    r2 <- goIfRange fs "foo.bin" (1,2) "Tue, 01 Oct 2030 04:58:08 GMT"
    assertEqual "foo.bin 206" 206 $ rspStatus r2
    b2 <- getBody r2
    assertEqual "foo.bin partial" "OO" b2

    r3 <- goIfRange fs "foo.bin" (1,24324) "Tue, 01 Oct 2030 04:58:08 GMT"
    assertEqual "foo.bin 200" 200 $ rspStatus r3
    b3 <- getBody r3
    assertEqual "foo.bin" "FOO\n" b3
