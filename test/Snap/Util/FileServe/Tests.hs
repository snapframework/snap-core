{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Util.FileServe.Tests
  ( tests ) where

------------------------------------------------------------------------------
import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM_, liftM)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as S (break, breakEnd, drop, isInfixOf, pack)
import qualified Data.HashMap.Strict            as HashMap (empty, fromList, toList)
import qualified Data.Map                       as Map (empty)
import           Data.Maybe                     (fromJust, isJust)
import qualified Data.Text                      as T (unpack)
import           Snap.Internal.Core             (Snap, fixupResponse, pass, runSnap, writeBS)
import           Snap.Internal.Http.Types       (Request, Response (rspContentLength, rspStatus), getHeader, setHeader)
import           Snap.Internal.Util.FileServe   (DirectoryConfig (..), decodeFilePath, defaultMimeTypes, fancyDirectoryConfig, serveDirectory, serveDirectoryWith, serveFile, simpleDirectoryConfig)
import qualified Snap.Test                      as Test (buildRequest, get, getResponseBody, setQueryStringRaw)
import           Snap.Test.Common               (expectExceptionH)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertBool, assertEqual)
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative                  ((<$>))
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testFooBin
        , testFooTxt
        , testFooHtml
        , testFooBinBinBin
        , testDirectoryPasses
        , test404s
        , testFsSingle
        , testFsCfgA
        , testFsCfgB
        , testFsCfgC
        , testFsCfgD
        , testFsCfgFancy
        , testRangeOK
        , testRangeBad
        , testMultiRange
        , testIfRange
        , testBadUrl
        , testDecodeFilePath
        ]


------------------------------------------------------------------------------
expect404 :: IO Response -> IO ()
expect404 m = do
    r <- m
    assertBool "expected 404" (rspStatus r == 404)


------------------------------------------------------------------------------
expect302 :: ByteString -> IO Response -> IO ()
expect302 p m = do
    r <- m
    assertBool "expected 302" (rspStatus r == 302)
    assertEqual "redir location"
                (Just p)
                (getHeader "location" r)


------------------------------------------------------------------------------
runIt :: Snap a -> Request -> IO (Request, Response)
runIt m rq = do
    (rq', rsp) <- runSnap m d d rq
    rsp' <- fixupResponse rq rsp
    return (rq', rsp')
  where
    d = const $ return ()


------------------------------------------------------------------------------
go :: Snap a -> ByteString -> IO Response
go m s = do
    rq <- mkRequest s
    liftM snd (runIt m rq)


------------------------------------------------------------------------------
goIfModifiedSince :: Snap a -> ByteString -> ByteString -> IO Response
goIfModifiedSince m s lm = do
    rq <- mkRequest s
    let r = setHeader "if-modified-since" lm rq
    liftM snd (runIt m r)


------------------------------------------------------------------------------
goIfRange :: Snap a -> ByteString -> (Int,Int) -> ByteString -> IO Response
goIfRange m s (start,end) lm = do
    rq <- mkRequest s
    let r = setHeader "if-range" lm $
            setHeader "Range"
                       (S.pack $ "bytes=" ++ show start ++ "-" ++ show end)
                       rq
    liftM snd (runIt m r)


------------------------------------------------------------------------------
goRange :: Snap a -> ByteString -> (Int,Int) -> IO Response
goRange m s (start,end) = do
    rq' <- mkRequest s
    let rq = setHeader "Range"
                       (S.pack $ "bytes=" ++ show start ++ "-" ++ show end)
                       rq'
    liftM snd (runIt m rq)


------------------------------------------------------------------------------
goMultiRange :: Snap a -> ByteString -> (Int,Int) -> (Int,Int) -> IO Response
goMultiRange m s (start,end) (start2,end2) = do
    rq' <- mkRequest s
    let rq = setHeader "Range"
                       (S.pack $ "bytes=" ++ show start ++ "-" ++ show end
                                 ++ "," ++ show start2 ++ "-" ++ show end2)
                       rq'
    liftM snd (runIt m rq)


------------------------------------------------------------------------------
goRangePrefix :: Snap a -> ByteString -> Int -> IO Response
goRangePrefix m s start = do
    rq' <- mkRequest s
    let rq = setHeader "Range"
                       (S.pack $ "bytes=" ++ show start ++ "-")
                       rq'
    liftM snd (runIt m rq)


------------------------------------------------------------------------------
goRangeSuffix :: Snap a -> ByteString -> Int -> IO Response
goRangeSuffix m s end = do
    rq' <- mkRequest s
    let rq = setHeader "Range"
                       (S.pack $ "bytes=-" ++ show end)
                       rq'
    liftM snd (runIt m rq)


------------------------------------------------------------------------------
mkRequest :: ByteString -> IO Request
mkRequest uri = Test.buildRequest $ do
                    Test.get pathPart Map.empty
                    Test.setQueryStringRaw queryPart
  where
    (pathPart, queryPart) = breakQuery uri

    breakQuery s = (a, S.drop 1 b)
      where
        (a,b) = S.break (=='?') s


------------------------------------------------------------------------------
fs :: Snap ()
fs = do
    x <- serveDirectory "test/data/fileServe"
    return $! x `seq` ()


------------------------------------------------------------------------------
fsSingle :: Snap ()
fsSingle = do
    x <- serveFile "test/data/fileServe/foo.html"
    return $! x `seq` ()


------------------------------------------------------------------------------
fsCfg :: DirectoryConfig Snap -> Snap ()
fsCfg cfg = do
    x <- serveDirectoryWith cfg "test/data/fileServe"
    return $! x `seq` ()


------------------------------------------------------------------------------
testFooBin :: Test
testFooBin = testCase "fileServe/foo.bin" $ do
    r1 <- go fs "foo.bin"
    checkProps "fileServe/foo.bin/default" r1
    let !lm = fromJust $ getHeader "last-modified" r1

    go fs "foo.bin?blah=blah" >>= checkProps "fileServe/foo.bin/query-string"

    -- check last modified stuff
    r2 <- goIfModifiedSince fs "foo.bin" lm
    assertEqual "foo.bin 304" 304 $ rspStatus r2

    r3 <- goIfModifiedSince fs "foo.bin" "Wed, 15 Nov 1995 04:58:08 GMT"
    checkProps "fileServe/foo.bin/ifModifiedSince" r3

  where
    checkProps name r = do
        b <- Test.getResponseBody r

        assertEqual (name ++ "/contents") "FOO\n" b
        assertEqual (name ++ "/content-type")
                    (Just "application/octet-stream")
                    (getHeader "content-type" r)

        assertEqual (name ++ "/size") (Just 4) (rspContentLength r)

        assertBool  (name ++ "/last-modified")
                    (isJust $ getHeader "last-modified" r)

        assertEqual (name ++ "/accept-ranges")
                    (Just "bytes")
                    (getHeader "accept-ranges" r)


------------------------------------------------------------------------------
testFooTxt :: Test
testFooTxt = testCase "fileServe/foo.txt" $ do
    go fs "foo.txt" >>= checkProps "fileServe/foo.txt/default"
    go fs "foo.txt?blah=blah" >>= checkProps "fileServe/foo.txt/query"

  where
    checkProps name r = do
        b <- Test.getResponseBody r

        assertEqual (name ++ "/contents") "FOO\n" b
        assertEqual (name ++ "/content-type")
                    (Just "text/plain")
                    (getHeader "content-type" r)

        assertEqual (name ++ "/size") (Just 4) (rspContentLength r)

        assertBool  (name ++ "/last-modified")
                    (isJust $ getHeader "last-modified" r)

        assertEqual (name ++ "/accept-ranges")
                    (Just "bytes")
                    (getHeader "accept-ranges" r)


------------------------------------------------------------------------------
testFooHtml :: Test
testFooHtml = testCase "fileServe/foo.html" $ do
    go fs "foo.html" >>= checkProps "fileServe/foo.html/default"
    go fs "foo.html?bar=bar" >>= checkProps "fileServe/foo.html/query"

  where
    checkProps name r = do
        b <- Test.getResponseBody r

        assertEqual (name ++ "/contents") "FOO\n" b
        assertEqual (name ++ "/content-type")
                    (Just "text/html")
                    (getHeader "content-type" r)

        assertEqual (name ++ "/size") (Just 4) (rspContentLength r)

        assertBool  (name ++ "/last-modified")
                    (isJust $ getHeader "last-modified" r)

        assertEqual (name ++ "/accept-ranges")
                    (Just "bytes")
                    (getHeader "accept-ranges" r)


------------------------------------------------------------------------------
testFooBinBinBin :: Test
testFooBinBinBin = testCase "fileServe/foo.bin.bin.bin" $ do
    go fs "foo.bin.bin.bin" >>= checkProps "fileServe/foo.bin.bin.bin"

  where
    checkProps name r = do
        b <- Test.getResponseBody r

        assertEqual (name ++ "/contents") "FOO\n" b
        assertEqual (name ++ "/content-type")
                    (Just "application/octet-stream")
                    (getHeader "content-type" r)

        assertEqual (name ++ "/size") (Just 4) (rspContentLength r)

        assertBool  (name ++ "/last-modified")
                    (isJust $ getHeader "last-modified" r)

        assertEqual (name ++ "/accept-ranges")
                    (Just "bytes")
                    (getHeader "accept-ranges" r)


------------------------------------------------------------------------------
test404s :: Test
test404s = testCase "fileServe/404s" $ do
    expect404 $ go fs "jfldksjflksd"
    expect404 $ go fs "dummy/../foo.txt"
    expect404 $ go fs "/etc/password"

    coverMimeMap


------------------------------------------------------------------------------
printName :: FilePath -> Snap ()
printName c = writeBS $ snd $ S.breakEnd (=='/') $ S.pack c


------------------------------------------------------------------------------
cfgA, cfgB, cfgC, cfgD :: DirectoryConfig Snap
cfgA = DirectoryConfig {
         indexFiles      = []
       , indexGenerator  = const pass
       , dynamicHandlers = HashMap.empty
       , mimeTypes       = defaultMimeTypes
       , preServeHook    = \x -> x `seq` (return $! ())
       }

cfgB = DirectoryConfig {
         indexFiles      = ["index.txt", "altindex.html"]
       , indexGenerator  = const pass
       , dynamicHandlers = HashMap.empty
       , mimeTypes       = defaultMimeTypes
       , preServeHook    = \x -> x `seq` (return $! ())
       }

cfgC = DirectoryConfig {
         indexFiles      = ["index.txt", "altindex.html"]
       , indexGenerator  = printName
       , dynamicHandlers = HashMap.empty
       , mimeTypes       = defaultMimeTypes
       , preServeHook    = \x -> x `seq` (return $! ())
       }

cfgD = DirectoryConfig {
         indexFiles      = []
       , indexGenerator  = const pass
       , dynamicHandlers = HashMap.fromList [ (".txt", printName) ]
       , mimeTypes       = defaultMimeTypes
       , preServeHook    = \x -> x `seq` (return $! ())
       }


------------------------------------------------------------------------------
testFsCfgA :: Test
testFsCfgA = testCase "fileServe/cfgA" $ do
    let gooo = go (fsCfg cfgA)

    -- Named file in the root directory
    gooo "foo.bin" >>= checkProps "cfgA1/1" "application/octet-stream"
    gooo "foo.bin?blah=blah" >>=
         checkProps "cfgA1/2" "application/octet-stream"

    -- Missing file in the root directory
    expect404 $ gooo "bar.bin"

    -- Named file in a subdirectory
    gooo "mydir2/foo.txt" >>= checkProps "cfgA1/subdir/1" "text/plain"
    gooo "mydir2/foo.txt?z=z" >>= checkProps "cfgA1/subdir/2" "text/plain"

    -- Missing file in a subdirectory
    expect404 $ gooo "mydir2/bar.txt"

    -- Request for directory with no trailing slash
    expect302 "/mydir1/" $ gooo "mydir1"

    -- Request for directory with no trailing slash, with query param
    expect302 "/mydir1/?z=z" $ gooo "mydir1?z=z"

    -- Request for directory with trailing slash, no index
    expect404 $ gooo "mydir1/"
    expect404 $ gooo "mydir2/"

    -- Request file with trailing slash
    expect404 $ gooo "foo.html/"
    expect404 $ gooo "mydir2/foo.txt/"

  where
    checkProps name ct r = do
        b <- Test.getResponseBody r

        assertEqual (name ++ "/contents") "FOO\n" b
        assertEqual (name ++ "/content-type")
                    (Just ct)
                    (getHeader "content-type" r)

        assertEqual (name ++ "/size") (Just 4) (rspContentLength r)

        assertBool  (name ++ "/last-modified")
                    (isJust $ getHeader "last-modified" r)

        assertEqual (name ++ "/accept-ranges")
                    (Just "bytes")
                    (getHeader "accept-ranges" r)


------------------------------------------------------------------------------
testFsCfgB :: Test
testFsCfgB = testCase "fileServe/cfgB" $ do
    let gooo = go (fsCfg cfgB)

    -- Request for root directory with index
    rB1 <- gooo "mydir1/"
    bB1 <- Test.getResponseBody rB1

    assertEqual "B1" "INDEX\n" bB1
    assertEqual "B1 content-type"
                (Just "text/plain")
                (getHeader "content-type" rB1)

    -- Request for root directory with index, query
    rB2 <- gooo "mydir1/?z=z"
    bB2 <- Test.getResponseBody rB2

    assertEqual "B2" "INDEX\n" bB2
    assertEqual "B2 content-type"
                (Just "text/plain")
                (getHeader "content-type" rB2)


    -- Request for root directory with alternate index
    rB3 <- gooo "mydir3/"
    bB3 <- Test.getResponseBody rB3

    assertEqual "B3" "ALTINDEX\n" bB3
    assertEqual "B3 content-type"
                (Just "text/html")
                (getHeader "content-type" rB3)

    -- Request for root directory with no index
    expect404 $ gooo "mydir2/"


------------------------------------------------------------------------------
testFsCfgC :: Test
testFsCfgC = testCase "fileServe/cfgC" $ do
    let gooo = go (fsCfg cfgC)

    -- Request for root directory with index
    rC1 <- gooo "mydir1/"
    bC1 <- Test.getResponseBody rC1

    assertEqual "C1" "INDEX\n" bC1
    assertEqual "C1 content-type"
                (Just "text/plain")
                (getHeader "content-type" rC1)

    -- Request for root directory with index, query
    rC2 <- gooo "mydir1/?z=z"
    bC2 <- Test.getResponseBody rC2

    assertEqual "C2" "INDEX\n" bC2
    assertEqual "C2 content-type"
                (Just "text/plain")
                (getHeader "content-type" rC2)

    -- Request for root directory with generated index
    rC3 <- gooo "mydir2/"
    bC3 <- Test.getResponseBody rC3

    assertEqual "C3" "mydir2" bC3


------------------------------------------------------------------------------
testFsCfgD :: Test
testFsCfgD = testCase "fileServe/cfgD" $ do
    -- Request for file with dynamic handler
    rD1 <- go (fsCfg cfgD) "mydir2/foo.txt"
    bD1 <- Test.getResponseBody rD1

    assertEqual "D1" "foo.txt" bD1


------------------------------------------------------------------------------
testFsCfgFancy :: Test
testFsCfgFancy = testCase "fileServe/cfgFancy" $ do
    -- Request for directory with autogen index
    rE1 <- go (fsCfg fancyDirectoryConfig) "mydir2/"
    assertEqual "index-type" (Just "text/html; charset=utf-8")
                             (getHeader "content-type" rE1)
    bE1 <- Test.getResponseBody rE1

    assertBool "autogen-sub-index" $
        "Directory Listing: /mydir2/" `S.isInfixOf` bE1
    assertBool "autogen-sub-parent" $
        "<a href='../'" `S.isInfixOf` bE1
    assertBool "autogen-sub-dir" $
        "<a href='dir/'" `S.isInfixOf` bE1
    assertBool "autogen-sub-file" $
        "<a href='foo.txt'" `S.isInfixOf` bE1

    -- Request for directory with autogen index
    rE2 <- go (fsCfg fancyDirectoryConfig) "mydir2/?z=z"
    bE2 <- Test.getResponseBody rE2

    assertBool "autogen-sub-index" $
        "Directory Listing: /mydir2/" `S.isInfixOf` bE2
    assertBool "autogen-sub-parent" $
        "<a href='../'" `S.isInfixOf` bE2
    assertBool "autogen-sub-file" $
        "<a href='foo.txt'" `S.isInfixOf` bE2

    rE3 <- go (fsCfg fancyDirectoryConfig) "mydir2/foo.txt"
    assertEqual "c-t" (Just "text/plain") (getHeader "content-type" rE3)


------------------------------------------------------------------------------
testFsSingle :: Test
testFsSingle = testCase "fileServe/Single" $ do
    r1 <- go fsSingle "foo.html"
    b1 <- Test.getResponseBody r1

    assertEqual "foo.html" "FOO\n" b1
    assertEqual "foo.html content-type"
                (Just "text/html")
                (getHeader "content-type" r1)

    assertEqual "foo.html size" (Just 4) (rspContentLength r1)


------------------------------------------------------------------------------
testRangeOK :: Test
testRangeOK = testCase "fileServe/range/ok" $ do
    r1 <- goRange fsSingle "foo.html" (1,2)
    assertEqual "foo.html 206" 206 $ rspStatus r1
    b1 <- Test.getResponseBody r1

    assertEqual "foo.html partial" "OO" b1
    assertEqual "foo.html partial size" (Just 2) (rspContentLength r1)
    assertEqual "foo.html content-range"
                (Just "bytes 1-2/4")
                (getHeader "Content-Range" r1)

    r2 <- goRangeSuffix fsSingle "foo.html" 3
    assertEqual "foo.html 206" 206 $ rspStatus r2
    assertEqual "c-l" (Just 3) (rspContentLength r2)
    assertEqual "c-l hdr" (Just "3") (getHeader "content-length" r2)
    b2 <- Test.getResponseBody r2
    assertEqual "foo.html partial suffix" "OO\n" b2

    r3 <- goRangePrefix fsSingle "foo.html" 2
    assertEqual "foo.html 206" 206 $ rspStatus r3
    b3 <- Test.getResponseBody r3
    assertEqual "foo.html partial prefix" "O\n" b3


------------------------------------------------------------------------------
testMultiRange :: Test
testMultiRange = testCase "fileServe/range/multi" $ do
    r1 <- goMultiRange fsSingle "foo.html" (1,2) (3,3)

    -- we don't support multiple ranges so it's ok for us to return 200 here;
    -- test this behaviour
    assertEqual "foo.html 200" 200 $ rspStatus r1
    b1 <- Test.getResponseBody r1

    assertEqual "foo.html" "FOO\n" b1


------------------------------------------------------------------------------
testRangeBad :: Test
testRangeBad = testCase "fileServe/range/bad" $ do
    r1 <- goRange fsSingle "foo.html" (1,17)
    assertEqual "bad range" 416 (rspStatus r1)
    assertEqual "bad range content-range"
                (Just "bytes */4")
                (getHeader "Content-Range" r1)
    assertEqual "bad range content-length" (Just 0) (rspContentLength r1)
    b1 <- Test.getResponseBody r1
    assertEqual "bad range empty body" "" b1

    r2 <- goRangeSuffix fsSingle "foo.html" 4893
    assertEqual "bad suffix range" 416 $ rspStatus r2


------------------------------------------------------------------------------
coverMimeMap :: (Monad m) => m ()
coverMimeMap = Prelude.mapM_ f $ HashMap.toList defaultMimeTypes
  where
    f (!k,!v) = return $ case k `seq` v `seq` () of () -> ()


------------------------------------------------------------------------------
testIfRange :: Test
testIfRange = testCase "fileServe/range/if-range" $ do
    r <- goIfRange fs "foo.bin" (1,2) "Wed, 15 Nov 1995 04:58:08 GMT"
    assertEqual "foo.bin 200" 200 $ rspStatus r
    b <- Test.getResponseBody r
    assertEqual "foo.bin" "FOO\n" b

    r2 <- goIfRange fs "foo.bin" (1,2) "Tue, 01 Oct 2030 04:58:08 GMT"
    assertEqual "foo.bin 206" 206 $ rspStatus r2
    b2 <- Test.getResponseBody r2
    assertEqual "foo.bin partial" "OO" b2

    r3 <- goIfRange fs "foo.bin" (1,24324) "Tue, 01 Oct 2030 04:58:08 GMT"
    assertEqual "foo.bin 200" 200 $ rspStatus r3
    b3 <- Test.getResponseBody r3
    assertEqual "foo.bin" "FOO\n" b3


------------------------------------------------------------------------------
testBadUrl :: Test
testBadUrl = testCase "fileServe/badUrl" $ do
    expectExceptionH $ go (fs <|> error "foo") "%$z%%%%%%%%"
    expectExceptionH $ go (fs <|> error "foo") "//etc/passwd"


------------------------------------------------------------------------------
testDirectoryPasses :: Test
testDirectoryPasses = testCase "fileServe/directory-passes" $ do
    expectExceptionH $ go (fs <|> error "foo") ""
    expectExceptionH $ go (fsCfg simpleDirectoryConfig <|> error "foo") ""
    r <- go (fsCfg simpleDirectoryConfig) "foo.txt"
    assertEqual "c-t" (Just "text/plain") (getHeader "content-type" r)


------------------------------------------------------------------------------
testDecodeFilePath :: Test
testDecodeFilePath = testCase "fileServe/decodeFilePath" $ do
    forM_ table $ \(nm, inp, expected) -> do
        out <- (map fromEnum . T.unpack) <$> decodeFilePath inp
        assertEqual nm expected out
  where
    table = [ ("bad"     ,  "\x00\xd8\x00\xd8"       , [0, 0xd8, 0, 0xd8]   )
            , ("bom"     , "\xfe\xff"                , [0xfe, 0xff]         )
            , ("ok"      , "ok"                      , [fromEnum 'o',
                                                        fromEnum 'k']       )
            ]
