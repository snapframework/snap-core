{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Types.Tests
  ( tests ) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans (liftIO)
import           Control.Parallel.Strategies
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.IORef
import           Data.Iteratee
import           Data.Text ()
import           Data.Text.Lazy ()
import qualified Data.Map as Map
import           Prelude hiding (catch)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)

import           Snap.Internal.Types
import           Snap.Internal.Http.Types
import           Snap.Iteratee
import           Snap.Test.Common ()


tests :: [Test]
tests = [ testFail
        , testAlternative
        , testEarlyTermination
        , testRqBody
        , testTrivials
        , testMethod
        , testDir
        , testCatchIO
        , testWrites
        , testParam
        , testURLEncode1
        , testURLEncode2
        , testDir2
        , testIpHeaderFilter
        , testMZero404
        , testEvalSnap
        , testLocalRequest
        , testRedirect ]


expectException :: IO () -> IO ()
expectException m = do
    r <- (try m :: IO (Either SomeException ()))
    let b = either (\e -> show e `using` rdeepseq `seq` True)
                   (const False) r
    assertBool "expected exception" b


expect404 :: IO (Request,Response) -> IO ()
expect404 m = do
    (_,r) <- m
    assertBool "expected 404" (rspStatus r == 404)


expectNo404 :: IO (Request,Response) -> IO ()
expectNo404 m = do
    (_,r) <- m
    assertBool ("expected 200, got " ++ show (rspStatus r))
               (rspStatus r /= 404)


mkRequest :: ByteString -> IO Request
mkRequest uri = do
    enum <- newIORef $ SomeEnumerator return

    return $ Request "foo" 80 "127.0.0.1" 999 "foo" 1000 "foo" False Map.empty
                     enum Nothing GET (1,1) [] "" uri "/"
                     (S.concat ["/",uri]) "" Map.empty

mkRequestQuery :: ByteString -> ByteString -> [ByteString] -> IO Request
mkRequestQuery uri k v = do
    enum <- newIORef $ SomeEnumerator return

    let mp = Map.fromList [(k,v)]
    let q  = S.concat [k,"=", S.concat v]

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                     enum Nothing GET (1,1) [] "" uri "/"
                     (S.concat ["/",uri,"?",q]) q mp


mkZomgRq :: IO Request
mkZomgRq = do
    enum <- newIORef $ SomeEnumerator return

    return $ Request "foo" 80 "127.0.0.1" 999 "foo" 1000 "foo" False Map.empty
                     enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty


mkIpHeaderRq :: IO Request
mkIpHeaderRq = do
    rq <- mkZomgRq
    return $ setHeader "X-Forwarded-For" "1.2.3.4" rq


mkRqWithBody :: IO Request
mkRqWithBody = do
    enum <- newIORef $ SomeEnumerator (enumBS "zazzle")
    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 enum Nothing GET (1,1) [] "" "/" "/" "/" ""
                 Map.empty


testCatchIO :: Test
testCatchIO = testCase "catchIO" $ do
    (_,rsp)  <- go f
    (_,rsp2) <- go g

    assertEqual "catchIO 1" (Just "bar") $ getHeader "foo" rsp
    assertEqual "catchIO 2" Nothing $ getHeader "foo" rsp2

  where
    f :: Snap ()
    f = (block $ unblock $ throw NoHandlerException) `catch` h

    g :: Snap ()
    g = return () `catch` h

    h :: SomeException -> Snap ()
    h e = e `seq` modifyResponse $ addHeader "foo" "bar"

go :: Snap a -> IO (Request,Response)
go m = do
    zomgRq <- mkZomgRq
    run $ runSnap m (\x -> return $! (show x `using` rdeepseq) `seq` ()) zomgRq


goIP :: Snap a -> IO (Request,Response)
goIP m = do
    rq <- mkIpHeaderRq
    run $ runSnap m (const $ return ()) rq


goPath :: ByteString -> Snap a -> IO (Request,Response)
goPath s m = do
    rq <- mkRequest s
    run $ runSnap m (const $ return ()) rq


goPathQuery :: ByteString
            -> ByteString
            -> [ByteString]
            -> Snap a
            -> IO (Request,Response)
goPathQuery s k v m = do
    rq <- mkRequestQuery s k v
    run $ runSnap m (const $ return ()) rq


goBody :: Snap a -> IO (Request,Response)
goBody m = do
    rq <- mkRqWithBody
    run $ runSnap m (const $ return ()) rq


testFail :: Test
testFail = testCase "failure" $ expect404 (go pass)


setFoo :: ByteString -> Snap ()
setFoo s = do
    modifyResponse (addHeader "Foo" s)
    fmap id $ pure ()
    pure id <*> (liftIO $ return ())


testAlternative :: Test
testAlternative = testCase "alternative" $ do
    (_,resp) <- go (pass <|> setFoo "Bar")
    assertEqual "foo present" (Just "Bar") $ getHeader "Foo" resp

    (_,resp2) <- go (fail ""
                       <|> fail2
                       <|> setFoo "Bar"
                       <|> setFoo "Baz")
    assertEqual "alternative chooses correct branch"
                (Just ["Bar"]) $ getHeaders "Foo" resp2

  where
    fail2 :: Snap ()
    fail2 = pass >>= \_ -> return ()


sampleResponse :: Response
sampleResponse = addHeader "Foo" "Quux" $ emptyResponse


testEarlyTermination :: Test
testEarlyTermination = testCase "early termination" $ do
    (_,resp) <- go (finishWith sampleResponse >>= \_ -> setFoo "Bar")
    assertEqual "foo" (Just ["Quux"]) $ getHeaders "Foo" resp


testRqBody :: Test
testRqBody = testCase "request bodies" $ do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    _ <- goBody $ f mvar1 mvar2

    v1 <- takeMVar mvar1
    v2 <- takeMVar mvar2

    assertEqual "rq body" "zazzle" v1
    assertEqual "rq body 2" "" v2

    _ <- goBody $ g mvar1 mvar2
    w1 <- takeMVar mvar1
    w2 <- takeMVar mvar2

    assertEqual "rq body" "zazzle" w1
    assertEqual "rq body 2" "" w2



  where
    f mvar1 mvar2 = do
        getRequestBody >>= liftIO . putMVar mvar1
        getRequestBody >>= liftIO . putMVar mvar2

    g mvar1 mvar2 = do
        senum <- unsafeDetachRequestBody
        let (SomeEnumerator enum) = senum
        bs <- liftM fromWrap (liftIO $ enum stream2stream >>= run)
        liftIO $ putMVar mvar1 bs
        getRequestBody >>= liftIO . putMVar mvar2


testTrivials :: Test
testTrivials = testCase "trivial functions" $ do
    (rq,rsp) <- go $ do
        req <- getRequest
        putRequest $ req { rqIsSecure=True }
        putResponse $ setResponseStatus 333 "333" sampleResponse
        r <- getResponse
        liftIO $ assertEqual "rsp status" 333 $ rspStatus r
        !_ <- localRequest (\x -> x {rqIsSecure=False}) $ do
            q <- getRequest
            liftIO $ assertEqual "localrq" False $ rqIsSecure q
            return ()

        logError "foo"
        writeText "zzz"
        writeLazyText "zzz"

        let req' = updateContextPath 0 req
        let cp1 = rqContextPath req
        let cp2 = rqContextPath req'

        liftIO $ assertEqual "updateContextPath 0" cp1 cp2

        withRequest $ return . (`seq` ())
        withResponse $ return . (`seq` ())


        return ()

    b <- getBody rsp
    let !_ = show b `using` rdeepseq


    let !_ = show NoHandlerException `seq` ()

    assertEqual "rq secure" True $ rqIsSecure rq
    assertEqual "rsp status" 333 $ rspStatus rsp


testMethod :: Test
testMethod = testCase "method" $ do
   expect404 $ go (method POST $ return ())
   expectNo404 $ go (method GET $ return ())


testDir :: Test
testDir = testCase "dir" $ do
   expect404 $ goPath "foo/bar" (dir "zzz" $ return ())
   expectNo404 $ goPath "foo/bar" (dir "foo" $ return ())
   expect404 $ goPath "fooz/bar" (dir "foo" $ return ())
   expectNo404 $ goPath "foo/bar" (path "foo/bar" $ return ())
   expect404 $ goPath "foo/bar/z" (path "foo/bar" $ return ())
   expectNo404 $ goPath "" (ifTop $ return ())
   expect404 $ goPath "a" (ifTop $ return ())


testParam :: Test
testParam = testCase "getParam" $ do
    expect404 $ goPath "/foo" f
    expectNo404 $ goPathQuery "/foo" "param" ["foo"] f
  where
    f = do
        mp <- getParam "param"
        maybe pass
              (\s -> if s == "foo" then return () else pass)
              mp


getBody :: Response -> IO L.ByteString
getBody r = liftM fromWrap ((rspBodyToEnum $ rspBody r) stream2stream >>= run)


testWrites :: Test
testWrites = testCase "writes" $ do
    (_,r) <- go h
    b <- getBody r
    assertEqual "output functions" "Foo1Foo2Foo3" b
  where
    h :: Snap ()
    h = do
        addToOutput $ enumBS "Foo1"
        writeBS "Foo2"
        writeLBS "Foo3"


testURLEncode1 :: Test
testURLEncode1 = testCase "url encoding 1" $ do
    let b = urlEncode "the quick brown fox~#"
    assertEqual "url encoding 1" "the+quick+brown+fox%7e%23" b
    assertEqual "fail" Nothing $ urlDecode "%"


testURLEncode2 :: Test
testURLEncode2 = testProperty "url encoding 2" prop
  where
    prop s = (urlDecode $ urlEncode s) == Just s


testDir2 :: Test
testDir2 = testCase "dir2" $ do
    (_,resp) <- goPath "foo/bar" f
    b <- getBody resp
    assertEqual "context path" "/foo/bar/" b

  where
    f = dir "foo" $ dir "bar" $ do
            p <- liftM rqContextPath getRequest
            addToOutput $ enumBS p


testIpHeaderFilter :: Test
testIpHeaderFilter = testCase "ipHeaderFilter" $ do
    (_,r) <- goIP f
    b <- getBody r
    assertEqual "ipHeaderFilter" "1.2.3.4" b


    (_,r2) <- go f
    b2 <- getBody r2
    assertEqual "ipHeaderFilter" "127.0.0.1" b2

  where
    f = do
        ipHeaderFilter
        ip <- liftM rqRemoteAddr getRequest
        writeBS ip


testMZero404 :: Test
testMZero404 = testCase "mzero 404" $ do
    (_,r) <- go mzero
    let l = rspContentLength r
    b <- getBody r
    assertEqual "mzero 404" "404" b
    assertEqual "mzero 404 length" (Just 3) l


testEvalSnap :: Test
testEvalSnap = testCase "evalSnap exception" $ do
    rq <- mkZomgRq
    expectException (run $ evalSnap f
                                    (const $ return ())
                                    rq >> return ())
  where
    f = do
        logError "zzz"
        v <- withResponse (return . rspHttpVersion)
        liftIO $ assertEqual "evalSnap rsp version" (1,1) v
        finishWith emptyResponse


testLocalRequest :: Test
testLocalRequest = testCase "localRequest" $ do
    rq1 <- mkZomgRq
    rq2 <- mkRequest "zzz/zz/z"

    let h = localRequest (const rq2) mzero

    (rq',_) <- go (h <|> return ())

    let u1 = rqURI rq1
    let u2 = rqURI rq'

    assertEqual "localRequest backtrack" u1 u2



testRedirect :: Test
testRedirect = testCase "redirect" $ do
    (_,rsp)  <- go (redirect "/foo/bar")

    assertEqual "redirect path" (Just "/foo/bar") $ getHeader "Location" rsp
    assertEqual "redirect status" 302 $ rspStatus rsp
    assertEqual "status description" "Found" $ rspStatusReason rsp


    (_,rsp)  <- go (redirect' "/bar/foo" 307)

    assertEqual "redirect path" (Just "/bar/foo") $ getHeader "Location" rsp
    assertEqual "redirect status" 307 $ rspStatus rsp
    assertEqual "status description" "Temporary Redirect" $ rspStatusReason rsp
