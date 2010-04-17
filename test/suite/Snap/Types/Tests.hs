{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Types.Tests
  ( tests ) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Iteratee
import qualified Data.Map as Map
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import           Snap.Internal.Types
import           Snap.Internal.Http.Types
import           Snap.Iteratee



tests :: [Test]
tests = [ testFail
        , testAlternative
        , testEarlyTermination
        , testRqBody
        , testTrivials
        , testMethod
        , testDir
        , testWrites ]


expectException :: IO a -> IO ()
expectException m = do
    e <- try m
    case e of
      Left (z::SomeException)  -> (show z) `seq` return ()
      Right _ -> assertFailure "expected exception, didn't get one"


expectNoException :: IO a -> IO ()
expectNoException m = do
    e <- try m
    case e of
      Left (_::SomeException) -> assertFailure "expected no exception, got one"
      Right _ -> return ()


mkRequest :: ByteString -> Request
mkRequest uri = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                        return Nothing GET (1,1) [] uri "/"
                        (B.concat ["/",uri]) "" Map.empty

zomgRq :: Request
zomgRq = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 return Nothing GET (1,1) [] "/" "/" "/" "" Map.empty


rqWithBody :: Request
rqWithBody = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 (enumBS "zazzle") Nothing GET (1,1) [] "/" "/" "/" ""
                 Map.empty


go :: Snap a -> IO (Request,Response)
go m = run $ runSnap m zomgRq


goPath :: ByteString -> Snap a -> IO (Request,Response)
goPath s m = run $ runSnap m $ mkRequest s


goBody :: Snap a -> IO (Request,Response)
goBody m = run $ runSnap m rqWithBody


testFail :: Test
testFail = testCase "failure" $ do
    x <- try (go pass)
    assertBool "fail" $
      case x of
        Left NoHandlerException -> True
        Right (_,_)             -> False


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
        enum <- unsafeDetachRequestBody
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
        return ()

    let !_ = show NoHandlerException `seq` ()

    assertEqual "rq secure" True $ rqIsSecure rq
    assertEqual "rsp status" 333 $ rspStatus rsp


testMethod :: Test
testMethod = testCase "method" $ do
   expectException $ go (method POST $ return ())
   expectNoException $ go (method GET $ return ())


testDir :: Test
testDir = testCase "dir" $ do
   expectException $ goPath "foo/bar" (dir "zzz" $ return ())
   expectNoException $ goPath "foo/bar" (dir "foo" $ return ())
   expectException $ goPath "fooz/bar" (dir "foo" $ return ())
   expectNoException $ goPath "foo/bar" (path "foo/bar" $ return ())
   expectException $ goPath "foo/bar/z" (path "foo/bar" $ return ())
   expectNoException $ goPath "" (ifTop $ return ())
   expectException $ goPath "a" (ifTop $ return ())


getBody :: Response -> IO L.ByteString
getBody r = liftM fromWrap (rspBody r stream2stream >>= run)


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
