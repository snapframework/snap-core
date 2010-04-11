{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Types.Tests
  ( tests ) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Trans (liftIO)
import           Data.ByteString.Char8 (ByteString)
import           Data.Iteratee
import qualified Data.Map as Map
import           Test.Framework
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import           Snap.Types
import           Snap.Internal.Http.Types
import           Snap.Iteratee

zomgRq :: Request
zomgRq = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 return Nothing GET (1,1) [] "/" "/" "/" "" Map.empty


rqWithBody :: Request
rqWithBody = Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 (enumBS "zazzle") Nothing GET (1,1) [] "/" "/" "/" ""
                 Map.empty


go :: Snap a -> IO (Request,Response)
go m = run $ runSnap m zomgRq


goBody :: Snap a -> IO (Request,Response)
goBody m = run $ runSnap m rqWithBody


tests :: [Test]
tests = [ testFail
        , testAlternative
        , testEarlyTermination
        , testRqBody
        , testTrivials]


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
    assertEqual "foo present" (Just ["Bar"]) $ getHeader "Foo" resp

    (_,resp2) <- go (fail ""
                       <|> fail2
                       <|> setFoo "Bar"
                       <|> setFoo "Baz")
    assertEqual "alternative chooses correct branch"
                (Just ["Bar"]) $ getHeader "Foo" resp2

  where
    fail2 :: Snap ()
    fail2 = pass >>= \_ -> return ()


sampleResponse :: Response
sampleResponse = addHeader "Foo" "Quux" $ emptyResponse


testEarlyTermination :: Test
testEarlyTermination = testCase "early termination" $ do
    (_,resp) <- go (finishWith sampleResponse >>= \_ -> setFoo "Bar")
    assertEqual "foo" (Just ["Quux"]) $ getHeader "Foo" resp


testRqBody :: Test
testRqBody = testCase "request bodies" $ do

    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    _ <- goBody $ f mvar1 mvar2

    v1 <- readMVar mvar1
    v2 <- readMVar mvar2

    assertEqual "rq body" "zazzle" v1
    assertEqual "rq body 2" "" v2

  where
    f mvar1 mvar2 = do
        getRequestBody >>= liftIO . putMVar mvar1
        getRequestBody >>= liftIO . putMVar mvar2


testTrivials :: Test
testTrivials = testCase "trivial functions" $ do
    (rq,rsp) <- go $ do
        req <- getRequest
        putRequest $ req { rqIsSecure=True }
        putResponse $ setResponseStatus 333 "333" sampleResponse
        r <- getResponse
        liftIO $ assertEqual "rsp status" 333 $ rspStatus r
        !_ <- localRequest (\r -> r {rqIsSecure=False}) $ do
            q <- getRequest
            liftIO $ assertEqual "localrq" False $ rqIsSecure q
            return ()
        return ()

    let !s = show NoHandlerException

    assertEqual "rq secure" True $ rqIsSecure rq
    assertEqual "rsp status" 333 $ rspStatus rsp
