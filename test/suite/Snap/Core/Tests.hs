{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Core.Tests
  ( tests ) where

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception.Lifted ( ErrorCall(..)
                                          , Exception
                                          , SomeException(..)
                                          , catch
                                          , fromException
                                          , mask
                                          , throwIO
                                          , try
                                          )
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Parallel.Strategies
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntMap as IM
import           Data.IORef
import           Data.Maybe (isJust)
import           Data.Text ()
import           Data.Text.Lazy ()
import qualified Data.Map as Map
import           Prelude hiding (catch)
import qualified System.IO.Streams as Streams
import           System.IO.Streams (InputStream)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)
import           Test.QuickCheck(oneof, variant, elements, Gen, arbitrary)

import           Snap.Internal.Http.Types
import           Snap.Internal.Parsing
import           Snap.Internal.Types
import qualified Snap.Test as Test
import           Snap.Test.Common


tests :: [Test]
tests = [ testFail
        , testAlternative
        , testEarlyTermination
        , testEscapeHttp
        , testCatchFinishWith
        , testRqBody
        , testRqBodyTooLong
        , testRqBodyException
        , testRqBodyTermination
        , testTrivials
        , testMethod
        , testMethods
        , testMethodEq
        , testMethodNotEq
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
        , testRedirect
        , testBracketSnap ]


expectSpecificException :: Exception e => e -> IO a -> IO ()
expectSpecificException e0 m = do
    r <- try m

    let b = either (\se -> isJust $
                           forceSameType (Just e0) (fromException se))
                   (const False)
                   r
    assertBool ("expected specific exception: " ++ show e0) b


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
mkRequest uri = Test.buildRequest $ Test.get uri Map.empty


mkRequestQuery :: ByteString -> ByteString -> [ByteString] -> IO Request
mkRequestQuery uri k v =
    Test.buildRequest $ Test.get uri $ Map.fromList [(k,v)]


mkZomgRq :: IO Request
mkZomgRq = Test.buildRequest $ Test.get "/" Map.empty

mkMethodRq :: Method -> IO Request
mkMethodRq m = Test.buildRequest $ do
                   Test.get "/" Map.empty
                   Test.setRequestType $ Test.RequestWithRawBody m ""

mkIpHeaderRq :: IO Request
mkIpHeaderRq = do
    rq <- mkZomgRq
    return $ setHeader "X-Forwarded-For" "1.2.3.4"
           $ deleteHeader "X-Forwarded-For"
           $ setHeader "X-Forwarded-For" "1.2.3.4" rq


mkRqWithBody :: IO Request
mkRqWithBody = Test.buildRequest $ Test.postRaw "/" "text/plain" "zazzle"


mkRqWithEnum :: (InputStream ByteString) -> IO Request
mkRqWithEnum str = do
    rq <- Test.buildRequest $ Test.postRaw "/" "text/plain" ""

    return $! rq { rqBody = str }

testCatchIO :: Test
testCatchIO = testCase "types/catchIO" $ do
    (_,rsp)  <- go f
    (_,rsp2) <- go g

    assertEqual "catchIO 1" (Just "bar") $ getHeader "foo" rsp
    assertEqual "catchIO 2" Nothing $ getHeader "foo" rsp2

  where
    f :: Snap ()
    f = (mask $ \restore -> restore $ throwIO $ NoHandlerException "")
            `catch` h

    g :: Snap ()
    g = return () `catch` h

    h :: SomeException -> Snap ()
    h e = e `seq` modifyResponse $ addHeader "foo" "bar"

go :: Snap a -> IO (Request,Response)
go m = do
    zomgRq <- mkZomgRq
    runSnap m dummy (const (return ())) zomgRq
  where
    dummy !x = return $! (show x `using` rdeepseq) `seq` ()

goMeth :: Method -> Snap a -> IO (Request,Response)
goMeth m s = do
    methRq <- mkMethodRq m
    runSnap s dummy (const (return ())) methRq
  where
    dummy !x = return $! (show x `using` rdeepseq) `seq` ()

goIP :: Snap a -> IO (Request,Response)
goIP m = do
    rq <- mkIpHeaderRq
    runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()

goPath :: ByteString -> Snap a -> IO (Request,Response)
goPath s m = do
    rq <- mkRequest s
    runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()


goPathQuery :: ByteString
            -> ByteString
            -> [ByteString]
            -> Snap a
            -> IO (Request,Response)
goPathQuery s k v m = do
    rq <- mkRequestQuery s k v
    runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()


goBody :: Snap a -> IO (Request,Response)
goBody m = do
    rq <- mkRqWithBody
    runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()


goEnum :: InputStream ByteString
       -> Snap b
       -> IO (Request,Response)
goEnum enum m = do
    rq <- mkRqWithEnum enum
    runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()


testFail :: Test
testFail = testCase "failure" $ expect404 (go pass)


setFoo :: ByteString -> Snap ()
setFoo s = do
    modifyResponse (addHeader "Foo" s)
    fmap id $ pure ()
    pure id <*> (liftIO $ return ())


testAlternative :: Test
testAlternative = testCase "types/alternative" $ do
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
testEarlyTermination = testCase "types/earlyTermination" $ do
    (_,resp) <- go (finishWith sampleResponse >>= \_ -> setFoo "Bar")
    assertEqual "foo" (Just ["Quux"]) $ getHeaders "Foo" resp


testEscapeHttp :: Test
testEscapeHttp = testCase "types/escapeHttp" $ flip catch catchEscape $ do
    (_, _) <- go (escapeHttp escaper)
    assertFailure "HTTP escape was ignored"
  where
    escaper _ _ _ = liftIO $ assert True
    tickle _      = return ()

    catchEscape (ex :: EscapeSnap) =
        case ex of
          EscapeHttp e -> do
              input  <- Streams.nullInput
              output <- Streams.nullOutput
              e tickle input output
          _ -> assertFailure "got TerminateConnection"


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False


testBracketSnap :: Test
testBracketSnap = testCase "types/bracketSnap" $ do
    rq <- mkZomgRq

    ref <- newIORef 0

    expectSpecificException (NoHandlerException "") $
        evalSnap (act ref) (const $ return ()) (const $ return ()) rq

    y <- readIORef ref
    assertEqual "bracketSnap/after1" (1::Int) y

    expectSpecificException (ErrorCall "no value") $
        evalSnap (act ref <|> finishWith emptyResponse)
                 (const $ return ())
                 (const $ return ())
                 rq

    y' <- readIORef ref
    assertEqual "bracketSnap/after" 2 y'


    expectSpecificException (ErrorCall "foo") $
        evalSnap (act2 ref)
                 (const $ return ())
                 (const $ return ())
                 rq

    y'' <- readIORef ref
    assertEqual "bracketSnap/after" 3 y''

  where
    act ref = bracketSnap (liftIO $ readIORef ref)
                          (\z -> liftIO $ writeIORef ref $! z+1)
                          (\z -> z `seq` mzero)

    act2 ref = bracketSnap (liftIO $ readIORef ref)
                           (\z -> liftIO $ writeIORef ref $! z+1)
                           (\z -> z `seq` liftIO $ throwIO $ ErrorCall "foo")


testCatchFinishWith :: Test
testCatchFinishWith = testCase "types/catchFinishWith" $ do
    rq <- mkZomgRq
    x <- evalSnap (catchFinishWith $ finishWith emptyResponse)
                  (const $ return ())
                  (const $ return ())
                  rq
    assertBool "catchFinishWith" $ isLeft x
    y <- evalSnap (catchFinishWith $ return ())
                  (const $ return ())
                  (const $ return ())
                  rq
    assertBool "catchFinishWith" $ isRight y


testRqBody :: Test
testRqBody = testCase "types/requestBodies" $ do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    _ <- goBody $ f mvar1 mvar2

    v1 <- takeMVar mvar1
    v2 <- takeMVar mvar2

    assertEqual "rq body" "zazzle" v1
    assertEqual "rq body 2" "" v2

    (_,rsp) <- goBody g
    bd      <- getBody rsp

    assertEqual "detached rq body" "zazzle" bd


  where
    f mvar1 mvar2 = do
        readRequestBody 100000 >>= liftIO . putMVar mvar1
        readRequestBody 100000 >>= liftIO . putMVar mvar2

    g = transformRequestBody (return . id)


testRqBodyTooLong :: Test
testRqBodyTooLong = testCase "types/requestBodyTooLong" $ do
    expectExceptionH $ goBody $ f 2
    (_, rsp) <- goBody $ f 200000
    bd       <- getBody rsp

    assertEqual "detached rq body" "zazzle" bd


  where
    f sz = readRequestBody sz >>= writeLBS


testRqBodyException :: Test
testRqBodyException = testCase "types/requestBodyException" $ do
    str <- Streams.fromList listData
    (req,resp) <- goEnum str hndlr
    bd         <- getBody resp

    b' <- Streams.toList $ rqBody req
    assertEqual "request body was consumed" [] b'
    assertEqual "response body was produced" "OK" bd

  where
    listData = ["the", "quick", "brown", "fox"]

    h0 = runRequestBody $ \str -> do
             !_ <- Streams.read str
             throwIO $ ErrorCall "foo"

    hndlr = h0 `catch` \(_::SomeException) -> writeBS "OK"


testRqBodyTermination :: Test
testRqBodyTermination = testCase "types/requestBodyTermination" $ do
    str <- Streams.fromList ["the", "quick", "brown", "fox"]
    expectExceptionH $ goEnum str h0

  where
    h0 = (runRequestBody $ \str -> do
              !_ <- Streams.read str
              throwIO $ TerminateConnection
                      $ SomeException $ ErrorCall "foo")
           `catch` tc

    tc (ex :: EscapeSnap) = case ex of
                              TerminateConnection e -> terminateConnection e
                              _                     -> throwIO ex


testTrivials :: Test
testTrivials = testCase "types/trivials" $ do
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
    coverShowInstance b
    coverShowInstance $ NoHandlerException ""
    coverShowInstance GET
    coverReadInstance GET
    coverEqInstance GET
    coverEqInstance $ NoHandlerException ""
    coverOrdInstance GET

    Prelude.map (\(x,y) -> (x,show y)) (IM.toList statusReasonMap)
            `deepseq` return ()

    let cookie = Cookie "" "" Nothing Nothing Nothing False False
    coverEqInstance cookie
    coverShowInstance cookie

    assertEqual "rq secure" True $ rqIsSecure rq
    assertEqual "rsp status" 333 $ rspStatus rsp


testMethod :: Test
testMethod = testCase "types/method" $ do
   expect404 $ go (method POST $ return ())
   expectNo404 $ go (method GET $ return ())

testMethods :: Test
testMethods = testCase "types/methods" $ do
   expect404 $ go (methods [POST,PUT,PATCH,Method "MOVE"] $ return ())
   expectNo404 $ go (methods [GET] $ return ())
   expectNo404 $ go (methods [POST,GET] $ return ())
   expectNo404 $ go (methods [PUT,GET] $ return ())
   expectNo404 $ go (methods [GET,PUT,DELETE] $ return ())
   expectNo404 $ go (methods [GET,PUT,DELETE,PATCH] $ return ())
   expectNo404 $ go (methods [GET,Method "COPY"] $ return ())
   expect404 $ goMeth PATCH (methods [POST,PUT,GET,Method "FOO"] $ return ())
   expect404 $ goMeth (Method "Baz")
     (methods [GET,POST,Method "Foo"] $ return ())
   expectNo404 $ goMeth (Method "Baz")
     (method (Method "Baz") $ return ())
   expectNo404 $ goMeth (Method "Foo")
     (methods [Method "Baz",PATCH,GET,Method "Foo"] $ return ())

   expectNo404 $ goMeth GET (method (Method "GET") $ return ())
   expectNo404 $ goMeth (Method "GET") (method GET $ return ())


methodGen :: Int -> Gen Method
methodGen n = variant n $ oneof
              [ elements [ GET, HEAD, POST, PUT, DELETE
                         , TRACE, OPTIONS, CONNECT, PATCH ]
              , Method <$> arbitrary
              ]

testMethodEq :: Test
testMethodEq = testProperty "types/Method/eq" $ prop
  where
    prop n = do
      m <- methodGen n
      return $ m == m && toMeth m == m
    toMeth GET     = Method "GET"
    toMeth HEAD    = Method "HEAD"
    toMeth POST    = Method "POST"
    toMeth PUT     = Method "PUT"
    toMeth DELETE  = Method "DELETE"
    toMeth TRACE   = Method "TRACE"
    toMeth OPTIONS = Method "OPTIONS"
    toMeth CONNECT = Method "CONNECT"
    toMeth PATCH   = Method "PATCH"
    toMeth (Method a) = Method a


testMethodNotEq :: Test
testMethodNotEq = testProperty "types/Method/noteq" $ prop
  where
    prop n = do
      m <- methodGen n
      m' <- methodGen (n + 1)
      return $ (m /= m') == not (m == m')


testDir :: Test
testDir = testCase "types/dir" $ do
   expect404 $ goPath "foo/bar" (dir "zzz" $ return ())
   expectNo404 $ goPath "foo/bar" (dir "foo" $ return ())
   expect404 $ goPath "fooz/bar" (dir "foo" $ return ())
   expectNo404 $ goPath "foo/bar" (path "foo/bar" $ return ())
   expect404 $ goPath "foo/bar/z" (path "foo/bar" $ return ())
   expectNo404 $ goPath "" (ifTop $ return ())
   expect404 $ goPath "a" (ifTop $ return ())


testParam :: Test
testParam = testCase "types/getParam" $ do
    expect404 $ goPath "/foo" f
    expectNo404 $ goPathQuery "/foo" "param" ["foo"] f
    expectNo404 $ goPathQuery "/foo" "param" ["foo"] fQ
    expect404 $ goPathQuery "/foo" "param" ["foo"] fP

  where
    p gp = do
        mp <- gp "param"
        maybe pass
              (\s -> if s == "foo" then return () else pass)
              mp

    f  = p getParam
    fQ = p getQueryParam
    fP = p getPostParam


getBody :: Response -> IO L.ByteString
getBody r = liftM (L.fromChunks . (:[])) $ Test.getResponseBody r


testWrites :: Test
testWrites = testCase "types/writes" $ do
    (_,r) <- go h
    b <- getBody r
    assertEqual "output functions" "Foo1Foo2Foo3" b
  where
    h :: Snap ()
    h = do
        addToOutput f
        writeBS "Foo2"
        writeLBS "Foo3"

    f str = do
        Streams.write (Just $ fromByteString "Foo1") str
        return str

testURLEncode1 :: Test
testURLEncode1 = testCase "types/urlEncoding1" $ do
    let b = urlEncode "the quick brown fox~#"
    assertEqual "url encoding 1" "the+quick+brown+fox%7e%23" b
    assertEqual "fail" Nothing $ urlDecode "%"


testURLEncode2 :: Test
testURLEncode2 = testProperty "types/urlEncoding2" prop
  where
    prop s = (urlDecode $ urlEncode s) == Just s


testDir2 :: Test
testDir2 = testCase "types/dir2" $ do
    (_,resp) <- goPath "foo/bar" f
    b <- getBody resp
    assertEqual "context path" "/foo/bar/" b

  where
    f = dir "foo" $ dir "bar" $ do
            p <- liftM rqContextPath getRequest
            addToOutput $ \s -> do
                Streams.write (Just $ fromByteString p) s
                return s


testIpHeaderFilter :: Test
testIpHeaderFilter = testCase "types/ipHeaderFilter" $ do
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
testMZero404 = testCase "types/mzero404" $ do
    (_,r) <- go mzero
    b <- getBody r
    assertBool "mzero 404" ("<!DOCTYPE html" `L.isPrefixOf` b)


testEvalSnap :: Test
testEvalSnap = testCase "types/evalSnap-exception" $ do
    rq <- mkZomgRq
    expectExceptionH (evalSnap f
                             (const $ return ())
                             (const $ return ())
                             rq >> return ())
  where
    f = do
        logError "zzz"
        v <- withResponse (return . rspHttpVersion)
        liftIO $ assertEqual "evalSnap rsp version" (1,1) v
        finishWith emptyResponse


testLocalRequest :: Test
testLocalRequest = testCase "types/localRequest" $ do
    rq1 <- mkZomgRq
    rq2 <- mkRequest "zzz/zz/z"

    let h = localRequest (const rq2) mzero

    (rq',_) <- go (h <|> return ())

    let u1 = rqURI rq1
    let u2 = rqURI rq'

    assertEqual "localRequest backtrack" u1 u2



testRedirect :: Test
testRedirect = testCase "types/redirect" $ do
    (_,rsp)  <- go (redirect "/foo/bar")

    b <- getBody rsp
    assertEqual "no response body" "" b
    assertEqual "response content length" (Just 0) $ rspContentLength rsp
    assertEqual "redirect path" (Just "/foo/bar") $ getHeader "Location" rsp
    assertEqual "redirect status" 302 $ rspStatus rsp
    assertEqual "status description" "Found" $ rspStatusReason rsp


    (_,rsp2)  <- go (redirect' "/bar/foo" 307)

    assertEqual "redirect path" (Just "/bar/foo") $ getHeader "Location" rsp2
    assertEqual "redirect status" 307 $ rspStatus rsp2
    assertEqual "status description" "Temporary Redirect" $ rspStatusReason rsp2
