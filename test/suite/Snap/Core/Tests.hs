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
import           Control.Exception (ErrorCall(..), SomeException, throwIO)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans (liftIO)
import           Control.Parallel.Strategies
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntMap as IM
import           Data.IORef
import           Data.Maybe (isJust)
import           Data.Monoid
import           Data.Text ()
import           Data.Text.Lazy ()
import qualified Data.Map as Map
import           Prelude hiding (catch)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)
import           Test.QuickCheck(oneof, variant, elements, Gen, arbitrary)

import           Snap.Internal.Exceptions
import           Snap.Internal.Http.Types
import           Snap.Internal.Parsing
import           Snap.Internal.Types
import           Snap.Iteratee
import qualified Snap.Iteratee as I
import           Snap.Test.Common
import qualified Snap.Types.Headers as H


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
mkRequest uri = do
    enum <- newIORef $ SomeEnumerator returnI

    return $! Request "foo" 80 "127.0.0.1" 999 "foo" 1000 "foo" False H.empty
                      enum Nothing GET (1,1) [] uri "/"
                      (S.concat ["/",uri]) "" Map.empty Map.empty Map.empty


mkRequestQuery :: ByteString -> ByteString -> [ByteString] -> IO Request
mkRequestQuery uri k v = do
    enum <- newIORef $ SomeEnumerator returnI

    let mp = Map.fromList [(k,v)]
    let q  = S.concat [k,"=", S.concat v]

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False H.empty
                     enum Nothing GET (1,1) [] uri "/"
                     (S.concat ["/",uri,"?",q]) q mp mp Map.empty


mkZomgRq :: IO Request
mkZomgRq = do
    enum <- newIORef $ SomeEnumerator returnI

    return $ Request "foo" 80 "127.0.0.1" 999 "foo" 1000 "foo" False H.empty
                     enum Nothing GET (1,1) [] "/" "/" "/" ""
                     Map.empty Map.empty Map.empty

mkMethodRq :: Method -> IO Request
mkMethodRq m = do
    enum <- newIORef $ SomeEnumerator returnI

    return $ Request "foo" 80 "127.0.0.1" 999 "foo" 1000 "foo" False H.empty
                     enum Nothing m (1,1) [] "/" "/" "/" ""
                     Map.empty Map.empty Map.empty

mkIpHeaderRq :: IO Request
mkIpHeaderRq = do
    rq <- mkZomgRq
    return $ setHeader "X-Forwarded-For" "1.2.3.4"
           $ deleteHeader "X-Forwarded-For"
           $ setHeader "X-Forwarded-For" "1.2.3.4" rq


mkRqWithBody :: IO Request
mkRqWithBody = mkRqWithEnum (enumBS "zazzle" >==> enumEOF)


mkRqWithEnum :: (forall a . Enumerator ByteString IO a) -> IO Request
mkRqWithEnum e = do
    enum <- newIORef $ SomeEnumerator e
    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False H.empty
                 enum Nothing GET (1,1) [] "/" "/" "/" ""
                 Map.empty Map.empty Map.empty

testCatchIO :: Test
testCatchIO = testCase "types/catchIO" $ do
    (_,rsp)  <- go f
    (_,rsp2) <- go g

    assertEqual "catchIO 1" (Just "bar") $ getHeader "foo" rsp
    assertEqual "catchIO 2" Nothing $ getHeader "foo" rsp2

  where
    f :: Snap ()
    f = (block $ unblock $ throw $ NoHandlerException "") `catch` h

    g :: Snap ()
    g = return () `catch` h

    h :: SomeException -> Snap ()
    h e = e `seq` modifyResponse $ addHeader "foo" "bar"

go :: Snap a -> IO (Request,Response)
go m = do
    zomgRq <- mkZomgRq
    run_ $ runSnap m dummy (const (return ())) zomgRq
  where
    dummy !x = return $! (show x `using` rdeepseq) `seq` ()

goMeth :: Method -> Snap a -> IO (Request,Response)
goMeth m s = do
    methRq <- mkMethodRq m
    run_ $ runSnap s dummy (const (return ())) methRq
  where
    dummy !x = return $! (show x `using` rdeepseq) `seq` ()

goIP :: Snap a -> IO (Request,Response)
goIP m = do
    rq <- mkIpHeaderRq
    run_ $ runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()

goPath :: ByteString -> Snap a -> IO (Request,Response)
goPath s m = do
    rq <- mkRequest s
    run_ $ runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()


goPathQuery :: ByteString
            -> ByteString
            -> [ByteString]
            -> Snap a
            -> IO (Request,Response)
goPathQuery s k v m = do
    rq <- mkRequestQuery s k v
    run_ $ runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()


goBody :: Snap a -> IO (Request,Response)
goBody m = do
    rq <- mkRqWithBody
    run_ $ runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()


goEnum :: (forall a . Enumerator ByteString IO a)
       -> Snap b
       -> IO (Request,Response)
goEnum enum m = do
    rq <- mkRqWithEnum enum
    run_ $ runSnap m dummy (const (return ())) rq
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
    escaper _ _ = liftIO $ assert True
    tickle _    = return ()

    catchEscape (EscapeHttpException iter) = run_ $ iter tickle (return ())


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
        run_ $ evalSnap (act ref) (const $ return ()) (const $ return ()) rq

    y <- readIORef ref
    assertEqual "bracketSnap/after1" (1::Int) y

    expectSpecificException (ErrorCall "no value") $
        run_ $ evalSnap (act ref <|> finishWith emptyResponse)
                        (const $ return ())
                        (const $ return ())
                        rq

    y' <- readIORef ref
    assertEqual "bracketSnap/after" 2 y'


    expectSpecificException (ErrorCall "foo") $
        run_ $ evalSnap (act2 ref)
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
    x <- run_ $ evalSnap (catchFinishWith $ finishWith emptyResponse)
                         (const $ return ())
                         (const $ return ())
                         rq
    assertBool "catchFinishWith" $ isLeft x
    y <- run_ $ evalSnap (catchFinishWith $ return ())
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
        getRequestBody >>= liftIO . putMVar mvar1
        getRequestBody >>= liftIO . putMVar mvar2

    g = transformRequestBody returnI


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
    (req,resp) <- goEnum (enumList 1 ["the", "quick", "brown", "fox"]) hndlr
    bd      <- getBody resp

    (SomeEnumerator e) <- readIORef $ rqBody req
    b' <- liftM (S.concat) $ run_ $ e $$ consume
    assertEqual "request body was consumed" "" b'
    assertEqual "response body was produced" "OK" bd

  where
    h0 = runRequestBody $ do
             _ <- I.head
             throw $ ErrorCall "foo"

    hndlr = h0 `catch` \(_::SomeException) -> writeBS "OK"


testRqBodyTermination :: Test
testRqBodyTermination =
    testCase "types/requestBodyTermination" $
    expectExceptionH $
    goEnum (enumList 1 ["the", "quick", "brown", "fox"]) hndlr

  where
    h0 = runRequestBody $ do
             _ <- I.head
             terminateConnection $ ErrorCall "foo"

    hndlr = h0 `catch` \(_::SomeException) -> writeBS "OK"



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
getBody r = do
    let benum = rspBodyToEnum $ rspBody r
    liftM (toLazyByteString . mconcat) (runIteratee consume >>= run_ . benum)


testWrites :: Test
testWrites = testCase "types/writes" $ do
    (_,r) <- go h
    b <- getBody r
    assertEqual "output functions" "Foo1Foo2Foo3" b
  where
    h :: Snap ()
    h = do
        addToOutput $ enumBuilder $ fromByteString "Foo1"
        writeBS "Foo2"
        writeLBS "Foo3"


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
            addToOutput $ enumBuilder $ fromByteString p


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
    expectExceptionH (run_ $ evalSnap f
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
