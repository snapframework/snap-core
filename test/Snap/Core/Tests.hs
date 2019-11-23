{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- Deprecations warnings turned off due to ErrorT deprecation
--
-- It's a pain that this setting is per-module, because we might end up hiding
-- deprecation warnings that we want to see. TODO: move any code that emits
-- these warnings to isolated modules.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Core.Tests ( tests ) where
------------------------------------------------------------------------------
import           Control.Applicative                  (Alternative ((<|>)), Applicative ((<*>), pure), (<$>))
import           Control.Concurrent.MVar              (newEmptyMVar, putMVar, takeMVar)
import           Control.DeepSeq                      (deepseq)
import           Control.Exception.Lifted             (ErrorCall (..), Exception, SomeException (..), catch, fromException, mask, throwIO, try)
import           Control.Monad                        (Functor (fmap), Monad ((>>), (>>=), return), MonadPlus (mplus, mzero), forM_, liftM, void)
import           Control.Monad.Base                   (MonadBase (liftBase))
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Error            (ErrorT (runErrorT))
import           Data.ByteString.Builder              (byteString)
#if MIN_VERSION_transformers(0,4,0)
import           Control.Monad.Trans.Except           (runExceptT)
#endif
import           Control.Monad.Trans.List             (ListT (runListT))
import           Control.Monad.Trans.Reader           (ReaderT (runReaderT))
import qualified Control.Monad.Trans.RWS.Lazy         as LRWS (RWST (runRWST))
import           Control.Monad.Trans.RWS.Strict       (RWST (runRWST))
import qualified Control.Monad.Trans.State.Lazy       as LState (evalStateT)
import           Control.Monad.Trans.State.Strict     (evalStateT)
import qualified Control.Monad.Trans.Writer.Lazy      as LWriter (WriterT (runWriterT))
import           Control.Monad.Trans.Writer.Strict    (WriterT (runWriterT))
import           Control.Parallel.Strategies          (rdeepseq, using)
import           Data.ByteString.Char8                (ByteString)
import qualified Data.ByteString.Char8                as S (length, pack, replicate)
import qualified Data.ByteString.Lazy.Char8           as L (ByteString, fromChunks, isPrefixOf)
import qualified Data.IntMap                          as IM (toList)
import           Data.IORef                           (newIORef, readIORef, writeIORef)
import qualified Data.Map                             as Map (empty, fromList)
import           Data.Maybe                           (isJust)
import           Data.Monoid                          (mappend)
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as T (encodeUtf8)
import           Data.Text.Lazy                       ()
import           Prelude                              (Bool (..), Either (..), Enum (..), Eq (..), IO, Int, Maybe (Just, Nothing), Num (..), Show (..), String, const, either, fail, flip, id, map, maybe, not, seq, undefined, ($), ($!), (&&), (++), (.))
import           Snap.Internal.Core                   (EscapeSnap (..), MonadSnap (..), NoHandlerException (NoHandlerException), Snap, addToOutput, bracketSnap, catchFinishWith, dir, escapeHttp, evalSnap, finishWith, getParam, getParams, getPostParam, getPostParams, getQueryParam, getQueryParams, getRequest, getResponse, getsResponse, ifTop, ipHeaderFilter, localRequest, logError, method, methods, modifyResponse, pass, path, pathArg, putRequest, putResponse, readRequestBody, redirect, redirect', runRequestBody, runSnap, setTimeout, terminateConnection, transformRequestBody, updateContextPath, withRequest, withResponse, writeBS, writeLBS, writeLazyText, writeText)
import           Snap.Internal.Http.Types             (Cookie (Cookie), Method (..), Request (rqBody, rqClientAddr, rqContextPath, rqIsSecure, rqURI), Response (rspContentLength, rspStatus, rspStatusReason, rspTransformingRqBody), addHeader, deleteHeader, emptyResponse, getHeader, rqRemoteAddr, setContentLength, setHeader, setResponseCode, setResponseStatus, statusReasonMap)
import           Snap.Internal.Parsing                (urlDecode, urlEncode)
import qualified Snap.Test                            as Test (RequestType (RequestWithRawBody), buildRequest, evalHandler, get, getResponseBody, postRaw, runHandler, setRequestType)
import           Snap.Test.Common                     (coverEqInstance, coverOrdInstance, coverReadInstance, coverShowInstance, coverTypeableInstance, expectExceptionH, forceSameType, waitabit)
import           System.IO.Streams                    (InputStream)
import qualified System.IO.Streams                    as Streams (fromList, makeInputStream, nullInput, nullOutput, read, throwIfTooSlow, toList, write)
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertable (assert), assertBool, assertEqual, assertFailure)
import           Test.QuickCheck                      (Gen, arbitrary, elements, oneof, variant)
------------------------------------------------------------------------------


tests :: [Test]
tests = [ testFail
        , testAlternative
        , testEarlyTermination
        , testEscapeHttp
        , testCatchFinishWith
        , testRqBody
        , testRqBodyException
        , testRqBodyTermination
        , testRqBodyTooLong
        , testRqBodyTooSlow
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
        , testBracketSnap
        , testCoverInstances
        , testPathArgs
        , testStateTAndExceptions
        , test304Fixup
        , testChunkedFixup
        ]


------------------------------------------------------------------------------
expectSpecificException :: Exception e => e -> IO a -> IO ()
expectSpecificException e0 m = do
    r <- try m

    let b = either (\se -> isJust $
                           forceSameType (Just e0) (fromException se))
                   (const False)
                   r
    assertBool ("expected specific exception: " ++ show e0) b


------------------------------------------------------------------------------
expect404 :: IO (Request,Response) -> IO ()
expect404 m = do
    (_,r) <- m
    assertBool "expected 404" (rspStatus r == 404)


------------------------------------------------------------------------------
expectNo404 :: IO (Request,Response) -> IO ()
expectNo404 m = do
    (_,r) <- m
    assertBool ("expected 200, got " ++ show (rspStatus r))
               (rspStatus r /= 404)


------------------------------------------------------------------------------
mkRequest :: ByteString -> IO Request
mkRequest uri = Test.buildRequest $ Test.get uri Map.empty


------------------------------------------------------------------------------
mkRequestQuery :: ByteString -> ByteString -> [ByteString] -> IO Request
mkRequestQuery uri k v =
    Test.buildRequest $ Test.get uri $ Map.fromList [(k,v)]


------------------------------------------------------------------------------
mkZomgRq :: IO Request
mkZomgRq = Test.buildRequest $ Test.get "/" Map.empty


------------------------------------------------------------------------------
mkMethodRq :: Method -> IO Request
mkMethodRq m = Test.buildRequest $ do
                   Test.get "/" Map.empty
                   Test.setRequestType $ Test.RequestWithRawBody m ""


------------------------------------------------------------------------------
mkIpHeaderRq :: IO Request
mkIpHeaderRq = do
    rq <- mkZomgRq
    return $ setHeader "X-Forwarded-For" "1.2.3.4"
           $ deleteHeader "X-Forwarded-For"
           $ setHeader "X-Forwarded-For" "1.2.3.4" rq


------------------------------------------------------------------------------
mkRqWithBody :: IO Request
mkRqWithBody = Test.buildRequest $ Test.postRaw "/" "text/plain" "zazzle"


------------------------------------------------------------------------------
mkRqWithEnum :: (InputStream ByteString) -> IO Request
mkRqWithEnum str = do
    rq <- Test.buildRequest $ Test.postRaw "/" "text/plain" ""

    return $! rq { rqBody = str }


------------------------------------------------------------------------------
testCatchIO :: Test
testCatchIO = testCase "core/catchIO" $ do
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


------------------------------------------------------------------------------
go :: Snap a -> IO (Request,Response)
go m = do
    zomgRq <- mkZomgRq
    runSnap m dummy timeoutModifier zomgRq
  where
    dummy !x = return $! (show x `using` rdeepseq) `seq` ()
    timeoutModifier !f = return $! f 0 `seq` ()


------------------------------------------------------------------------------
goMeth :: Method -> Snap a -> IO (Request,Response)
goMeth m s = do
    methRq <- mkMethodRq m
    runSnap s dummy timeoutModifier methRq
  where
    dummy !x = return $! (show x `using` rdeepseq) `seq` ()
    timeoutModifier !f = return $! f 0 `seq` ()


------------------------------------------------------------------------------
goIP :: Snap a -> IO (Request,Response)
goIP m = do
    rq <- mkIpHeaderRq
    runSnap m dummy timeoutModifier rq
  where
    timeoutModifier !f = return $! f 0 `seq` ()
    dummy = const $ return ()


------------------------------------------------------------------------------
goPath :: ByteString -> Snap a -> IO (Request,Response)
goPath s m = do
    rq <- mkRequest s
    runSnap m dummy timeoutModifier rq
  where
    timeoutModifier !f = return $! f 0 `seq` ()
    dummy = const $ return ()


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
goBody :: Snap a -> IO (Request,Response)
goBody m = do
    rq <- mkRqWithBody
    runSnap m dummy (const (return ())) rq
  where
    dummy = const $ return ()


------------------------------------------------------------------------------
goEnum :: InputStream ByteString
       -> Snap b
       -> IO (Request,Response)
goEnum enum m = do
    rq <- mkRqWithEnum enum
    runSnap m logerr tout rq


------------------------------------------------------------------------------
testFail :: Test
testFail = testCase "failure" $ expect404 (go pass)


------------------------------------------------------------------------------
setFoo :: ByteString -> Snap ()
setFoo s = do
    modifyResponse (addHeader "Foo" s)
    fmap id $ pure ()
    pure id <*> (liftIO $ return ())
    !x <- liftBase $! return $! ()
    return $! x


------------------------------------------------------------------------------
testAlternative :: Test
testAlternative = testCase "core/alternative" $ do
    (_,resp) <- go (pass <|> setFoo "Bar")
    assertEqual "foo present" (Just "Bar") $ getHeader "Foo" resp

    (_,resp2) <- go (fail "fail should not error"
                       <|> fail2
                       <|> setFoo "Bar"
                       <|> setFoo "Baz")
    assertEqual "alternative chooses correct branch"
                (Just "Bar") $ getHeader "Foo" resp2

  where
    fail2 :: Snap ()
    fail2 = pass >>= \_ -> return ()


------------------------------------------------------------------------------
sampleResponse :: Response
sampleResponse = addHeader "Foo" "Quux" $ emptyResponse


------------------------------------------------------------------------------
testEarlyTermination :: Test
testEarlyTermination = testCase "core/earlyTermination" $ do
    (_,resp) <- go (finishWith sampleResponse >>= \_ -> setFoo "Bar")
    assertEqual "foo" (Just "Quux") $ getHeader "Foo" resp


------------------------------------------------------------------------------
testStateTAndExceptions :: Test
testStateTAndExceptions = testCase "core/stateT_exceptions" $ do
    Test.evalHandler (return ()) h1 >>= assertEqual "h1" True
    Test.evalHandler (return ()) h2 >>= assertEqual "h2" True
    Test.evalHandler (return ()) h3 >>= assertEqual "h3" True
    Test.evalHandler (return ()) h4 >>= assertEqual "h4" True
    Test.evalHandler (return ()) h5 >>= assertEqual "h5" True

  where
    useState = do rq <- getRequest
                  return (rqURI rq == "/")

    h1 = do let m = ((try mzero) :: Snap (Either SomeException Int))
            (m >> return False) <|> useState

    h2 = do catchFinishWith (getResponse >>= finishWith)
            useState

    h3 = do catchFinishWith (return ())
            useState

    h4 = do (void (catchFinishWith mzero)) <|> return ()
            useState

    h5 = do let m1 = (void (getResponse >>= finishWith)) `mplus` return ()
            void (catchFinishWith m1)
            useState


------------------------------------------------------------------------------
testEscapeHttp :: Test
testEscapeHttp = testCase "core/escapeHttp" $ flip catch catchEscape $ do
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


------------------------------------------------------------------------------
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False


------------------------------------------------------------------------------
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False


------------------------------------------------------------------------------
logerr :: ByteString -> IO ()
logerr !_ = return $! ()


------------------------------------------------------------------------------
tout :: (Int -> Int) -> IO ()
tout !f = let !_ = f 2 in return $! ()


------------------------------------------------------------------------------
testBracketSnap :: Test
testBracketSnap = testCase "core/bracketSnap" $ do
    rq <- mkZomgRq

    ref <- newIORef 0

    expectSpecificException (NoHandlerException "") $
        evalSnap (act ref) logerr tout rq

    y <- readIORef ref
    assertEqual "bracketSnap/after1" (1::Int) y

    expectSpecificException (ErrorCall "no value") $
        evalSnap (act ref <|> finishWith emptyResponse)
                 logerr
                 tout
                 rq

    y' <- readIORef ref
    assertEqual "bracketSnap/after" 2 y'


    expectSpecificException (ErrorCall "foo") $
        evalSnap (act2 ref)
                 logerr
                 tout
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


------------------------------------------------------------------------------
testCatchFinishWith :: Test
testCatchFinishWith = testCase "core/catchFinishWith" $ do
    rq <- mkZomgRq
    x <- evalSnap (catchFinishWith $ finishWith emptyResponse)
                  logerr tout rq
    assertBool "catchFinishWith" $ isLeft x
    let (Left resp) = x
    assertEqual "code" 200 (rspStatus resp)

    y <- evalSnap (catchFinishWith $ return $! ())
                  logerr tout rq
    assertBool "catchFinishWith" $ isRight y
    let (Right val) = y
    assertEqual "val" () val

    expectExceptionH $ evalSnap (catchFinishWith pass) logerr tout rq


------------------------------------------------------------------------------
testRqBody :: Test
testRqBody = testCase "core/requestBodies" $ do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    _ <- goBody $ f mvar1 mvar2

    v1 <- takeMVar mvar1
    v2 <- takeMVar mvar2

    assertEqual "rq body" "zazzle" v1
    assertEqual "rq body 2" "" v2

    (_,rsp) <- goBody (g >> putResponse emptyResponse)
    bd      <- getBody rsp

    assertEqual "detached rq body" "zazzle" bd
    assertBool "transforming" (rspTransformingRqBody rsp)

  where
    f mvar1 mvar2 = do
        readRequestBody 100000 >>= liftIO . putMVar mvar1
        readRequestBody 100000 >>= liftIO . putMVar mvar2

    g = transformRequestBody (return . id)


------------------------------------------------------------------------------
testRqBodyTooLong :: Test
testRqBodyTooLong = testCase "core/requestBodyTooLong" $ do
    expectExceptionH $ goBody $ f 2
    (_, rsp) <- goBody $ f 200000
    bd       <- getBody rsp
    assertEqual "detached rq body" "zazzle" bd
  where
    f sz = readRequestBody sz >>= writeLBS


------------------------------------------------------------------------------
testRqBodyException :: Test
testRqBodyException = testCase "core/requestBodyException" $ do
    str <- Streams.fromList listData
    (req,resp) <- goEnum str hndlr
    bd         <- getBody resp
    b'         <- Streams.toList $ rqBody req
    assertEqual "request body was consumed" [] b'
    assertEqual "response body was produced" "OK" bd
  where
    listData = ["the", "quick", "brown", "fox"]

    h0 = runRequestBody $ \str -> do
             !_ <- Streams.read str
             throwIO $ ErrorCall "foo"

    hndlr = h0 `catch` \(_::SomeException) -> writeBS "OK"


------------------------------------------------------------------------------
testRqBodyTooSlow :: Test
testRqBodyTooSlow = testCase "core/requestBodyTooSlow" $ do
    str <- Streams.makeInputStream strFunc >>=
           Streams.throwIfTooSlow (return ()) 100000.0 1
    expectExceptionH (goEnum str hndlr)

  where
    strFunc = waitabit >> return (Just "1")
    hndlr = runRequestBody $ \_ -> throwIO $ ErrorCall "foo"


------------------------------------------------------------------------------
testRqBodyTermination :: Test
testRqBodyTermination = testCase "core/requestBodyTermination" $ do
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


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "core/trivials" $ do
    (rq,rsp) <- go $ do
        req <- getRequest
        putRequest $ req { rqIsSecure=True }
        putResponse $ setResponseStatus 333 "333" sampleResponse
        r <- getResponse
        liftIO $ assertEqual "rsp status" 333 $ rspStatus r
        code <- getsResponse rspStatus
        liftIO $ assertEqual "rsp status 2" 333 code
        !_ <- localRequest (\x -> x {rqIsSecure=False}) $ do
            q <- getRequest
            liftIO $ assertEqual "localrq" False $ rqIsSecure q
            return ()

        !_ <- logError "foo"
        writeText "zzz"
        writeLazyText "zzz"

        let req' = updateContextPath 0 req
        let cp1 = rqContextPath req
        let cp2 = rqContextPath req'

        setTimeout 30
        !_ <- getParams
        !_ <- getPostParams
        !_ <- getQueryParams

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
    coverTypeableInstance (undefined :: Snap ())
    coverShowInstance (EscapeHttp undefined)

    -- number serialization
    forM_ [ 0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 10000000000
          , 100000000000
          ] $ \i -> do
              let s = show i
              rsp_cl <- Test.runHandler (return ()) (clHandler i)
              assertEqual ("number " ++ s)
                          (Just $ S.pack s)
                          (getHeader "content-length" rsp_cl)
  where
    clHandler i = do modifyResponse (setContentLength i)
                     writeBS (S.replicate (fromEnum i) ' ')

------------------------------------------------------------------------------
testMethod :: Test
testMethod = testCase "core/method" $ do
   expect404 $ go (method POST $ return ())
   expectNo404 $ go (method GET $ return ())


------------------------------------------------------------------------------
testMethods :: Test
testMethods = testCase "core/methods" $ do
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


------------------------------------------------------------------------------
methodGen :: Int -> Gen Method
methodGen n = variant n $ oneof
              [ elements [ GET, HEAD, POST, PUT, DELETE
                         , TRACE, OPTIONS, CONNECT, PATCH ]
              , Method <$> arbitrary
              ]


------------------------------------------------------------------------------
testMethodEq :: Test
testMethodEq = testProperty "core/Method/eq" $ prop
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


------------------------------------------------------------------------------
testMethodNotEq :: Test
testMethodNotEq = testProperty "core/Method/noteq" $ prop
  where
    prop n = do
      m <- methodGen n
      m' <- methodGen (n + 1)
      return $ (m /= m') == not (m == m')


------------------------------------------------------------------------------
testDir :: Test
testDir = testCase "core/dir" $ do
   expect404 $ goPath "foo/bar" (dir "zzz" $ return ())
   expectNo404 $ goPath "foo/bar" (dir "foo" $ return ())
   expect404 $ goPath "fooz/bar" (dir "foo" $ return ())
   expectNo404 $ goPath "foo/bar" (path "foo/bar" $ return ())
   expect404 $ goPath "foo/bar/z" (path "foo/bar" $ return ())
   expectNo404 $ goPath "" (ifTop $ return ())
   expect404 $ goPath "a" (ifTop $ return ())


------------------------------------------------------------------------------
testParam :: Test
testParam = testCase "core/getParam" $ do
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


------------------------------------------------------------------------------
getBody :: Response -> IO L.ByteString
getBody r = liftM (L.fromChunks . (:[])) $ Test.getResponseBody r


------------------------------------------------------------------------------
testWrites :: Test
testWrites = testCase "core/writes" $ do
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
        Streams.write (Just $ byteString "Foo1") str
        return str


------------------------------------------------------------------------------
testURLEncode1 :: Test
testURLEncode1 = testCase "core/urlEncoding1" $ do
    let b = urlEncode "the quick brown fox~#"
    assertEqual "url encoding 1" "the+quick+brown+fox%7e%23" b
    assertEqual "fail" Nothing $ urlDecode "%"


------------------------------------------------------------------------------
testURLEncode2 :: Test
testURLEncode2 = testProperty "core/urlEncoding2" prop
  where
    prop s = (urlDecode $ urlEncode s) == Just s


------------------------------------------------------------------------------
testDir2 :: Test
testDir2 = testCase "core/dir2" $ do
    (_,resp) <- goPath "foo/bar" f
    b <- getBody resp
    assertEqual "context path" "/foo/bar/" b

  where
    f = dir "foo" $ dir "bar" $ do
            p <- liftM rqContextPath getRequest
            addToOutput $ \s -> do
                Streams.write (Just $ byteString p) s
                return s


------------------------------------------------------------------------------
testIpHeaderFilter :: Test
testIpHeaderFilter = testCase "core/ipHeaderFilter" $ do
    (_,r) <- goIP f
    b <- getBody r
    assertEqual "ipHeaderFilter" "1.2.3.4" b


    (_,r2) <- go f'
    b2 <- getBody r2
    assertEqual "ipHeaderFilter" "127.0.0.1" b2

  where
    f = do
        ipHeaderFilter
        ip <- liftM rqClientAddr getRequest
        writeBS ip

    f' = do
        ipHeaderFilter
        ip <- liftM rqRemoteAddr getRequest
        writeBS ip


------------------------------------------------------------------------------
testMZero404 :: Test
testMZero404 = testCase "core/mzero404" $ do
    (_,r) <- go mzero
    b <- getBody r
    assertBool "mzero 404" ("<!DOCTYPE html" `L.isPrefixOf` b)


testEvalSnap :: Test
testEvalSnap = testCase "core/evalSnap-exception" $ do
    rq <- mkZomgRq
    expectExceptionH (evalSnap f logerr tout rq >> return ())
  where
    f = do
        logError "zzz"
        v <- withResponse (return . rspStatus)
        liftIO $ assertEqual "evalSnap rsp status" 200 v
        finishWith emptyResponse


------------------------------------------------------------------------------
testLocalRequest :: Test
testLocalRequest = testCase "core/localRequest" $ do
    rq1 <- mkZomgRq
    rq2 <- mkRequest "zzz/zz/z"

    let h = localRequest (const rq2) mzero

    (rq',_) <- go (h <|> return ())

    let u1 = rqURI rq1
    let u2 = rqURI rq'

    assertEqual "localRequest backtrack" u1 u2


------------------------------------------------------------------------------
testRedirect :: Test
testRedirect = testCase "core/redirect" $ do
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
    assertEqual "status description" "Temporary Redirect" $
                rspStatusReason rsp2


------------------------------------------------------------------------------
testCoverInstances :: Test
testCoverInstances = testCase "core/instances" $ do
    coverErrorT
#if MIN_VERSION_transformers(0,4,0)
    coverExceptT
#endif
    coverListT
    coverRWST
    coverLRWS
    coverReaderT
    coverStateT
    coverLStateT
    coverWriterT
    coverLWriterT

  where
    snap :: MonadSnap m => m ()
    snap = liftSnap $ writeBS "OK"

    cover :: MonadSnap m => (m () -> Snap ()) -> IO ()
    cover runFunc = do
        !_ <- Test.runHandler (return ()) (runFunc snap)
        return ()

    rwst :: RWST () () () Snap () -> Snap ()
    rwst m = void $ runRWST m () ()

    lrwst :: LRWS.RWST () () () Snap () -> Snap ()
    lrwst m = void $ LRWS.runRWST m () ()

    wt :: WriterT () Snap () -> Snap ()
    wt m = void $ runWriterT m

    lwt :: LWriter.WriterT () Snap () -> Snap ()
    lwt m = void $ LWriter.runWriterT m

    coverErrorT   = cover (\m -> do
                               (_ :: Either String ()) <- runErrorT m
                               return ())
#if MIN_VERSION_transformers(0,4,0)
    coverExceptT  = cover (\m -> do
                               (_ :: Either String ()) <- runExceptT m
                               return ())
#endif
    coverListT    = cover (void . runListT)
    coverRWST     = cover rwst
    coverLRWS     = cover lrwst
    coverReaderT  = cover (flip runReaderT ())
    coverStateT   = cover (flip evalStateT ())
    coverLStateT  = cover (flip LState.evalStateT ())
    coverWriterT  = cover wt
    coverLWriterT = cover lwt


------------------------------------------------------------------------------
testPathArgs :: Test
testPathArgs = testCase "core/pathArgs" $ do
    (_, rsp) <- goPath "%e4%b8%ad" m
    b <- getBody rsp
    assertEqual "pathargs url- and utf8-decodes" "ok" b

    Test.evalHandler (Test.get "/%zzzz" Map.empty) m2 >>= assertEqual "m2" True
    Test.evalHandler (Test.get "/z/foo" Map.empty) m3 >>= assertEqual "m3" "/z/"

  where
    m = pathArg f

    m2 = pathArg (\(_ :: Text) -> return False) <|> return True

    m3 = pathArg (\(_ :: Text) -> rqContextPath <$> getRequest)


    f x = if x == ("\x4e2d" :: Text)
            then writeBS "ok"
            else writeBS $ "not ok: " `mappend` T.encodeUtf8 x

------------------------------------------------------------------------------
test304Fixup :: Test
test304Fixup = testCase "core/304fixup" $ do
    rsp1 <- Test.runHandler (return ()) h1
    assertEqual "code1" (rspStatus rsp1) 304
    assertEqual "cl1" Nothing (rspContentLength rsp1)
    Test.getResponseBody rsp1 >>= assertEqual "body1" ""
    assertBool "date" $ getHeader "date" rsp1 /= (Just "zzz")
    assertEqual "cl-header" Nothing $ getHeader "content-length" rsp1
    assertEqual "transfer-encoding" Nothing $ getHeader "transfer-encoding" rsp1

  where
    h1 = do let s = "this should get eaten"
            modifyResponse (setResponseCode 304 .
                            setContentLength (toEnum $ S.length s) .
                            setHeader "content-length" "zzz" .
                            setHeader "transfer-encoding" "chunked" .
                            setHeader "date" "zzz")
            writeBS s


------------------------------------------------------------------------------
testChunkedFixup :: Test
testChunkedFixup = testCase "core/chunked-fixup" $ do
    rsp1 <- Test.runHandler (return ()) h1
    Test.getResponseBody rsp1 >>= assertEqual "body1" "OK"
    assertEqual "transfer-encoding" (Just "chunked")
        $ getHeader "transfer-encoding" rsp1
    assertEqual "baz" (Just "baz") $ getHeader "baz" rsp1
    assertEqual "foo" (Just "foo") $ getHeader "foo" rsp1

  where
    h1 = do modifyResponse $ setHeader "baz" "baz" .
                             setHeader "transfer-encoding" "chunked" .
                             setHeader "foo" "foo" .
                             setHeader "bar" "bar"
            writeBS "OK"
