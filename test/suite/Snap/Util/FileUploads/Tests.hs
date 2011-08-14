{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Util.FileUploads.Tests
  ( tests ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.DeepSeq
import           Control.Exception (Exception(..), SomeException(..))
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import           Data.Typeable
import           Prelude hiding (catch)
import           System.Directory
import           System.Mem
import           System.Timeout
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
------------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Debug
import           Snap.Internal.Iteratee.Debug
import           Snap.Internal.Types
import           Snap.Iteratee hiding (map)
import           Snap.Test.Common
import           Snap.Util.FileUploads


------------------------------------------------------------------------------
data TestException = TestException
  deriving (Show, Typeable)

instance Exception TestException


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testSuccess1
        , testSuccess2
        , testPerPartPolicyViolation1
        , testPerPartPolicyViolation2
        , testFormInputPolicyViolation
        , testTooManyHeaders
        , testNoBoundary
        , testNoMixedBoundary
        , testWrongContentType
        , testSlowEnumerator
        , testTrivials
        , testDisconnectionCleanup
        ]


------------------------------------------------------------------------------
testSuccess1 :: Test
testSuccess1 = testCase "fileUploads/success1" $
               harness tmpdir hndl mixedTestBody

  where
    tmpdir = "tempdir1"

    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'

    hndl' xs = do
        fileMap <- foldM f Map.empty xs

        p1 <- getParam "field1"
        p2 <- getParam "field2"

        liftIO $ do
            assertEqual "file1 contents"
                        (Just ("text/plain", file1Contents))
                        (Map.lookup "file1.txt" fileMap)

            assertEqual "file2 contents"
                        (Just ("image/gif", file2Contents))
                        (Map.lookup "file2.gif" fileMap)

            assertEqual "field1 contents"
                        (Just formContents1)
                        p1

            assertEqual "field2 contents"
                        (Just formContents2)
                        p2



    f mp (partInfo, e) =
        either throw
               (\fp -> do
                    x <- liftIO $ S.readFile fp
                    let fn = fromJust $ partFileName partInfo
                    let ct = partContentType partInfo
                    return $ Map.insert fn (ct,x) mp)
               e


------------------------------------------------------------------------------
testSuccess2 :: Test
testSuccess2 = testCase "fileUploads/success2" $
               harness tmpdir hndl mixedTestBody

  where
    tmpdir = "tempdir2"

    policy = setProcessFormInputs False defaultUploadPolicy

    hndl = handleFileUploads tmpdir
                             policy
                             (const $ allowWithMaximumSize 300000)
                             hndl'

    hndl' xs = do
        liftIO $ assertEqual "num params" 4 (length xs)
        show xs `deepseq` return ()


------------------------------------------------------------------------------
testPerPartPolicyViolation1 :: Test
testPerPartPolicyViolation1 = testCase "fileUploads/perPartPolicyViolation1" $
                              harness tmpdir hndl mixedTestBody
  where
    tmpdir = "tempdir_pol1"

    hndl = handleFileUploads tmpdir defaultUploadPolicy
                             (const disallow)
                             hndl'

    hndl' xs = do
        p1 <- getParam "field1"
        p2 <- getParam "field2"

        mapM_ f xs

        liftIO $ do
            assertEqual "field1 contents"
                        (Just formContents1)
                        p1

            assertEqual "field2 contents"
                        (Just formContents2)
                        p2

    f (!_, e) = either (\i -> show i `deepseq` return ())
                       (const $ error "expected policy violation")
                       e


------------------------------------------------------------------------------
testPerPartPolicyViolation2 :: Test
testPerPartPolicyViolation2 = testCase "fileUploads/perPartPolicyViolation2" $
                              harness tmpdir hndl mixedTestBody
  where
    tmpdir = "tempdir_pol2"

    hndl = handleFileUploads tmpdir defaultUploadPolicy
                             (const $ allowWithMaximumSize 4)
                             hndl'

    hndl' xs = mapM_ f xs

    f (partInfo, e) = (if partFileName partInfo == Just "file1.txt"
                         then ePass
                         else eFail) e

    eFail = either (\i -> show i `deepseq` return ())
                   (const $ error "expected policy violation")

    ePass = either (throw)
                   (\i -> show i `deepseq` return ())



------------------------------------------------------------------------------
testFormInputPolicyViolation :: Test
testFormInputPolicyViolation = testCase "fileUploads/formInputTooBig" $
                               (harness tmpdir hndl mixedTestBody `catch` h)
  where
    h !(e :: FileUploadException) = do
        let r = fileUploadExceptionReason e
        assertBool "correct exception"
                   (T.isInfixOf "form input" r &&
                    T.isInfixOf "exceeded maximum" r)

    tmpdir = "tempdir_formpol"

    policy = setMaximumFormInputSize 2 defaultUploadPolicy

    hndl = handleFileUploads tmpdir policy
                             (const $ allowWithMaximumSize 4)
                             hndl'

    hndl' xs = show xs `deepseq` return ()


------------------------------------------------------------------------------
testNoBoundary :: Test
testNoBoundary = testCase "fileUploads/noBoundary" $
                 expectException $
                 harness' goBadContentType tmpdir hndl mixedTestBody
  where
    tmpdir = "tempdir_noboundary"

    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'

    hndl' xs = show xs `deepseq` return ()


------------------------------------------------------------------------------
testNoMixedBoundary :: Test
testNoMixedBoundary = testCase "fileUploads/noMixedBoundary" $
                      expectException $
                      harness' go tmpdir hndl badMixedBody
  where
    tmpdir = "tempdir_mixednoboundary"

    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'

    hndl' xs = show xs `deepseq` return ()


------------------------------------------------------------------------------
testWrongContentType :: Test
testWrongContentType = testCase "fileUploads/wrongContentType" $
                       expectException $
                       harness' goWrongContentType tmpdir hndl mixedTestBody
  where
    tmpdir = "tempdir_noboundary"

    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'
           <|> error "expect fail here"

    hndl' xs = show xs `deepseq` return ()


------------------------------------------------------------------------------
testTooManyHeaders :: Test
testTooManyHeaders = testCase "fileUploads/tooManyHeaders" $
                     (harness tmpdir hndl bigHeadersBody `catch` h)
  where
    h (e :: BadPartException) = show e `deepseq` return ()

    tmpdir = "tempdir_tooManyHeaders"

    hndl = handleFileUploads tmpdir defaultUploadPolicy
                             (const $ allowWithMaximumSize 4)
                             hndl'

    hndl' xs = show xs `deepseq` return ()


------------------------------------------------------------------------------
testSlowEnumerator :: Test
testSlowEnumerator = testCase "fileUploads/tooSlow" $
                     (harness' goSlowEnumerator tmpdir hndl mixedTestBody
                               `catch` h0)
  where
    h0 (e :: ConnectionTerminatedException) =
        let (ConnectionTerminatedException se) = e
            (me :: Maybe RateTooSlowException) = fromException se
        in maybe (throw e) h me

    h (e :: RateTooSlowException) = e `seq` return ()

    tmpdir = "tempdir_tooslow"

    policy = setMinimumUploadRate 200000 $
             setMinimumUploadSeconds 2 $
             defaultUploadPolicy

    hndl = handleFileUploads tmpdir policy
                             (const $ allowWithMaximumSize 400000)
                             hndl'

    hndl' xs = show xs `deepseq` return ()


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "fileUploads/trivials" $ do
    assertEqual "" False $ doProcessFormInputs policy
    assertEqual "" 1000  $ getMinimumUploadRate policy
    assertEqual "" 1000  $ getMinimumUploadRate defaultUploadPolicy
    assertEqual "" 5     $ getMinimumUploadSeconds policy
    assertEqual "" 9     $ getUploadTimeout policy

  where
    policy = setProcessFormInputs False $
             setMinimumUploadRate 1000 $
             setMinimumUploadSeconds 5 $
             setUploadTimeout 9 $
             defaultUploadPolicy


------------------------------------------------------------------------------
testDisconnectionCleanup :: Test
testDisconnectionCleanup = testCase "fileUploads/disconnectionCleanup" $ do
    runTest `finally` removeDirectoryRecursive tmpdir
  where
    runTest = do
        eatException $ removeDirectoryRecursive tmpdir
        createDirectoryIfMissing True tmpdir
        rq <- mkDamagedRequest mixedTestBody
        eatException $ liftM snd (run_ $ runIt hndl rq)
        performGC
        dirs <- liftM (filter (\x -> x /= "." && x /= "..")) $
                getDirectoryContents tmpdir
        assertEqual "files should be cleaned up" [] dirs
    

    tmpdir = "tempdir1"
    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'

    hndl' _ = return ()
        

------------------------------------------------------------------------------
harness :: FilePath -> Snap a -> ByteString -> IO ()
harness = harness' go


------------------------------------------------------------------------------
harness' :: (Snap a -> ByteString -> IO Response)
         -> FilePath
         -> Snap a
         -> ByteString
         -> IO ()
harness' g tmpdir hndl body = (do
    createDirectoryIfMissing True tmpdir
    g hndl body
    return ()) `finally` removeDirectoryRecursive tmpdir


------------------------------------------------------------------------------
mkRequest :: ByteString -> IO Request
mkRequest body = do
    enum <- newIORef $ SomeEnumerator $ enumBS body

    let hdrs = Map.fromList [
                 ("Content-type", [S.append "multipart/form-data; boundary="
                                            boundaryValue])
                ]

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False hdrs
                     enum Nothing POST (1,1) [] "" "/" "/"
                     "/" "" Map.empty


------------------------------------------------------------------------------
mkDamagedRequest :: ByteString -> IO Request
mkDamagedRequest body = do
    enum <- newIORef $ SomeEnumerator $ enum

    let hdrs = Map.fromList [
                 ("Content-type", [S.append "multipart/form-data; boundary="
                                            boundaryValue])
                ]

    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False hdrs
                     enum Nothing POST (1,1) [] "" "/" "/"
                     "/" "" Map.empty
  where
    enum = enumBS (S.take (S.length body - 1) body) >==> dieNow
    dieNow _ = throw TestException


------------------------------------------------------------------------------
go :: Snap a -> ByteString -> IO Response
go m s = do
    rq <- mkRequest s
    liftM snd (run_ $ runIt m rq)


------------------------------------------------------------------------------
goBadContentType :: Snap a -> ByteString -> IO Response
goBadContentType m s = do
    rq <- mkRequest s
    let rq' = setHeader "Content-Type" "multipart/form-data" rq
    liftM snd (run_ $ runIt m rq')


------------------------------------------------------------------------------
goWrongContentType :: Snap a -> ByteString -> IO Response
goWrongContentType m s = do
    rq <- mkRequest s
    let rq' = setHeader "Content-Type" "text/plain" rq
    liftM snd (run_ $ runIt m rq')

------------------------------------------------------------------------------
goSlowEnumerator :: Snap a -> ByteString -> IO Response
goSlowEnumerator m s = do
    rq <- mkRequest s
    writeIORef (rqBody rq) $ SomeEnumerator slowEnum
    mx <- timeout (20*seconds) (liftM snd (run_ $ runIt m rq))
    maybe (error "timeout") return mx

  where
    body = S.unpack s

    slowEnum x = goo x body

    goo (Continue k) []     = k EOF
    goo (Continue k) (x:xs) = do
        debug $ "goSlowEnumerator: sending " ++ show x
        step <- lift $ runIteratee $ k $ Chunks [ S.pack (x:[]) ]
        liftIO waitabit
        goo step xs
    goo (Error e) _ = throwError e
    goo _ _ = error "impossible"


------------------------------------------------------------------------------
waitabit :: IO ()
waitabit = threadDelay $ 2*seconds

------------------------------------------------------------------------------
seconds :: Int
seconds = (10::Int) ^ (6::Int)


------------------------------------------------------------------------------
runIt :: Snap a -> Request -> Iteratee ByteString IO (Request, Response)
runIt m rq = iterateeDebugWrapper "test" $ runSnap m d d rq
  where
    d :: forall a . Show a => a -> IO ()
    d = \x -> show x `deepseq` return ()


------------------------------------------------------------------------------
-- TEST DATA

formContents1 :: ByteString
formContents1 = "form contents 1"

formContents2 :: ByteString
formContents2 = "form contents 2 zzzzzzzzzzzzzzzzzzzz"

file1Contents :: ByteString
file1Contents = "foo"

file2Contents :: ByteString
file2Contents = "... contents of file2.gif ..."

boundaryValue :: ByteString
boundaryValue = "fkjldsakjfdlsafldksjf"

subBoundaryValue :: ByteString
subBoundaryValue = "zjkzjjfjskzjzjkz"

crlf :: ByteString
crlf = "\r\n"


------------------------------------------------------------------------------
mixedTestBody :: ByteString
mixedTestBody =
    S.concat
         [ "--"
         , boundaryValue
         , crlf
         , "content-disposition: form-data; name=\"field1\"\r\n"
         , crlf
         , formContents1
         , crlf
         , "--"
         , boundaryValue
         , crlf
         , "content-disposition: form-data; name=\"field2\"\r\n"
         , crlf
         , formContents2
         , crlf
         , "--"
         , boundaryValue
         , crlf
         , "content-disposition: form-data; name=\"files\"\r\n"
         , "Content-type: multipart/mixed; boundary="
         , subBoundaryValue
         , crlf
         , crlf
         , "--"
         , subBoundaryValue
         , crlf
         , "Content-disposition: attachment; filename=\"file1.txt\"\r\n"
         , "Content-Type: text/plain\r\n"
         , crlf
         , file1Contents
         , crlf
         , "--"
         , subBoundaryValue
         , crlf
         , "Content-disposition: attachment; filename=\"file2.gif\"\r\n"
         , "Content-type: image/gif\r\n"
         , "Content-Transfer-Encoding: binary\r\n"
         , crlf
         , file2Contents
         , crlf
         , "--"
         , subBoundaryValue
         , "--\r\n"
         , "--"
         , boundaryValue
         , "--\r\n"
         ]


------------------------------------------------------------------------------
badMixedBody :: ByteString
badMixedBody =
    S.concat
         [ "--"
         , boundaryValue
         , crlf
         , "content-disposition: form-data; name=\"field1\"\r\n"
         , crlf
         , formContents1
         , crlf
         , "--"
         , boundaryValue
         , crlf
         , "content-disposition: form-data; name=\"field2\"\r\n"
         , crlf
         , formContents2
         , crlf
         , "--"
         , boundaryValue
         , crlf
         , "content-disposition: form-data; name=\"files\"\r\n"
         , "Content-type: multipart/mixed"
         , crlf
         , crlf
         , "--"
         , subBoundaryValue
         , crlf
         , "Content-disposition: attachment; filename=\"file1.txt\"\r\n"
         , "Content-Type: text/plain\r\n"
         , crlf
         , file1Contents
         , crlf
         , "--"
         , subBoundaryValue
         , crlf
         , "Content-disposition: attachment; filename=\"file2.gif\"\r\n"
         , "Content-type: image/gif\r\n"
         , "Content-Transfer-Encoding: binary\r\n"
         , crlf
         , file2Contents
         , crlf
         , "--"
         , subBoundaryValue
         , "--\r\n"
         , "--"
         , boundaryValue
         , "--\r\n"
         ]


------------------------------------------------------------------------------
bigHeadersBody :: ByteString
bigHeadersBody =
    S.concat (
         [ "--"
         , boundaryValue
         , crlf
         , "content-disposition: form-data; name=\"field1\"\r\n" ]
         ++
         map (\i -> S.pack ("field_" ++ show i ++ ": bar\r\n")) [1..40000::Int]
         ++
         [ crlf
         , formContents1
         , crlf
         , "--"
         , boundaryValue
         , "--\r\n"
         ])


------------------------------------------------------------------------------
expectException :: IO a -> IO ()
expectException m = do
    e <- try m
    case e of
      Left (z::SomeException)  -> (show z) `deepseq` return ()
      Right _ -> assertFailure "expected exception, didn't get one"
