{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Util.FileUploads.Tests
  ( tests ) where

------------------------------------------------------------------------------
import           Control.Applicative            (Alternative ((<|>)), (<$>))
import           Control.DeepSeq                (deepseq)
import           Control.Exception              (ErrorCall (..), evaluate, throwIO)
import           Control.Exception.Lifted       (Exception (fromException, toException), catch, finally, throw)
import           Control.Monad                  (Monad (return, (>>), (>>=)), liftM, void)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as S
import qualified Data.ByteString.Lazy           as L
import           Data.IORef                     (atomicModifyIORef, newIORef, readIORef, writeIORef)
import           Data.List                      (foldl', length)
import qualified Data.Map                       as Map
import           Data.Maybe                     (Maybe (..), fromJust, maybe)
import qualified Data.Text                      as T
import           Data.Typeable                  (Typeable)
import           Prelude                        (Bool (..), Either (..), Eq (..), FilePath, IO, Int, Num (..), Show (..), const, either, error, filter, map, seq, snd, ($), ($!), (&&), (++), (.))
import           Snap.Internal.Core             (EscapeSnap (TerminateConnection), Snap, getParam, getPostParam, getQueryParam, runSnap)
import           Snap.Internal.Http.Types       (Request (rqBody), Response, setHeader)
import           Snap.Internal.Util.FileUploads (BadPartException (..), FileUploadException (..), FormFile (..), PartDisposition (..), PartInfo (..), PolicyViolationException (..), allowWithMaximumSize, defaultFileUploadPolicy, defaultUploadPolicy, disallow, doProcessFormInputs, fileUploadExceptionReason, foldMultipart, getMaximumNumberOfFormInputs, getMinimumUploadRate, getMinimumUploadSeconds, getUploadTimeout, handleFileUploads, handleFileUploads, handleFormUploads, setMaximumFileSize, setMaximumFormInputSize, setMaximumNumberOfFiles, setMaximumNumberOfFormInputs, setMaximumSkippedFileSize, setMinimumUploadRate, setMinimumUploadSeconds, setProcessFormInputs, setSkipFilesWithoutNames, setUploadTimeout, storeAsLazyByteString, toPartDisposition, withTemporaryStore)
import qualified Snap.Test                      as Test
import           Snap.Test.Common               (coverEqInstance, coverShowInstance, coverTypeableInstance, eatException, expectExceptionH, seconds, waitabit)
import qualified Snap.Types.Headers             as H
import           System.Directory               (createDirectoryIfMissing, doesFileExist, getDirectoryContents, removeDirectoryRecursive, removeFile)
import           System.IO.Streams              (RateTooSlowException)
import qualified System.IO.Streams              as Streams
import           System.Mem                     (performGC)
import           System.Timeout                 (timeout)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertBool, assertEqual, assertFailure)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data TestException = TestException
  deriving (Show, Typeable)

instance Exception TestException

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testFoldMultipart1
        , testFoldMultipart2
        , testFilePolicyViolation1
        , testFilePolicyViolation2
        , testEmptyNamePolicyViolation
        , testEmptyNameStore
        , testEmptyNameSkip
        , testTemporaryStore
        , testTemporaryStoreSafeMove
        , testSuccess1
        , testSuccess2
        , testSuccessUtf8Filename
        , testBadParses
        , testPerPartPolicyViolation1
        , testPerPartPolicyViolation2
        , testFormInputsPolicyViolation
        , testFormSizePolicyViolation
        , testNoFileName
        , testNoFileNameTooBig
        , testTooManyHeaders
        , testNoBoundary
        , testNoMixedBoundary
        , testWrongContentType
        , testSlowEnumerator
        , testSlowEnumerator2
        , testAbortedBody
        , testTrivials
        , testDisconnectionCleanup
        ]

------------------------------------------------------------------------------
testFoldMultipart1 :: Test
testFoldMultipart1 = testCase "fileUploads/fold1" $
     void $ go hndl mixedTestBody

  where
    hndl :: Snap ()
    hndl = do
        (params, files) <- foldMultipart defaultUploadPolicy  hndl' []

        let fileMap = foldl' f Map.empty files
        liftIO $ assertEqual "2 params returned" 2 (length params)
        let [p1, p2] = params

        liftIO $ do
            let Just (a1, a2, a3) = Map.lookup "file1.txt" fileMap
            let Just (b1, b2, b3) = Map.lookup "file2.gif" fileMap
            assertEqual "file1 contents"
                        ("text/plain", file1Contents)
                        (a1, a2)
            assertEqual "file1 header 1"
                        (Just "text/plain")
                        (H.lookup "content-type" a3)
            assertEqual "file1 header 2"
                        (Just "attachment; filename=\"file1.txt\"")
                        (H.lookup "content-disposition" a3)

            assertEqual "file2 contents"
                        ("image/gif", file2Contents)
                        (b1, b2)
            assertEqual "file2 header 1"
                        (Just "image/gif")
                        (H.lookup "content-type" b3)
            assertEqual "file2 header 2"
                        (Just "attachment; filename=\"file2.gif\"")
                        (H.lookup "content-disposition" b3)

            assertEqual "field1 contents"
                        ("field1", formContents1)
                        p1

            assertEqual "field2 contents"
                        ("field2", formContents2)
                        p2

    f mp (fn, ct, x, hdrs) = Map.insert fn (ct,x,hdrs) mp

    hndl' partInfo istream acc = do
       let
         fn = fromJust $ partFileName partInfo
         ct = partContentType partInfo
         hdrs = partHeaders partInfo
       body <- S.concat <$> Streams.toList istream
       return (acc ++ [(fn, ct, body, hdrs)])



------------------------------------------------------------------------------
testFoldMultipart2 :: Test
testFoldMultipart2 = testCase "fileUploads/fold2" $
    void $ go hndl mixedTestBody
  where
    policy = setProcessFormInputs False defaultUploadPolicy

    hndl = do
        (fields, fileCount) <- foldMultipart policy hndl' (0::Int)

        liftIO $ do
             assertEqual "num params" 4 fileCount
             assertEqual "num processed" 0 (length fields)

    hndl' !_ !_ !acc = return $ acc + 1



------------------------------------------------------------------------------
testFilePolicyViolation1 :: Test
testFilePolicyViolation1 = testCase "fileUploads/filePolicyViolation1" $
     assertThrows (go hndl mixedTestBody) h
  where
    h e = assertIsFileSizeException e

    hndl = handleFormUploads defaultUploadPolicy
             (setMaximumFileSize 0 defaultFileUploadPolicy)
             (const storeAsLazyByteString)



------------------------------------------------------------------------------
testFilePolicyViolation2 :: Test
testFilePolicyViolation2 = testCase "fileUploads/filePolicyViolation2" $
     assertThrows (go hndl mixedTestBody) h
  where
    h (PolicyViolationException r) =
        assertBool "correct exception"
                   (T.isInfixOf "number of files exceeded the maximum" r)

    hndl = handleFormUploads defaultUploadPolicy
             (setMaximumNumberOfFiles 0 defaultFileUploadPolicy)
             (const storeAsLazyByteString)


------------------------------------------------------------------------------
testEmptyNamePolicyViolation :: Test
testEmptyNamePolicyViolation = testCase "fileUploads/emptyNamePolicyViolation" $
     assertThrows (go hndl noFileNameTestBody) h
  where
    h e = assertIsFileSizeException e

    hndl = handleFormUploads defaultUploadPolicy
             (setSkipFilesWithoutNames True defaultFileUploadPolicy)
             (const storeAsLazyByteString)


------------------------------------------------------------------------------
assertIsFileSizeException :: PolicyViolationException -> Assertion
assertIsFileSizeException (PolicyViolationException r) =
    assertBool "file size exception"
        (T.isInfixOf "File" r &&
          T.isInfixOf "exceeded maximum allowable size" r)


------------------------------------------------------------------------------
testEmptyNameStore :: Test
testEmptyNameStore = testCase "fileUploads/emptyNameStore" $
     void $ go hndl noFileNameTestBody
  where
    hndl = do
      (inputs, files) <- handleFormUploads defaultUploadPolicy
                         (setSkipFilesWithoutNames False defaultFileUploadPolicy)
                         (const storeAsLazyByteString)
      liftIO $ do
         assertEqual "got both files" 2 (length files)
         let [f1, f2] = files
         assertEqual "file1 contents"
                     (FormFile "files" $ L.fromChunks [file1Contents])
                     f1
         assertEqual "file2 contents"
                     (FormFile "files" $ L.fromChunks [file2Contents])
                     f2
         assertEqual "inputs present" 2 (length inputs)
      return ()


------------------------------------------------------------------------------
testEmptyNameSkip :: Test
testEmptyNameSkip = testCase "fileUploads/emptyNameSkip" $
     void $ go hndl noFileNameTestBody
  where
    hndl = do
      (inputs, files) <- handleFormUploads defaultUploadPolicy
                         (  setMaximumSkippedFileSize 4000
                          . setSkipFilesWithoutNames True
                          $ defaultFileUploadPolicy )
                         (const storeAsLazyByteString)
      liftIO $ do
         assertEqual "files skipped" 0 (length files)
         assertEqual "inputs present" 2 (length inputs)
      return ()


------------------------------------------------------------------------------
testTemporaryStore :: Test
testTemporaryStore = testCase "fileUploads/temporaryStore" $
     harness "tempdir1" hndl mixedTestBody
  where
    hndl = do
      (fn1, fn2) <- withTemporaryStore "tempdir1" "upload" $ \store -> do
          (inputs, files) <- handleFormUploads defaultUploadPolicy
                                               defaultFileUploadPolicy
                                               (const store)
          liftIO $ do
             assertEqual "num files" 2 (length files)
             assertEqual "inputs present" 2 (length inputs)
             let [FormFile name1 fn1, FormFile name2 fn2] = files
             fc1 <- liftIO $ S.readFile fn1
             fc2 <- liftIO $ S.readFile fn2

             assertEqual "file1 content"
                         ("files", file1Contents)
                         (name1, fc1)

             assertEqual "file2 content"
                         ("files", file2Contents)
                         (name2, fc2)
             return (fn1, fn2)
      liftIO $ do
        ex1 <- doesFileExist fn1
        ex2 <- doesFileExist fn2
        assertEqual "file1 deleted" False ex1
        assertEqual "file2 deleted" False ex2


------------------------------------------------------------------------------
testTemporaryStoreSafeMove :: Test
testTemporaryStoreSafeMove = testCase "fileUploads/temporaryStoreSafeMove" $
     harness "tempdir1" hndl mixedTestBody
  where
    hndl = do
      -- should not throw
      withTemporaryStore "tempdir1" "upload" $ \store -> do
          (_, files) <- handleFormUploads defaultUploadPolicy
                                               defaultFileUploadPolicy
                                               (const store)
          liftIO $ do
             assertEqual "num files" 2 (length files)
             let [FormFile _ fn1, FormFile _ fn2] = files
             removeFile fn1
             removeFile fn2



------------------------------------------------------------------------------
testSuccess1 :: Test
testSuccess1 = testCase "fileUploads/success1" $
               harness tmpdir hndl mixedTestBody

  where
    tmpdir = "tempdir1"

    hndl = do
        xs <- handleFileUploads tmpdir
                                defaultUploadPolicy
                                (const $ allowWithMaximumSize 300000)
                                hndl'

        let fileMap = foldl' f Map.empty xs
        p1  <- getParam "field1"
        p1P <- getPostParam "field1"
        p1Q <- getQueryParam "field1"
        p2  <- getParam "field2"

        liftIO $ do
            let Just (a1, a2, a3) = Map.lookup "file1.txt" fileMap
            let Just (b1, b2, b3) = Map.lookup "file2.gif" fileMap
            assertEqual "file1 contents"
                        ("text/plain", file1Contents)
                        (a1, a2)
            assertEqual "file1 header 1"
                        (Just "text/plain")
                        (H.lookup "content-type" a3)
            assertEqual "file1 header 2"
                        (Just "attachment; filename=\"file1.txt\"")
                        (H.lookup "content-disposition" a3)

            assertEqual "file2 contents"
                        ("image/gif", file2Contents)
                        (b1, b2)
            assertEqual "file2 header 1"
                        (Just "image/gif")
                        (H.lookup "content-type" b3)
            assertEqual "file2 header 2"
                        (Just "attachment; filename=\"file2.gif\"")
                        (H.lookup "content-disposition" b3)

            assertEqual "field1 contents"
                        (Just formContents1)
                        p1

            assertEqual "field1 POST contents" (Just formContents1) p1P
            assertEqual "field1 query contents" Nothing p1Q
            assertEqual "field2 contents" (Just formContents2) p2

    f mp (fn, ct, x, hdrs) = Map.insert fn (ct,x,hdrs) mp

    hndl' partInfo =
        either throw
               (\fp -> do
                    x <- liftIO $ S.readFile fp
                    let fn = fromJust $ partFileName partInfo
                    let ct = partContentType partInfo
                    let hdrs = partHeaders partInfo
                    return (fn, ct, x, hdrs))


------------------------------------------------------------------------------
testSuccess2 :: Test
testSuccess2 = testCase "fileUploads/success2" $
               harness tmpdir hndl mixedTestBody

  where
    tmpdir = "tempdir2"

    policy = setProcessFormInputs False defaultUploadPolicy

    hndl = do
        ref <- liftIO $ newIORef (0::Int)
        _ <- handleFileUploads tmpdir
                               policy
                               (const $ allowWithMaximumSize 300000)
                               (hndl' ref)

        n <- liftIO $ readIORef ref
        liftIO $ assertEqual "num params" 4 n

    hndl' !ref !_ !_ = atomicModifyIORef ref (\x -> (x+1, ()))

 ------------------------------------------------------------------------------
testSuccessUtf8Filename :: Test
testSuccessUtf8Filename = testCase "fileUploads/utf8-filename" $
                          harness tmpdir hndl utf8FilenameBody

  where
    tmpdir = "tempdir3"

    hndl = do
        xs <- handleFileUploads tmpdir
                                defaultUploadPolicy
                                (const $ allowWithMaximumSize 300000)
                                hndl'

        liftIO $ assertEqual "filename" [filenameUtf8] xs

    hndl' pinfo _ = return $ fromJust $ partFileName pinfo


------------------------------------------------------------------------------
testBadParses :: Test
testBadParses = testCase "fileUploads/badParses" $ do
                harness tmpdir hndl mixedTestBodyWithBadTypes
  where
    tmpdir = "tempdir_bad_types"

    hndl = do
        xs <- handleFileUploads tmpdir
                                defaultUploadPolicy
                                (const $ allowWithMaximumSize 300000)
                                hndl'

        let fileMap = foldl' f Map.empty xs
        p1   <- getParam "field1"
        p1P  <- getPostParam "field1"
        p1Q  <- getQueryParam "field1"
        p2   <- getParam "field2"
        pBoo <- getParam "boo"

        liftIO $ do
            assertEqual "file1 contents"
                        (Just ("text/plain", file1Contents))
                        (Map.lookup "file1.txt" fileMap)

            assertEqual "file2 contents"
                        (Just ("text/plain", file2Contents))
                        (Map.lookup "file2.gif" fileMap)

            assertEqual "field1 param contents" (Just formContents1) p1
            assertEqual "field1 POST contents" (Just formContents1) p1P
            assertEqual "field1 query contents" Nothing p1Q
            assertEqual "field2 contents" Nothing p2
            assertEqual "boo contents" (Just "boo") pBoo

    f mp (fn, ct, x) = Map.insert fn (ct,x) mp

    hndl' partInfo =
        either throw
               (\fp -> do
                    x <- liftIO $ S.readFile fp
                    let fn = fromJust $ partFileName partInfo
                    let ct = partContentType partInfo
                    return (fn, ct, x))



------------------------------------------------------------------------------
testPerPartPolicyViolation1 :: Test
testPerPartPolicyViolation1 = testCase "fileUploads/perPartPolicyViolation1" $
                              harness tmpdir hndl mixedTestBody
  where
    tmpdir = "tempdir_pol1"

    hndl = do
        _ <- handleFileUploads tmpdir defaultUploadPolicy
                               (const disallow)
                               hndl'

        p1 <- getParam "field1"
        p2 <- getParam "field2"


        liftIO $ do
            assertEqual "field1 contents"
                        (Just formContents1)
                        p1

            assertEqual "field2 contents"
                        (Just formContents2)
                        p2

    hndl' !_ e = either (\i -> show i `deepseq` return $! ())
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

    hndl' partInfo e = (if partFileName partInfo == Just "file1.txt"
                          then ePass
                          else eFail) e

    eFail = either (\i -> show i `deepseq` return ())
                   (const $ error "expected policy violation")

    ePass = either (throw)
                   (\i -> show i `deepseq` return ())



------------------------------------------------------------------------------
testNoFileName :: Test
testNoFileName = testCase "fileUploads/noFileName" $
                 (harness tmpdir hndl noFileNameTestBody)
  where
    tmpdir = "tempdir_noname"

    hndl = handleFileUploads tmpdir defaultUploadPolicy
                             (const $ allowWithMaximumSize 400000)
                             hndl'

    hndl' pinfo !_ = do
        assertEqual "filename" Nothing $ partFileName pinfo
        assertEqual "disposition" DispositionFile $ partDisposition pinfo


------------------------------------------------------------------------------
testNoFileNameTooBig :: Test
testNoFileNameTooBig = testCase "fileUploads/noFileNameTooBig" $
                       assertThrows (harness tmpdir hndl noFileNameTestBody) h
  where
    h !(e :: FileUploadException) = do
        let r = fileUploadExceptionReason e
        assertBool "correct exception"
                   (T.isInfixOf "File" r &&
                    T.isInfixOf "exceeded maximum allowable size" r)

    tmpdir = "tempdir_noname_toobig"

    hndl = handleFileUploads tmpdir defaultUploadPolicy
                             (const $ allowWithMaximumSize 1)
                             hndl'

    hndl' pinfo !e = do
        let (Left !x) = e
        coverShowInstance x
        assertEqual "filename" Nothing $ partFileName pinfo
        assertEqual "disposition" DispositionFile $ partDisposition pinfo
        throwIO x


------------------------------------------------------------------------------
testFormSizePolicyViolation :: Test
testFormSizePolicyViolation =
    testCase "fileUploads/formSizePolicy" $
    assertThrows (harness tmpdir hndl mixedTestBody) h
  where
    h !(e :: FileUploadException) = do
        let r = fileUploadExceptionReason e
        assertBool "correct exception"
                   (T.isInfixOf "form input" r &&
                    T.isInfixOf "exceeded maximum permissible" r)

    tmpdir = "tempdir_formpol"

    policy = setMaximumFormInputSize 2 defaultUploadPolicy

    hndl = handleFileUploads tmpdir policy
                             (const $ allowWithMaximumSize 4)
                             hndl'

    hndl' xs _ = show xs `deepseq` return ()


------------------------------------------------------------------------------
testFormInputsPolicyViolation :: Test
testFormInputsPolicyViolation =
    testCase "fileUploads/formInputsPolicy" $
    assertThrows (harness tmpdir hndl mixedTestBody) h
  where
    h !(e :: FileUploadException) = do
        let r = fileUploadExceptionReason e
        assertBool "correct exception"
                   (T.isInfixOf "number of form inputs" r &&
                    T.isInfixOf "exceeded maximum" r)

    tmpdir = "tempdir_formpol2"

    policy = setMaximumNumberOfFormInputs 0 defaultUploadPolicy

    hndl = handleFileUploads tmpdir policy
                             (\x -> x `seq` allowWithMaximumSize 4000)
                             hndl'

    hndl' xs _ = show xs `deepseq` return ()


------------------------------------------------------------------------------
testNoBoundary :: Test
testNoBoundary = testCase "fileUploads/noBoundary" $
                 expectExceptionH $
                 harness' goBadContentType tmpdir hndl mixedTestBody
  where
    tmpdir = "tempdir_noboundary"

    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'

    hndl' xs _ = show xs `deepseq` return ()


------------------------------------------------------------------------------
testNoMixedBoundary :: Test
testNoMixedBoundary = testCase "fileUploads/noMixedBoundary" $
                      expectExceptionH $
                      harness' go tmpdir hndl badMixedBody
  where
    tmpdir = "tempdir_mixednoboundary"

    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'

    hndl' xs _ = show xs `deepseq` return ()


------------------------------------------------------------------------------
testWrongContentType :: Test
testWrongContentType = testCase "fileUploads/wrongContentType" $
                       expectExceptionH $
                       harness' goWrongContentType tmpdir hndl mixedTestBody
  where
    tmpdir = "tempdir_noboundary"

    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'
           <|> error "expect fail here"

    hndl' xs _ = show xs `deepseq` return ()


------------------------------------------------------------------------------
testTooManyHeaders :: Test
testTooManyHeaders = testCase "fileUploads/tooManyHeaders" $
                     assertThrows (harness tmpdir hndl bigHeadersBody) h
  where
    h (e :: BadPartException) = show e `deepseq` return ()

    tmpdir = "tempdir_tooManyHeaders"

    hndl = handleFileUploads tmpdir defaultUploadPolicy
                             (const $ allowWithMaximumSize 4)
                             hndl'

    hndl' xs _ = show xs `deepseq` return ()


------------------------------------------------------------------------------
testAbortedBody :: Test
testAbortedBody = testCase "fileUploads/abortedBody" $
                  expectExceptionH $
                  harness' goAndAbort tmpdir hndl abortedTestBody
  where
    tmpdir = "tempdir_abort"

    hndl = handleFileUploads tmpdir defaultUploadPolicy
                             (const $ allowWithMaximumSize 400000)
                             hndl'

    hndl' xs _ = show xs `deepseq` return ()



------------------------------------------------------------------------------
testSlowEnumerator :: Test
testSlowEnumerator =
    testCase "fileUploads/tooSlow" $
    assertThrows (harness' goSlowEnumerator tmpdir hndl mixedTestBody) h0
  where
    h0 (e :: EscapeSnap) = do
        let (TerminateConnection se) = e
            (me :: Maybe RateTooSlowException) = fromException se
        maybe (throw e) h me

    h (e :: RateTooSlowException) = coverShowInstance e

    tmpdir = "tempdir_tooslow"

    policy = setMinimumUploadRate 200000 $
             setMinimumUploadSeconds 2 $
             defaultUploadPolicy

    hndl = handleFileUploads tmpdir policy
                             (const $ allowWithMaximumSize 400000)
                             hndl'

    hndl' xs _ = show xs `deepseq` return ()


------------------------------------------------------------------------------
testSlowEnumerator2 :: Test
testSlowEnumerator2 =
    testCase "fileUploads/tooSlow2" $
    assertThrows (harness' goSlowEnumerator tmpdir hndl mixedTestBody) h0
  where
    h0 (e :: EscapeSnap) = do
        let (TerminateConnection se) = e
            (me :: Maybe RateTooSlowException) = fromException se
        maybe (throw e) h me

    h (e :: RateTooSlowException) = e `seq` return ()

    tmpdir = "tempdir_tooslow2"

    policy = setUploadTimeout 2 defaultUploadPolicy

    hndl = handleFileUploads tmpdir policy
                             (const $ allowWithMaximumSize 400000)
                             hndl'

    hndl' xs _ = show xs `deepseq` return ()


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "fileUploads/trivials" $ do
    assertEqual "" False $ doProcessFormInputs policy
    assertEqual "" 1000  $ getMinimumUploadRate policy
    assertEqual "" 1000  $ getMinimumUploadRate defaultUploadPolicy
    assertEqual "" 5     $ getMinimumUploadSeconds policy
    assertEqual "" 9     $ getUploadTimeout policy

    let pvi = PolicyViolationException ""
    coverTypeableInstance pvi
    evaluate $ ((fromJust $
                 fromException (toException pvi)) :: PolicyViolationException)
    evaluate $ ((fromJust $
                 fromException (toException pvi)) :: FileUploadException)
    let !_ = policyViolationExceptionReason pvi

    let bpi = BadPartException ""
    coverTypeableInstance bpi
    let !_ = badPartExceptionReason bpi

    coverShowInstance $ WrappedFileUploadException $ BadPartException ""
    coverShowInstance $ PartInfo "" Nothing "" DispositionFile (H.empty)
    coverShowInstance $ toPartDisposition ""
    coverEqInstance $ DispositionOther ""

    let !gfui = WrappedFileUploadException $ BadPartException ""
    evaluate $ fileUploadExceptionReason gfui

    void $ evaluate
         $ getMaximumNumberOfFormInputs
         $ setMaximumNumberOfFormInputs 2 policy

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
        eatException $ liftM snd (runIt hndl rq)
        performGC
        dirs <- liftM (filter (\x -> x /= "." && x /= "..")) $
                getDirectoryContents tmpdir
        assertEqual "files should be cleaned up" [] dirs


    tmpdir = "tempdirC"
    hndl = handleFileUploads tmpdir
                             defaultUploadPolicy
                             (const $ allowWithMaximumSize 300000)
                             hndl'

    hndl' _ _ = return ()


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
    !_ <- g hndl body
    return ()) `finally` removeDirectoryRecursive tmpdir


------------------------------------------------------------------------------
assertThrows :: Exception e => IO a -> (e -> IO ()) -> IO ()
assertThrows act handle = catch action handle
  where
    action = act >> assertFailure "Expected exception to be thrown"


------------------------------------------------------------------------------
mkRequest :: ByteString -> IO Request
mkRequest body = Test.buildRequest $ Test.postRaw "/" ct body
  where
    ct = S.append "multipart/form-data; boundary=" boundaryValue


------------------------------------------------------------------------------
mkDamagedRequest :: ByteString -> IO Request
mkDamagedRequest body = do
    req <- Test.buildRequest $ Test.postRaw "/" ct ""

    e <- newIORef False >>= Streams.makeInputStream . enum

    return $! req { rqBody = e }

  where
    ct = S.append "multipart/form-data; boundary=" boundaryValue
    enum ref = do
        x <- readIORef ref
        if x
           then throw TestException
           else do writeIORef ref True
                   return $! Just $! S.take (S.length body - 1) body


------------------------------------------------------------------------------
go :: Snap a -> ByteString -> IO Response
go m s = do
    rq <- mkRequest s
    liftM snd (runIt m rq)


------------------------------------------------------------------------------
goBadContentType :: Snap a -> ByteString -> IO Response
goBadContentType m s = do
    rq <- mkRequest s
    let rq' = setHeader "Content-Type" "multipart/form-data" rq
    liftM snd (runIt m rq')


------------------------------------------------------------------------------
goWrongContentType :: Snap a -> ByteString -> IO Response
goWrongContentType m s = do
    rq <- mkRequest s
    let rq' = setHeader "Content-Type" "text/plain" rq
    liftM snd (runIt m rq')


------------------------------------------------------------------------------
goSlowEnumerator :: Snap a -> ByteString -> IO Response
goSlowEnumerator m s = do
    rq <- mkRequest s
    e  <- Streams.fromGenerator slowInput
    let rq' = rq { rqBody = e }
    mx <- timeout (20*seconds) (liftM snd (runIt m rq'))
    maybe (error "timeout") return mx

  where
    body = S.unpack s

    slowInput = f body
      where
        f []     = return ()
        f (x:xs) = do
            liftIO waitabit
            Streams.yield $ S.singleton x
            f xs


------------------------------------------------------------------------------
goAndAbort :: Snap a -> ByteString -> IO Response
goAndAbort m s = do
    rq <- mkRequest s
    e  <- Streams.fromGenerator generator
    let rq' = rq { rqBody = e }
    mx <- timeout (20*seconds) (liftM snd (runIt m rq'))
    maybe (error "timeout") return mx

  where
    generator = do
        Streams.yield s
        liftIO $ throwIO
               $ ErrorCall "For in that sleep of death what dreams may come."


------------------------------------------------------------------------------
runIt :: Snap a -> Request -> IO (Request, Response)
runIt m rq = runSnap m d bump rq
  where
    bump !f = let !_ = f 1 in return $! ()

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

filenameUtf8 :: ByteString
filenameUtf8 =  "\xd1\x82\xd0\xb5\xd1\x81\xd1\x82.png" -- "тест.png" as UTF-8

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
mixedTestBodyWithBadTypes :: ByteString
mixedTestBodyWithBadTypes =
    S.concat
         [ "--"
         , boundaryValue
         , crlf
         , "content-type: ;\x01;\x01;\x01;\r\n"
         , "content-disposition: form-data; name=\"field1\"\r\n\r\n"
         , formContents1
         , crlf
         , "--"
         , boundaryValue
         , crlf
         , "content-disposition: form-data;\x01;;;\x01 name=\"field2\"\r\n"
         , crlf
         , formContents2
         , crlf
         , "--"
         , boundaryValue
         , crlf
         , "content-disposition: \x01\x01\x01\x01\r\n"
         , "Content-type: multipart/mixed; boundary="
         , subBoundaryValue
         , crlf
         , crlf
         , "--"
         , subBoundaryValue
         , crlf
         , "Content-disposition: attachment; filename=\"file1.txt\"\r\n"
         , "Content-Type: ;\x01;\x01;\x01;\r\n"
         , crlf
         , file1Contents
         , crlf
         , "--"
         , subBoundaryValue
         , crlf
         , "Content-disposition: attachment; filename=\"file2.gif\"\r\n"
         , "Content-type: ;\x01;\x01;\x01\r\n"
         , "Content-Transfer-Encoding: binary\r\n"
         , crlf
         , file2Contents
         , crlf
         , "--"
         , subBoundaryValue
         , "--\r\n"
         , "--"
         , boundaryValue
         , crlf
         , "Content-type: multipart/mixed; \x01\x01;;\x01;\r\n"
         , "Content-disposition: form-data; name=boo\r\n"
         , crlf
         , "boo"
         , crlf
         , "--"
         , boundaryValue
         , "--\r\n"
         ]


------------------------------------------------------------------------------
badMixedBody :: ByteString
badMixedBody =
    S.concat
         [ crlf
         , "--"
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
noFileNameTestBody :: ByteString
noFileNameTestBody =
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
         , "Content-disposition: file\r\n"
         , "Content-Type: text/plain\r\n"
         , crlf
         , file1Contents
         , crlf
         , "--"
         , subBoundaryValue
         , crlf
         , "Content-disposition: file\r\n"
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
abortedTestBody :: ByteString
abortedTestBody =
 S.concat [ "--"
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
          , "fdjkljflsdkjfsd"
          ]


------------------------------------------------------------------------------
utf8FilenameBody :: ByteString
utf8FilenameBody =
    S.concat
         [ "--"
         , boundaryValue
         , crlf
         , "Content-Disposition: form-data; name=\"file\"; filename=\""
         , filenameUtf8
         , "\"\r\n"
         , "Content-Type: image/png\r\n"
         , crlf
         , file2Contents
         , crlf
         , "--"
         , boundaryValue
         , "--\r\n"
         ]
