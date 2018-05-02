{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Snap.Internal.Util.FileUploads
  ( -- * Functions
    handleFormUploads
  , foldMultipart
  , PartFold
  , FormParam
  , FormFile (..)
  , storeAsLazyByteString
  , withTemporaryStore
    -- ** Backwards compatible API
  , handleFileUploads
  , handleMultipart
  , PartProcessor

    -- * Uploaded parts
  , PartInfo(..)
  , PartDisposition(..)
  , toPartDisposition

    -- ** Policy
    -- *** General upload policy
  , UploadPolicy(..)
  , defaultUploadPolicy
  , doProcessFormInputs
  , setProcessFormInputs
  , getMaximumFormInputSize
  , setMaximumFormInputSize
  , getMaximumNumberOfFormInputs
  , setMaximumNumberOfFormInputs
  , getMinimumUploadRate
  , setMinimumUploadRate
  , getMinimumUploadSeconds
  , setMinimumUploadSeconds
  , getUploadTimeout
  , setUploadTimeout

    -- *** File upload policy
  , FileUploadPolicy(..)
  , defaultFileUploadPolicy
  , setMaximumFileSize
  , setMaximumNumberOfFiles
  , setSkipFilesWithoutNames
  , setMaximumSkippedFileSize

    -- *** Per-file upload policy
  , PartUploadPolicy(..)
  , disallow
  , allowWithMaximumSize

    -- * Exceptions
  , FileUploadException(..)
  , fileUploadExceptionReason
  , BadPartException(..)
  , PolicyViolationException(..)
  ) where

------------------------------------------------------------------------------
import           Control.Applicative              (Alternative ((<|>)), Applicative (pure, (*>), (<*)))
import           Control.Arrow                    (Arrow (first))
import           Control.Exception.Lifted         (Exception, SomeException (..), bracket, catch, finally, fromException, mask, throwIO, toException)
import qualified Control.Exception.Lifted         as E (try)
import           Control.Monad                    (Functor (fmap), Monad (return, (>>=)), MonadPlus (mzero), forM_, guard, liftM, sequence, unless, void, when, (>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Data.Attoparsec.ByteString.Char8 (Parser, isEndOfLine, string, takeWhile)
import qualified Data.Attoparsec.ByteString.Char8 as Atto (try)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as S
import           Data.ByteString.Internal         (c2w)
import qualified Data.ByteString.Lazy.Internal    as LB (ByteString (Empty), chunk)
import qualified Data.CaseInsensitive             as CI (mk)
import           Data.Int                         (Int, Int64)
import qualified Data.IORef                       as IORef
import           Data.List                        (find, map, (++))
import qualified Data.Map                         as Map (insertWith)
import           Data.Maybe                       (Maybe (..), fromMaybe, isJust, maybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as T (concat, pack, unpack)
import qualified Data.Text.Encoding               as TE (decodeUtf8)
import           Data.Typeable                    (Typeable, cast)
import           Prelude                          (Bool (..), Double, Either (..), Eq (..), FilePath, IO, Ord (..), Show (..), String, const, either, foldr, fst, id, max, not, otherwise, seq, snd, succ, ($), ($!), (.), (^), (||))
import           Snap.Core                        (HasHeaders (headers), Headers, MonadSnap, Request (rqParams, rqPostParams), getHeader, getRequest, getTimeoutModifier, putRequest, runRequestBody)
import           Snap.Internal.Parsing            (crlf, fullyParse, pContentTypeWithParameters, pHeaders, pValueWithParameters')
import qualified Snap.Types.Headers               as H (fromList)
import           System.Directory                 (removeFile)
import           System.FilePath                  ((</>))
import           System.IO                        (BufferMode (NoBuffering), Handle, hClose, hSetBuffering, openBinaryTempFile)
import           System.IO.Error                  (isDoesNotExistError)
import           System.IO.Streams                (InputStream, MatchInfo (..), TooManyBytesReadException, search)
import qualified System.IO.Streams                as Streams
import           System.IO.Streams.Attoparsec     (parseFromStream)
import           System.PosixCompat.Temp          (mkstemp)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Reads uploaded files into a temporary directory and calls a user handler
-- to process them.
--
-- Note: /THE REQUEST MUST BE CORRECTLY ENCODED/. If the request's
-- @Content-type@ is not \"@multipart/formdata@\", this function skips
-- processing using 'pass'.
--
-- Given a temporary directory, global and file-specific upload policies, and a
-- user handler, this function consumes a request body uploaded with
-- @Content-type: multipart/form-data@. Each file is read into the temporary
-- directory, and is then passed to the user handler. After the user handler
-- runs (but before the 'Response' body is streamed to the client), the files
-- are deleted from disk; so if you want to retain or use the uploaded files in
-- the generated response, you need to move or otherwise process them.
--
-- The argument passed to the user handler is a tuple:
--
-- > (PartInfo, Either PolicyViolationException FilePath)
--
-- The first half of this tuple is a 'PartInfo', which contains the
-- information the client browser sent about the given upload part (like
-- filename, content-type, etc). The second half of this tuple is an 'Either'
-- stipulating that either:
--
-- 1. the file was rejected on a policy basis because of the provided
--    'PartUploadPolicy' handler
--
-- 2. the file was accepted and exists at the given path.
--
-- /Exceptions/
--
-- If the client's upload rate passes below the configured minimum (see
-- 'setMinimumUploadRate' and 'setMinimumUploadSeconds'), this function
-- terminates the connection. This setting is there to protect the server
-- against slowloris-style denial of service attacks.
--
-- If the given 'UploadPolicy' stipulates that you wish form inputs to be
-- placed in the 'rqParams' parameter map (using 'setProcessFormInputs'), and
-- a form input exceeds the maximum allowable size, this function will throw a
-- 'PolicyViolationException'.
--
-- If an uploaded part contains MIME headers longer than a fixed internal
-- threshold (currently 32KB), this function will throw a 'BadPartException'.

handleFileUploads ::
       (MonadSnap m) =>
       FilePath                       -- ^ temporary directory
    -> UploadPolicy                   -- ^ general upload policy
    -> (PartInfo -> PartUploadPolicy) -- ^ per-part upload policy
    -> (PartInfo -> Either PolicyViolationException FilePath -> IO a)
                                      -- ^ user handler (see function
                                      -- description)
    -> m [a]
handleFileUploads tmpdir uploadPolicy partPolicy partHandler =
    handleMultipart uploadPolicy go

  where
    go partInfo stream = maybe disallowed takeIt mbFs
      where
        ctText = partContentType partInfo
        fnText = fromMaybe "" $ partFileName partInfo

        ct = TE.decodeUtf8 ctText
        fn = TE.decodeUtf8 fnText

        (PartUploadPolicy mbFs) = partPolicy partInfo

        takeIt maxSize = do
            str' <- Streams.throwIfProducesMoreThan maxSize stream
            fileReader tmpdir partHandler partInfo str' `catch` tooMany maxSize

        tooMany maxSize (_ :: TooManyBytesReadException) =
            partHandler partInfo
                        (Left $
                         PolicyViolationException $
                         T.concat [ "File \""
                                  , fn
                                  , "\" exceeded maximum allowable size "
                                  , T.pack $ show maxSize ])

        disallowed =
            partHandler partInfo
                        (Left $
                         PolicyViolationException $
                         T.concat [ "Policy disallowed upload of file \""
                                  , fn
                                  , "\" with content-type \""
                                  , ct
                                  , "\"" ] )


------------------------------------------------------------------------------
-- | Contents of form field of type @file@
data FormFile a = FormFile
    { formFileName  :: !ByteString
         -- ^ Name of a field
    , formFileValue :: a
         -- ^ Result of storing file
    } deriving (Eq, Ord, Show)

data UploadState a = UploadState
     { numUploadedFiles :: !Int
     , uploadedFiles :: !([FormFile a] -> [FormFile a])
     }

-- | Processes form data and calls provided storage function on
-- file parts.
--
-- You can use this together with 'withTemporaryStore', 'storeAsLazyByteString'
-- or provide your own callback to store uploaded files.
--
-- If you need to process uploaded file mime type or file name, do it in the
-- store callback function.
--
-- See also 'foldMultipart'.
--
-- Example using with small files which can safely be stored in memory.
--
-- @
--
-- import qualified Data.ByteString.Lazy as Lazy
--
-- handleSmallFiles :: MonadSnap m => [(ByteString, ByteString, Lazy.ByteString)]
-- handleSmallFiles = handleFormUploads uploadPolicy filePolicy store
--
--   where
--     uploadPolicy = defaultUploadPolicy
--     filePolicy = setMaximumFileSize (64*1024)
--                  $ setMaximumNumberOfFiles 5
--                    defaultUploadPolicy
--     store partInfo stream = do
--        content <- storeAsLazyByteString partInfo stream
--        let
--          fileName = partFileName partInfo
--          fileMime = partContentType partInfo
--        in (fileName, fileMime, content)
-- @
--
handleFormUploads ::
       (MonadSnap m) =>
       UploadPolicy                   -- ^ general upload policy
    -> FileUploadPolicy               -- ^ Upload policy for files
    -> (PartInfo -> InputStream ByteString -> IO a)
                                      -- ^ A file storage function
    -> m ([FormParam], [FormFile a])
handleFormUploads uploadPolicy filePolicy partHandler = do
    (params, !st) <- foldMultipart uploadPolicy go (UploadState 0 id)
    return (params, uploadedFiles st [])
  where
    go !partInfo stream !st = do
        when (numUploads >= maxFiles) throwTooManyFiles

        case partFileName partInfo of
          Nothing -> onEmptyName
          Just _ -> takeIt

      where
        numUploads = numUploadedFiles st
        files = uploadedFiles st
        maxFiles = maxNumberOfFiles filePolicy
        maxFileSize = maxFileUploadSize filePolicy
        fnText = fromMaybe "" $ partFileName partInfo

        fn = TE.decodeUtf8 fnText

        takeIt = do
            str' <- Streams.throwIfProducesMoreThan maxFileSize stream
            r <- partHandler partInfo str' `catch` tooMany maxFileSize
            let f = FormFile (partFieldName partInfo) r
            return $! UploadState (succ numUploads) (files . ([f] ++) )

        skipIt maxSize = do
            str' <- Streams.throwIfProducesMoreThan maxSize stream
            !_ <- Streams.skipToEof str' `catch` tooMany maxSize
            return $! UploadState (succ numUploads) files

        onEmptyName = if skipEmptyFileName filePolicy
                      then skipIt (maxEmptyFileNameSize filePolicy)
                      else takeIt


        throwTooManyFiles = throwIO . PolicyViolationException $ T.concat
                            ["number of files exceeded the maximum of "
                            ,T.pack (show maxFiles) ]

        tooMany maxSize (_ :: TooManyBytesReadException) =
            throwIO . PolicyViolationException $
                    T.concat [ "File \""
                             , fn
                             , "\" exceeded maximum allowable size "
                             , T.pack $ show maxSize ]


------------------------------------------------------------------------------
-- | A type alias for a function that will process one of the parts of a
-- @multipart/form-data@ HTTP request body with accumulator.
type PartFold a = PartInfo -> InputStream ByteString -> a -> IO a


------------------------------------------------------------------------------
-- | Given an upload policy and a function to consume uploaded \"parts\",
-- consume a request body uploaded with @Content-type: multipart/form-data@.
--
-- If 'setProcessFormInputs' is 'True', then parts with disposition @form-data@
-- (a form parameter) will be processed and returned as first element of
-- resulting pair. Parts with other disposition will be fed to 'PartFold'
-- handler.
--
-- If 'setProcessFormInputs' is 'False', then parts with any disposition will
-- be fed to 'PartFold' handler and first element of returned pair will be
-- empty. In this case it is important that you limit number of form inputs
-- and sizes of inputs in your 'PartFold' handler to avoid common DOS attacks.
--
-- Note: /THE REQUEST MUST BE CORRECTLY ENCODED/. If the request's
-- @Content-type@ is not \"@multipart/formdata@\", this function skips
-- processing using 'pass'.
--
-- Most users will opt for the higher-level 'handleFileUploads', which writes
-- to temporary files, rather than 'handleMultipart'. This function should be
-- chosen, however, if you need to stream uploaded files directly to your own
-- processing function: e.g. to a database or a remote service via RPC.
--
-- If the client's upload rate passes below the configured minimum (see
-- 'setMinimumUploadRate' and 'setMinimumUploadSeconds'), this function
-- terminates the connection. This setting is there to protect the server
-- against slowloris-style denial of service attacks.
--
-- /Exceptions/
--
-- If the given 'UploadPolicy' stipulates that you wish form inputs to be
-- processed (using 'setProcessFormInputs'), and a form input exceeds the
-- maximum allowable size or the form exceeds maximum number of inputs, this
-- function will throw a 'PolicyViolationException'.
--
-- If an uploaded part contains MIME headers longer than a fixed internal
-- threshold (currently 32KB), this function will throw a 'BadPartException'.
--
-- /Since: 1.0.3.0/
foldMultipart ::
       (MonadSnap m) =>
       UploadPolicy        -- ^ global upload policy
    -> PartFold a          -- ^ part processor
    -> a                   -- ^ seed accumulator
    -> m ([FormParam], a)
foldMultipart uploadPolicy origPartHandler zero = do
    hdrs <- liftM headers getRequest
    let (ct, mbBoundary) = getContentType hdrs

    tickleTimeout <- liftM (. max) getTimeoutModifier
    let bumpTimeout = tickleTimeout $ uploadTimeout uploadPolicy

    let partHandler = if doProcessFormInputs uploadPolicy
                        then captureVariableOrReadFile
                                 (getMaximumFormInputSize uploadPolicy)
                                 origPartHandler
                        else \x y acc -> liftM File $ origPartHandler x y acc

    -- not well-formed multipart? bomb out.
    guard (ct == "multipart/form-data")

    boundary <- maybe (throwIO $ BadPartException
                       "got multipart/form-data without boundary")
                      return
                      mbBoundary

    -- RateTooSlowException will be caught and properly dealt with by
    -- runRequestBody
    runRequestBody (proc bumpTimeout boundary partHandler)

  where
    --------------------------------------------------------------------------
    uploadRate  = minimumUploadRate uploadPolicy
    uploadSecs  = minimumUploadSeconds uploadPolicy
    maxFormVars = maximumNumberOfFormInputs uploadPolicy

    --------------------------------------------------------------------------
    proc bumpTimeout boundary partHandler =
        Streams.throwIfTooSlow bumpTimeout uploadRate uploadSecs >=>
        internalFoldMultipart maxFormVars boundary partHandler zero

------------------------------------------------------------------------------
-- | A type alias for a function that will process one of the parts of a
-- @multipart/form-data@ HTTP request body without usinc accumulator.
type PartProcessor a = PartInfo -> InputStream ByteString -> IO a


------------------------------------------------------------------------------
-- | A variant of 'foldMultipart' accumulating results into a list.
-- Also puts captured 'FormParam's into rqPostParams and rqParams maps.
--
handleMultipart ::
       (MonadSnap m) =>
       UploadPolicy        -- ^ global upload policy
    -> PartProcessor a     -- ^ part processor
    -> m [a]
handleMultipart uploadPolicy origPartHandler = do
    (captures, files) <- foldMultipart uploadPolicy partFold id
    procCaptures captures
    return $! files []

  where
    partFold info input acc = do
      x <- origPartHandler info input
      return $ acc . ([x]++)
    --------------------------------------------------------------------------
    procCaptures []          = pure ()
    procCaptures params = do
        rq <- getRequest
        putRequest $ modifyParams (\m -> foldr ins m params) rq

    --------------------------------------------------------------------------
    ins (!k, !v) = Map.insertWith (\_ ex -> (v:ex)) k [v]
         -- prepend value if key exists, since we are folding from right

    --------------------------------------------------------------------------
    modifyParams f r = r { rqPostParams = f $ rqPostParams r
                         , rqParams     = f $ rqParams r
                         }

------------------------------------------------------------------------------
-- | Represents the disposition type specified via the @Content-Disposition@
-- header field. See <https://www.ietf.org/rfc/rfc1806.txt RFC 1806>.
data PartDisposition =
    DispositionAttachment       -- ^ @Content-Disposition: attachment@.
  | DispositionFile             -- ^ @Content-Disposition: file@.
  | DispositionFormData         -- ^ @Content-Disposition: form-data@.
  | DispositionOther ByteString -- ^ Any other value.
  deriving (Eq, Show)


------------------------------------------------------------------------------
-- | 'PartInfo' contains information about a \"part\" in a request uploaded
-- with @Content-type: multipart/form-data@.
data PartInfo =
  PartInfo
  { partFieldName   :: !ByteString
    -- ^ Field name associated with this part (i.e., the name specified with
    -- @\<input name=\"partFieldName\" ...@).
  , partFileName    :: !(Maybe ByteString)
    -- ^ Name of the uploaded file.
  , partContentType :: !ByteString
    -- ^ Content type of this part.
  , partDisposition :: !PartDisposition
    -- ^ Disposition type of this part. See 'PartDisposition'.
  , partHeaders     :: !Headers
    -- ^ Remaining headers associated with this part.
  }
  deriving (Show)


------------------------------------------------------------------------------
toPartDisposition :: ByteString -> PartDisposition
toPartDisposition s | s == "attachment" = DispositionAttachment
                    | s == "file"       = DispositionFile
                    | s == "form-data"  = DispositionFormData
                    | otherwise         = DispositionOther s


------------------------------------------------------------------------------
-- | All of the exceptions defined in this package inherit from
-- 'FileUploadException', so if you write
--
-- > foo `catch` \(e :: FileUploadException) -> ...
--
-- you can catch a 'BadPartException', a 'PolicyViolationException', etc.
data FileUploadException = forall e . (ExceptionWithReason e, Show e) =>
                           WrappedFileUploadException e
  deriving (Typeable)


------------------------------------------------------------------------------
class Exception e => ExceptionWithReason e where
    exceptionReason :: e -> Text


------------------------------------------------------------------------------
instance Show FileUploadException where
    show (WrappedFileUploadException e) = show e


------------------------------------------------------------------------------
instance Exception FileUploadException


------------------------------------------------------------------------------
-- | Human-readable error message corresponding to the 'FileUploadException'.
fileUploadExceptionReason :: FileUploadException -> Text
fileUploadExceptionReason (WrappedFileUploadException e) = exceptionReason e


------------------------------------------------------------------------------
uploadExceptionToException :: ExceptionWithReason e => e -> SomeException
uploadExceptionToException = toException . WrappedFileUploadException


------------------------------------------------------------------------------
uploadExceptionFromException :: ExceptionWithReason e => SomeException -> Maybe e
uploadExceptionFromException x = do
    WrappedFileUploadException e <- fromException x
    cast e


------------------------------------------------------------------------------
-- | Thrown when a part is invalid in some way (e.g. the headers are too large).
data BadPartException = BadPartException {
  -- | Human-readable error message corresponding to the 'BadPartException'.
  badPartExceptionReason :: Text
  }
  deriving (Typeable)

instance Exception BadPartException where
    toException = uploadExceptionToException
    fromException = uploadExceptionFromException

instance ExceptionWithReason BadPartException where
    exceptionReason (BadPartException e) = T.concat ["Bad part: ", e]

instance Show BadPartException where
  show = T.unpack . exceptionReason


------------------------------------------------------------------------------
-- | Thrown when an 'UploadPolicy' or 'PartUploadPolicy' is violated.
data PolicyViolationException = PolicyViolationException {
      -- | Human-readable error message corresponding to the
      -- 'PolicyViolationException'.
      policyViolationExceptionReason :: Text
    } deriving (Typeable)

instance Exception PolicyViolationException where
    toException e@(PolicyViolationException _) =
        uploadExceptionToException e
    fromException = uploadExceptionFromException

instance ExceptionWithReason PolicyViolationException where
    exceptionReason (PolicyViolationException r) =
        T.concat ["File upload policy violation: ", r]

instance Show PolicyViolationException where
  show (PolicyViolationException s) = "File upload policy violation: "
                                            ++ T.unpack s


------------------------------------------------------------------------------
-- | 'UploadPolicy' controls overall policy decisions relating to
-- @multipart/form-data@ uploads, specifically:
--
-- * whether to treat parts without filenames as form input (reading them into
--   the 'rqParams' map)
--
-- * because form input is read into memory, the maximum size of a form input
--   read in this manner, and the maximum number of form inputs
--
-- * the minimum upload rate a client must maintain before we kill the
--   connection; if very low-bitrate uploads were allowed then a Snap server
--   would be vulnerable to a trivial denial-of-service using a
--   \"slowloris\"-type attack
--
-- * the minimum number of seconds which must elapse before we start killing
--   uploads for having too low an upload rate.
--
-- * the amount of time we should wait before timing out the connection
--   whenever we receive input from the client.
data UploadPolicy = UploadPolicy {
      processFormInputs         :: Bool
    , maximumFormInputSize      :: Int64
    , maximumNumberOfFormInputs :: Int
    , minimumUploadRate         :: Double
    , minimumUploadSeconds      :: Int
    , uploadTimeout             :: Int
}


------------------------------------------------------------------------------
-- | A reasonable set of defaults for upload policy. The default policy is:
--
--   [@maximum form input size@]                128kB
--
--   [@maximum number of form inputs@]          10
--
--   [@minimum upload rate@]                    1kB/s
--
--   [@seconds before rate limiting kicks in@]  10
--
--   [@inactivity timeout@]                     20 seconds
--
defaultUploadPolicy :: UploadPolicy
defaultUploadPolicy = UploadPolicy True maxSize maxNum minRate minSeconds tout
  where
    maxSize    = 2^(17::Int)
    maxNum     = 10
    minRate    = 1000
    minSeconds = 10
    tout       = 20


------------------------------------------------------------------------------
-- | Does this upload policy stipulate that we want to treat parts without
-- filenames as form input?
doProcessFormInputs :: UploadPolicy -> Bool
doProcessFormInputs = processFormInputs


------------------------------------------------------------------------------
-- | Set the upload policy for treating parts without filenames as form input.
setProcessFormInputs :: Bool -> UploadPolicy -> UploadPolicy
setProcessFormInputs b u = u { processFormInputs = b }


------------------------------------------------------------------------------
-- | Get the maximum size of a form input which will be read into our
--   'rqParams' map.
getMaximumFormInputSize :: UploadPolicy -> Int64
getMaximumFormInputSize = maximumFormInputSize


------------------------------------------------------------------------------
-- | Set the maximum size of a form input which will be read into our
--   'rqParams' map.
setMaximumFormInputSize :: Int64 -> UploadPolicy -> UploadPolicy
setMaximumFormInputSize s u = u { maximumFormInputSize = s }


------------------------------------------------------------------------------
-- | Get the maximum size of a form input which will be read into our
--   'rqParams' map.
getMaximumNumberOfFormInputs :: UploadPolicy -> Int
getMaximumNumberOfFormInputs = maximumNumberOfFormInputs


------------------------------------------------------------------------------
-- | Set the maximum size of a form input which will be read into our
--   'rqParams' map.
setMaximumNumberOfFormInputs :: Int -> UploadPolicy -> UploadPolicy
setMaximumNumberOfFormInputs s u = u { maximumNumberOfFormInputs = s }


------------------------------------------------------------------------------
-- | Get the minimum rate (in /bytes\/second/) a client must maintain before
--   we kill the connection.
getMinimumUploadRate :: UploadPolicy -> Double
getMinimumUploadRate = minimumUploadRate


------------------------------------------------------------------------------
-- | Set the minimum rate (in /bytes\/second/) a client must maintain before
--   we kill the connection.
setMinimumUploadRate :: Double -> UploadPolicy -> UploadPolicy
setMinimumUploadRate s u = u { minimumUploadRate = s }


------------------------------------------------------------------------------
-- | Get the amount of time which must elapse before we begin enforcing the
--   upload rate minimum
getMinimumUploadSeconds :: UploadPolicy -> Int
getMinimumUploadSeconds = minimumUploadSeconds


------------------------------------------------------------------------------
-- | Set the amount of time which must elapse before we begin enforcing the
--   upload rate minimum
setMinimumUploadSeconds :: Int -> UploadPolicy -> UploadPolicy
setMinimumUploadSeconds s u = u { minimumUploadSeconds = s }


------------------------------------------------------------------------------
-- | Get the \"upload timeout\". Whenever input is received from the client,
--   the connection timeout is set this many seconds in the future.
getUploadTimeout :: UploadPolicy -> Int
getUploadTimeout = uploadTimeout


------------------------------------------------------------------------------
-- | Set the upload timeout.
setUploadTimeout :: Int -> UploadPolicy -> UploadPolicy
setUploadTimeout s u = u { uploadTimeout = s }


------------------------------------------------------------------------------

-- | File upload policy, if any policy is violated then
-- 'PolicyViolationException' is thrown
data FileUploadPolicy = FileUploadPolicy
    { maxFileUploadSize    :: !Int64
    , maxNumberOfFiles     :: !Int
    , skipEmptyFileName    :: !Bool
    , maxEmptyFileNameSize :: !Int64
    }

-- | A default 'FileUploadPolicy'
--
--   [@maximum file size@]             1MB
--
--   [@maximum number of files@]       10
--
--   [@skip files without name@]       yes
--
--   [@maximum size of skipped file@]  0
--
--
defaultFileUploadPolicy :: FileUploadPolicy
defaultFileUploadPolicy = FileUploadPolicy maxFileSize maxFiles
                                           skipEmptyName maxEmptySize
  where
    maxFileSize = 1048576 -- 1MB
    maxFiles    = 10
    skipEmptyName = True
    maxEmptySize = 0

-- | Maximum size of single uploaded file.
setMaximumFileSize :: Int64 -> FileUploadPolicy -> FileUploadPolicy
setMaximumFileSize maxSize s =
    s { maxFileUploadSize = maxSize }

-- | Maximum number of uploaded files.
setMaximumNumberOfFiles :: Int -> FileUploadPolicy -> FileUploadPolicy
setMaximumNumberOfFiles maxFiles s =
    s { maxNumberOfFiles = maxFiles }

-- | Skip files with empty file names.
--
-- If set, parts without filenames will not be fed to storage function.
--
-- HTML5 form data encoding standard states that form input fields of type
-- file, without value set, are encoded same way as if file with empty body,
-- empty file name, and type @application/octet-stream@ was set as value.
--
-- You most likely want to use this with zero bytes allowed to avoid storing
-- such fields (see 'setMaximumSkippedFileSize').
--
-- By default files without names are skipped.
--
-- /Since: 1.0.3.0/
setSkipFilesWithoutNames :: Bool -> FileUploadPolicy -> FileUploadPolicy
setSkipFilesWithoutNames shouldSkip s =
    s { skipEmptyFileName = shouldSkip }

-- | Maximum size of file without name which can be skipped.
--
-- Ignored if 'setSkipFilesWithoutNames' is @False@.
--
-- If skipped file is larger than this setting then 'FileUploadException'
-- is thrown.
--
-- By default maximum file size is 0.
--
-- /Since: 1.0.3.0/
setMaximumSkippedFileSize :: Int64 -> FileUploadPolicy -> FileUploadPolicy
setMaximumSkippedFileSize maxSize s =
    s { maxEmptyFileNameSize = maxSize }


------------------------------------------------------------------------------
-- | Upload policy can be set on an \"general\" basis (using 'UploadPolicy'),
--   but handlers can also make policy decisions on individual files\/parts
--   uploaded. For each part uploaded, handlers can decide:
--
-- * whether to allow the file upload at all
--
-- * the maximum size of uploaded files, if allowed
data PartUploadPolicy = PartUploadPolicy (Maybe Int64)


------------------------------------------------------------------------------
-- | Disallows the file to be uploaded.
disallow :: PartUploadPolicy
disallow = PartUploadPolicy Nothing


------------------------------------------------------------------------------
-- | Allows the file to be uploaded, with maximum size /n/ in bytes.
allowWithMaximumSize :: Int64 -> PartUploadPolicy
allowWithMaximumSize = PartUploadPolicy . Just


------------------------------------------------------------------------------
-- | Stores file body in memory as Lazy ByteString.
storeAsLazyByteString :: InputStream ByteString -> IO LB.ByteString
storeAsLazyByteString !str = do
   f <- Streams.fold (\f c -> f . LB.chunk c) id str
   return $! f LB.Empty


------------------------------------------------------------------------------
-- | Store files in a temporary directory, and clean up on function exit.
--
-- Files are safe to move until function exists.
--
-- If asynchronous exception is thrown during cleanup, temporary files may
-- remain.
--
-- @
-- uploadsHandler = withTemporaryStore "/var/tmp" "upload-" $ \store -> do
--     (inputs, files) <- handleFormUploads defaultUploadpolicy
--                                          defaultFileUploadPolicy
--                                          (const store)
--     saveFiles files
--
-- @
--
withTemporaryStore ::
    MonadSnap m
    => FilePath -- ^ temporary directory
    -> String   -- ^ file name pattern
    -> ((InputStream ByteString -> IO FilePath) -> m a)
      -- ^ Action taking store function
    -> m a
withTemporaryStore tempdir pat act = do
    ioref <- liftIO $ IORef.newIORef []
    let
      modifyIORef' ref f = do -- ghc 7.4 does not have modifyIORef'
          x <- IORef.readIORef ref
          let x' = f x
          x' `seq` IORef.writeIORef ref x'

      go input = do
          (fn, h) <- openBinaryTempFile tempdir pat
          modifyIORef' ioref (fn:)
          hSetBuffering h NoBuffering
          output <- Streams.handleToOutputStream h
          Streams.connect input output
          hClose h
          pure fn

      cleanup = liftIO $ do
          files <- IORef.readIORef ioref
          forM_ files $ \fn ->
             removeFile fn `catch` handleExists
      handleExists e = unless (isDoesNotExistError e) $ throwIO e

    act go `finally` cleanup


------------------------------------------------------------------------------
-- private exports follow. FIXME: organize
------------------------------------------------------------------------------

------------------------------------------------------------------------------
captureVariableOrReadFile ::
       Int64                                   -- ^ maximum size of form input
    -> PartFold a                              -- ^ file reading code
    -> PartInfo -> InputStream ByteString
    -> a
    -> IO (Capture a)
captureVariableOrReadFile maxSize fileHandler partInfo stream acc =
    if isFile
      then liftM File $ fileHandler partInfo stream acc
      else variable `catch` handler

  where
    isFile = isJust (partFileName partInfo) ||
             partDisposition partInfo == DispositionFile

    variable = do
        !x <- liftM S.concat $
             Streams.throwIfProducesMoreThan maxSize stream >>= Streams.toList
        return $! Capture fieldName x

    fieldName = partFieldName partInfo

    handler (_ :: TooManyBytesReadException) =
        throwIO $ PolicyViolationException $
                T.concat [ "form input '"
                         , TE.decodeUtf8 fieldName
                         , "' exceeded maximum permissible size ("
                         , T.pack $ show maxSize
                         , " bytes)" ]


------------------------------------------------------------------------------
data Capture a = Capture !ByteString !ByteString
               | File a


------------------------------------------------------------------------------
fileReader :: FilePath
           -> (PartInfo -> Either PolicyViolationException FilePath -> IO a)
           -> PartProcessor a
fileReader tmpdir partProc partInfo input =
    withTempFile tmpdir "snap-upload-" $ \(fn, h) -> do
        hSetBuffering h NoBuffering
        output <- Streams.handleToOutputStream h
        Streams.connect input output
        hClose h
        partProc partInfo $ Right fn


------------------------------------------------------------------------------
data MultipartState a = MultipartState
  { numFormVars       :: {-# UNPACK #-} !Int
  , numFormFiles      :: {-# UNPACK #-} !Int
  , capturedFields    :: !([FormParam] -> [FormParam])
  , accumulator       :: !a
  }

------------------------------------------------------------------------------
-- | A form parameter name-value pair
type FormParam = (ByteString, ByteString)

------------------------------------------------------------------------------
addCapture :: ByteString -> ByteString -> MultipartState a -> MultipartState a
addCapture !k !v !ms =
  let !kv = (k,v)
      f = capturedFields ms . ([kv]++)
      !ms' = ms { capturedFields = f
                , numFormVars = succ (numFormVars ms) }
  in ms'


------------------------------------------------------------------------------
internalFoldMultipart ::
       Int           -- ^ max num fields
    -> ByteString                                     -- ^ boundary value
    -> (PartInfo -> InputStream ByteString -> a -> IO (Capture a))  -- ^ part processor
    -> a
    -> InputStream ByteString
    -> IO ([FormParam], a)
internalFoldMultipart !maxFormVars !boundary clientHandler !zeroAcc !stream = go
  where
    --------------------------------------------------------------------------
    initialState = MultipartState 0 0 id zeroAcc

    --------------------------------------------------------------------------
    go = do
        -- swallow the first boundary
        _        <- parseFromStream (parseFirstBoundary boundary) stream
        bmstream <- search (fullBoundary boundary) stream
        ms <- foldParts goPart bmstream initialState
        return $ (capturedFields ms [], accumulator ms)

    --------------------------------------------------------------------------
    pBoundary !b = Atto.try $ do
      _ <- string "--"
      string b

    --------------------------------------------------------------------------
    fullBoundary !b       = S.concat ["\r\n", "--", b]
    pLine                 = takeWhile (not . isEndOfLine . c2w) <* eol
    parseFirstBoundary !b = pBoundary b <|> (pLine *> parseFirstBoundary b)


    --------------------------------------------------------------------------
    takeHeaders !str = hdrs `catch` handler
      where
        hdrs = do
            str' <- Streams.throwIfProducesMoreThan mAX_HDRS_SIZE str
            liftM toHeaders $ parseFromStream pHeadersWithSeparator str'

        handler (_ :: TooManyBytesReadException) =
            throwIO $ BadPartException "headers exceeded maximum size"

    --------------------------------------------------------------------------
    goPart !str !state = do
        hdrs <- takeHeaders str

        -- are we using mixed?
        let (contentType, mboundary) = getContentType hdrs
        let (fieldName, fileName, disposition) = getFieldHeaderInfo hdrs

        if contentType == "multipart/mixed"
          then maybe (throwIO $ BadPartException $
                      "got multipart/mixed without boundary")
                     (processMixed fieldName str state)
                     mboundary
          else do
              let info = PartInfo fieldName fileName contentType disposition hdrs
              handlePart info str state

    --------------------------------------------------------------------------
    handlePart !info !str !ms = do
      r <- clientHandler info str (accumulator ms)
      case r of
        Capture !k !v -> do
           when (maxFormVars <= numFormVars ms) throwTooMuchVars
           return $! addCapture k v ms
        File !newAcc -> return $! ms { accumulator = newAcc
                                     , numFormFiles = succ (numFormFiles ms)
                                     }

    throwTooMuchVars =
        throwIO . PolicyViolationException
        $ T.concat [ "number of form inputs exceeded maximum of "
                   , T.pack $ show maxFormVars ]

    --------------------------------------------------------------------------
    processMixed !fieldName !str !state !mixedBoundary = do
        -- swallow the first boundary
        _  <- parseFromStream (parseFirstBoundary mixedBoundary) str
        bm <- search (fullBoundary mixedBoundary) str
        foldParts (mixedStream fieldName) bm state


    --------------------------------------------------------------------------
    mixedStream !fieldName !str !acc = do
        hdrs <- takeHeaders str

        let (contentType, _)           = getContentType hdrs
        let (_, fileName, disposition) = getFieldHeaderInfo hdrs

        let info = PartInfo fieldName fileName contentType disposition hdrs
        handlePart info str acc


------------------------------------------------------------------------------
getContentType :: Headers
               -> (ByteString, Maybe ByteString)
getContentType hdrs = (contentType, boundary)
  where
    contentTypeValue = fromMaybe "text/plain" $
                       getHeader "content-type" hdrs

    eCT = fullyParse contentTypeValue pContentTypeWithParameters
    (!contentType, !params) = either (const ("text/plain", [])) id eCT

    boundary = findParam "boundary" params


------------------------------------------------------------------------------
getFieldHeaderInfo :: Headers -> (ByteString, Maybe ByteString, PartDisposition)
getFieldHeaderInfo hdrs = (fieldName, fileName, disposition)
  where
    contentDispositionValue = fromMaybe "unknown" $
                              getHeader "content-disposition" hdrs

    eDisposition = fullyParse contentDispositionValue $ pValueWithParameters' (const True)

    (!dispositionType, dispositionParameters) =
        either (const ("unknown", [])) id eDisposition

    disposition = toPartDisposition dispositionType

    fieldName = fromMaybe "" $ findParam "name" dispositionParameters

    fileName = findParam "filename" dispositionParameters


------------------------------------------------------------------------------
findParam :: (Eq a) => a -> [(a, b)] -> Maybe b
findParam p = fmap snd . find ((== p) . fst)


------------------------------------------------------------------------------
partStream :: InputStream MatchInfo -> IO (InputStream ByteString)
partStream st = Streams.makeInputStream go

  where
    go = do
        s <- Streams.read st
        return $! s >>= f

    f (NoMatch s) = return s
    f _           = mzero




------------------------------------------------------------------------------
-- | Assuming we've already identified the boundary value and split the input
-- up into parts which match and parts which don't, run the given 'ByteString'
-- InputStream over each part and grab a list of the resulting values.
--
-- TODO/FIXME: fix description
foldParts :: (InputStream ByteString -> MultipartState a -> IO (MultipartState a))
             -> InputStream MatchInfo
             -> (MultipartState a)
             -> IO (MultipartState a)
foldParts partFunc stream = go
  where
    part acc pStream = do
        isLast <- parseFromStream pBoundaryEnd pStream

        if isLast
          then return Nothing
          else do
              !x <- partFunc pStream acc
              Streams.skipToEof pStream
              return $! Just x

    go !acc = do
      cap <- partStream stream >>= part acc
      maybe (return acc) go cap

    pBoundaryEnd = (eol *> pure False) <|> (string "--" *> pure True)


------------------------------------------------------------------------------
eol :: Parser ByteString
eol = (string "\n") <|> (string "\r\n")


------------------------------------------------------------------------------
pHeadersWithSeparator :: Parser [(ByteString,ByteString)]
pHeadersWithSeparator = pHeaders <* crlf


------------------------------------------------------------------------------
toHeaders :: [(ByteString,ByteString)] -> Headers
toHeaders kvps = H.fromList kvps'
  where
    kvps'     = map (first CI.mk) kvps


------------------------------------------------------------------------------
mAX_HDRS_SIZE :: Int64
mAX_HDRS_SIZE = 32768


------------------------------------------------------------------------------
withTempFile :: FilePath
             -> String
             -> ((FilePath, Handle) -> IO a)
             -> IO a
withTempFile tmpl temp handler =
    mask $ \restore -> bracket make cleanup (restore . handler)

  where
    make           = mkstemp $ tmpl </> (temp ++ "XXXXXXX")
    cleanup (fp,h) = sequence $ map gobble [hClose h, removeFile fp]

    t :: IO z -> IO (Either SomeException z)
    t = E.try

    gobble = void . t
