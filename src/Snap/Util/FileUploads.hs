{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

------------------------------------------------------------------------------
-- | This module contains primitives and helper functions for handling
-- requests with @Content-type: multipart/form-data@, i.e. HTML forms and file
-- uploads.
--
-- Typically most users will want to use 'handleFileUploads', which writes
-- uploaded files to a temporary directory before sending them on to a handler
-- specified by the user.
--
-- Users who wish to handle their file uploads differently can use the
-- lower-level streaming 'Iteratee' interface called 'handleMultipart'. That
-- function takes uploaded files and streams them to an 'Iteratee' consumer of
-- the user's choosing.
--
-- Using these functions requires making \"policy\" decisions which Snap can't
-- really make for users, such as \"what's the largest PDF file a user is
-- allowed to upload?\" and \"should we read form inputs into the parameters
-- mapping?\". Policy is specified on a \"global\" basis (using
-- 'UploadPolicy'), and on a per-file basis (using 'PartUploadPolicy', which
-- allows you to reject or limit the size of certain uploaded
-- @Content-type@s).
module Snap.Util.FileUploads
  ( -- * Functions
    handleFileUploads
  , handleMultipart
  , PartProcessor

    -- * Uploaded parts
  , PartInfo(..)

    -- ** Policy
    -- *** General upload policy
  , UploadPolicy
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

    -- *** Per-file upload policy
  , PartUploadPolicy
  , disallow
  , allowWithMaximumSize

    -- * Exceptions
  , FileUploadException
  , fileUploadExceptionReason
  , BadPartException
  , badPartExceptionReason
  , PolicyViolationException
  , policyViolationExceptionReason
  ) where

------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Applicative
import           Control.Exception (SomeException(..))
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.CatchIO
import qualified Data.Attoparsec.Char8 as Atto
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Internal (c2w)
import qualified Data.CaseInsensitive as CI
import           Data.Int
import           Data.List hiding (takeWhile)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Data.Typeable
#if MIN_VERSION_base(4,6,0)
import           Prelude hiding (getLine, takeWhile)
#else
import           Prelude hiding (catch, getLine, takeWhile)
#endif
import           System.Directory
import           System.IO hiding (isEOF)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Internal as StreamsI
import           System.IO.Streams ( InputStream
                                   , MatchInfo(..)
                                   , search
                                   , RateTooSlowException
                                   , TooManyBytesReadException )
import           System.IO.Streams.Attoparsec
import           System.FilePath ((</>))
import           System.PosixCompat.Temp (mkstemp)
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Internal.Parsing
import qualified Snap.Types.Headers as H


------------------------------------------------------------------------------
-- | Reads uploaded files into a temporary directory and calls a user handler
-- to process them.
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
-- If the request's @Content-type@ was not \"@multipart/formdata@\", this
-- function skips processing using 'pass'.
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
            fileReader tmpdir partHandler partInfo str' `catches`
                       [ Handler $ tooMany maxSize
                       , Handler $ policyViolation ]

        tooMany maxSize (_ :: TooManyBytesReadException) =
            partHandler partInfo
                        (Left $
                         PolicyViolationException $
                         T.concat [ "File \""
                                  , fn
                                  , "\" exceeded maximum allowable size "
                                  , T.pack $ show maxSize ])

        policyViolation e = partHandler partInfo $ Left e

        disallowed =
            partHandler partInfo
                        (Left $
                         PolicyViolationException $
                         T.concat [ "Policy disallowed upload of file \""
                                  , fn
                                  , "\" with content-type \""
                                  , ct
                                  , "\"" ] )


{-
handleFileUploads ::
       (MonadSnap m) =>
       FilePath                       -- ^ temporary directory
    -> UploadPolicy                   -- ^ general upload policy
    -> (PartInfo -> PartUploadPolicy) -- ^ per-part upload policy
    -> ([(PartInfo, Either PolicyViolationException FilePath)] -> m a)
                                      -- ^ user handler (see function
                                      -- description)
    -> m a
handleFileUploads tmpdir uploadPolicy partPolicy handler = do
    uploadedFiles <- newUploadedFiles

    (do
        xs <- handleMultipart uploadPolicy (iter uploadedFiles)
        handler xs
        ) `finally` (cleanupUploadedFiles uploadedFiles)

  where
    iter uploadedFiles partInfo = maybe disallowed takeIt mbFs
      where
        ctText = partContentType partInfo
        fnText = fromMaybe "" $ partFileName partInfo

        ct = TE.decodeUtf8 ctText
        fn = TE.decodeUtf8 fnText

        (PartUploadPolicy mbFs) = partPolicy partInfo

        retVal (_,x) = (partInfo, Right x)

        takeIt maxSize = do
            debug "handleFileUploads/takeIt: begin"
            let it = fmap retVal $
                     joinI' $
                     iterateeDebugWrapper "takeNoMoreThan" $
                     takeNoMoreThan maxSize $$
                     fileReader uploadedFiles tmpdir partInfo

            it `catches` [
                    Handler $ \(_ :: TooManyBytesReadException) -> do
                        debug $ "handleFileUploads/iter: " ++
                                "caught TooManyBytesReadException"
                        skipToEof
                        tooMany maxSize
                  , Handler $ \(e :: SomeException) -> do
                        debug $ "handleFileUploads/iter: caught " ++ show e
                        debug "handleFileUploads/iter: rethrowing"
                        throw e
                  ]

        tooMany maxSize =
            return ( partInfo
                   , Left $ PolicyViolationException
                          $ T.concat [ "File \""
                                     , fn
                                     , "\" exceeded maximum allowable size "
                                     , T.pack $ show maxSize ] )

        disallowed =
            return ( partInfo
                   , Left $ PolicyViolationException
                          $ T.concat [ "Policy disallowed upload of file \""
                                     , fn
                                     , "\" with content-type \""
                                     , ct
                                     , "\"" ] )

------------------------------------------------------------------------------
-- | Given an upload policy and a function to consume uploaded \"parts\",
-- consume a request body uploaded with @Content-type: multipart/form-data@.
-- Normally most users will want to use 'handleFileUploads' (which writes
-- uploaded files to a temporary directory and passes their names to a given
-- handler) rather than this function; the lower-level 'handleMultipart'
-- function should be used if you want to stream uploaded files to your own
-- iteratee function.
--
-- If the request's @Content-type@ was not \"@multipart/formdata@\", this
-- function skips processing using 'pass'.
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
--
handleMultipart ::
       (MonadSnap m) =>
       UploadPolicy                            -- ^ global upload policy
    -> (PartInfo -> Iteratee ByteString IO a)  -- ^ part processor
    -> m [a]
handleMultipart uploadPolicy origPartHandler = do
    hdrs <- liftM headers getRequest
    let (ct, mbBoundary) = getContentType hdrs

    tickleTimeout <- liftM (. max) getTimeoutModifier
    let bumpTimeout = tickleTimeout $ uploadTimeout uploadPolicy

    let partHandler = if doProcessFormInputs uploadPolicy
                        then captureVariableOrReadFile
                                 (getMaximumFormInputSize uploadPolicy)
                                 origPartHandler
                        else (\p -> fmap File (origPartHandler p))

    -- not well-formed multipart? bomb out.
    when (ct /= "multipart/form-data") $ do
        debug $ "handleMultipart called with content-type=" ++ S.unpack ct
                  ++ ", passing"
        pass

    when (isNothing mbBoundary) $
         throw $ BadPartException $
         "got multipart/form-data without boundary"

    let boundary = fromJust mbBoundary
    captures <- runRequestBody (iter bumpTimeout boundary partHandler)

    xs <- procCaptures [] captures
    modifyRequest $ \req ->
        let pp = rqPostParams req
        in rqModifyParams (\p -> Map.unionWith (++) p pp) req
    return xs

  where
    rateLimit bump m =
        killIfTooSlow bump
            (minimumUploadRate uploadPolicy)
            (minimumUploadSeconds uploadPolicy)
            m
          `catchError` \e -> do
              debug $ "rateLimit: caught " ++ show e
              let (me::Maybe RateTooSlowException) = fromException e
              maybe (throwError e)
                    terminateConnection
                    me

    iter bump boundary ph = iterateeDebugWrapper "killIfTooSlow" $
                            rateLimit bump $
                            internalHandleMultipart boundary ph

    ins k v = Map.insertWith' (flip (++)) k [v]

    maxFormVars = maximumNumberOfFormInputs uploadPolicy

    modifyParams f r = r { rqPostParams = f $ rqPostParams r
                         , rqParams     = f $ rqParams r
                         }

    procCaptures l [] = return $! reverse l
    procCaptures l ((File x):xs) = procCaptures (x:l) xs
    procCaptures l ((Capture k v):xs) = do
        rq <- getRequest
        let n = Map.size $ rqPostParams rq
        when (n >= maxFormVars) $
          throw $ PolicyViolationException $
          T.concat [ "number of form inputs exceeded maximum of "
                   , T.pack $ show maxFormVars ]
        modifyRequest $ modifyParams (ins k v)
        procCaptures l xs

-}

------------------------------------------------------------------------------
-- | A type alias for a function that will process one of the parts of a
-- @multipart/form-data@ HTTP request body.
type PartProcessor a = PartInfo -> InputStream ByteString -> IO a


------------------------------------------------------------------------------
-- | Given an upload policy and a function to consume uploaded \"parts\",
-- consume a request body uploaded with @Content-type: multipart/form-data@.
-- Normally most users will want to use 'handleFileUploads' (which writes
-- uploaded files to a temporary directory and passes their names to a given
-- handler) rather than this function; the lower-level 'handleMultipart'
-- function should be used if you want to stream uploaded files to your own
-- iteratee function.
--
-- If the request's @Content-type@ was not \"@multipart/formdata@\", this
-- function skips processing using 'pass'.
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
--
handleMultipart ::
       (MonadSnap m) =>
       UploadPolicy        -- ^ global upload policy
    -> PartProcessor a     -- ^ part processor
    -> m [a]
handleMultipart uploadPolicy origPartHandler = do
    hdrs <- liftM headers getRequest
    let (ct, mbBoundary) = getContentType hdrs

    tickleTimeout <- liftM (. max) getTimeoutModifier
    let bumpTimeout = tickleTimeout $ uploadTimeout uploadPolicy

    let partHandler = if doProcessFormInputs uploadPolicy
                        then captureVariableOrReadFile
                                 (getMaximumFormInputSize uploadPolicy)
                                 origPartHandler
                        else \x y -> liftM File $ origPartHandler x y

    -- not well-formed multipart? bomb out.
    guard (ct == "multipart/form-data")

    boundary <- maybe (throw $ BadPartException
                       "got multipart/form-data without boundary")
                      return
                      mbBoundary

    captures <- runRequestBody (proc bumpTimeout boundary partHandler)
    procCaptures captures id

  where
    --------------------------------------------------------------------------
    uploadRate  = minimumUploadRate uploadPolicy
    uploadSecs  = minimumUploadSeconds uploadPolicy
    maxFormVars = maximumNumberOfFormInputs uploadPolicy

    --------------------------------------------------------------------------
    proc bumpTimeout boundary partHandler stream = do
        str <- Streams.throwIfTooSlow bumpTimeout uploadRate uploadSecs stream
        internalHandleMultipart boundary partHandler str `catch` errHandler

      where
        errHandler (e :: RateTooSlowException) = terminateConnection e

    --------------------------------------------------------------------------
    procCaptures []                 dl = return $! dl []
    procCaptures ((File x):xs)      dl = procCaptures xs (dl . (x:))
    procCaptures ((Capture k v):xs) dl = do
        rq <- getRequest
        let n = Map.size $ rqPostParams rq
        when (n >= maxFormVars) $
          throw $ PolicyViolationException $
          T.concat [ "number of form inputs exceeded maximum of "
                   , T.pack $ show maxFormVars ]
        putRequest $ modifyParams (ins k v) rq
        procCaptures xs dl

    --------------------------------------------------------------------------
    ins k v = Map.insertWith' (flip (++)) k [v]

    --------------------------------------------------------------------------
    modifyParams f r = r { rqPostParams = f $ rqPostParams r
                         , rqParams     = f $ rqParams r
                         }


------------------------------------------------------------------------------
-- | 'PartInfo' contains information about a \"part\" in a request uploaded
-- with @Content-type: multipart/form-data@.
data PartInfo =
    PartInfo { partFieldName   :: !ByteString
             , partFileName    :: !(Maybe ByteString)
             , partContentType :: !ByteString
             }
  deriving (Show)


------------------------------------------------------------------------------
-- | All of the exceptions defined in this package inherit from
-- 'FileUploadException', so if you write
--
-- > foo `catch` \(e :: FileUploadException) -> ...
--
-- you can catch a 'BadPartException', a 'PolicyViolationException', etc.
data FileUploadException =
    GenericFileUploadException {
      _genericFileUploadExceptionReason :: Text
    }
  | forall e . (Exception e, Show e) =>
    WrappedFileUploadException {
      _wrappedFileUploadException :: e
    , _wrappedFileUploadExceptionReason :: Text
    }
  deriving (Typeable)


------------------------------------------------------------------------------
instance Show FileUploadException where
    show (GenericFileUploadException r) = "File upload exception: " ++
                                          T.unpack r
    show (WrappedFileUploadException e _) = show e


------------------------------------------------------------------------------
instance Exception FileUploadException


------------------------------------------------------------------------------
fileUploadExceptionReason :: FileUploadException -> Text
fileUploadExceptionReason (GenericFileUploadException r) = r
fileUploadExceptionReason (WrappedFileUploadException _ r) = r


------------------------------------------------------------------------------
uploadExceptionToException :: Exception e => e -> Text -> SomeException
uploadExceptionToException e r =
    SomeException $ WrappedFileUploadException e r


------------------------------------------------------------------------------
uploadExceptionFromException :: Exception e => SomeException -> Maybe e
uploadExceptionFromException x = do
    WrappedFileUploadException e _ <- fromException x
    cast e


------------------------------------------------------------------------------
data BadPartException = BadPartException { badPartExceptionReason :: Text }
  deriving (Typeable)

instance Exception BadPartException where
    toException e@(BadPartException r) = uploadExceptionToException e r
    fromException = uploadExceptionFromException

instance Show BadPartException where
  show (BadPartException s) = "Bad part: " ++ T.unpack s


------------------------------------------------------------------------------
data PolicyViolationException = PolicyViolationException {
      policyViolationExceptionReason :: Text
    } deriving (Typeable)

instance Exception PolicyViolationException where
    toException e@(PolicyViolationException r) =
        uploadExceptionToException e r
    fromException = uploadExceptionFromException

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
} deriving (Show, Eq)


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
-- | Upload policy can be set on an \"general\" basis (using 'UploadPolicy'),
--   but handlers can also make policy decisions on individual files\/parts
--   uploaded. For each part uploaded, handlers can decide:
--
-- * whether to allow the file upload at all
--
-- * the maximum size of uploaded files, if allowed
data PartUploadPolicy = PartUploadPolicy {
      _maximumFileSize :: Maybe Int64
} deriving (Show, Eq)


------------------------------------------------------------------------------
-- | Disallows the file to be uploaded.
disallow :: PartUploadPolicy
disallow = PartUploadPolicy Nothing


------------------------------------------------------------------------------
-- | Allows the file to be uploaded, with maximum size /n/.
allowWithMaximumSize :: Int64 -> PartUploadPolicy
allowWithMaximumSize = PartUploadPolicy . Just


------------------------------------------------------------------------------
-- private exports follow. FIXME: organize
------------------------------------------------------------------------------

------------------------------------------------------------------------------
captureVariableOrReadFile ::
       Int64                                   -- ^ maximum size of form input
    -> PartProcessor a                         -- ^ file reading code
    -> PartProcessor (Capture a)
captureVariableOrReadFile maxSize fileHandler partInfo stream =
    case partFileName partInfo of
      Nothing -> variable `catch` handler
      _       -> liftM File $ fileHandler partInfo stream
  where
    variable = do
        x <- liftM S.concat $
             Streams.throwIfProducesMoreThan maxSize stream >>= Streams.toList
        return $! Capture fieldName x

    fieldName = partFieldName partInfo

    handler (e :: SomeException) =
        maybe (throw e)
              (const $ throw $ PolicyViolationException $
                     T.concat [ "form input '"
                              , TE.decodeUtf8 fieldName
                              , "' exceeded maximum permissible size ("
                              , T.pack $ show maxSize
                              , " bytes)" ])
              (fromException e :: Maybe TooManyBytesReadException)


------------------------------------------------------------------------------
data Capture a = Capture ByteString ByteString
               | File a
  deriving (Show)


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
internalHandleMultipart ::
       ByteString                                    -- ^ boundary value
    -> (PartInfo -> InputStream ByteString -> IO a)  -- ^ part processor
    -> InputStream ByteString
    -> IO [a]
internalHandleMultipart boundary clientHandler stream = go
  where
    --------------------------------------------------------------------------
    go = do
        -- swallow the first boundary
        _        <- parseFromStream (parseFirstBoundary boundary) stream
        bmstream <- search (fullBoundary boundary) stream
        liftM concat $ processParts goPart bmstream

    --------------------------------------------------------------------------
    pBoundary b = Atto.try $ do
      _ <- string "--"
      string b

    --------------------------------------------------------------------------
    fullBoundary b       = S.concat ["\r\n", "--", b]
    pLine                = takeWhile (not . isEndOfLine . c2w) <* eol
    takeLine             = pLine *> pure ()
    parseFirstBoundary b = pBoundary b <|> (takeLine *> parseFirstBoundary b)


    --------------------------------------------------------------------------
    takeHeaders str = hdrs `catch` handler
      where
        hdrs = do
            str' <- Streams.throwIfProducesMoreThan mAX_HDRS_SIZE str
            liftM toHeaders $ parseFromStream pHeadersWithSeparator str'

        handler (_ :: TooManyBytesReadException) =
            throw $ BadPartException "headers exceeded maximum size"

    --------------------------------------------------------------------------
    goPart str = do
        hdrs <- takeHeaders str

        -- are we using mixed?
        let (contentType, mboundary) = getContentType hdrs
        let (fieldName, fileName)    = getFieldName hdrs

        if contentType == "multipart/mixed"
          then maybe (throw $ BadPartException $
                      "got multipart/mixed without boundary")
                     (processMixed fieldName str)
                     mboundary
          else do
              let info = PartInfo fieldName fileName contentType
              liftM (:[]) $ clientHandler info str


    --------------------------------------------------------------------------
    processMixed fieldName str mixedBoundary = do
        -- swallow the first boundary
        _  <- parseFromStream (parseFirstBoundary mixedBoundary) str
        bm <- search (fullBoundary mixedBoundary) str
        processParts (mixedStream fieldName) bm


    --------------------------------------------------------------------------
    mixedStream fieldName str = do
        hdrs <- takeHeaders str

        let (contentType, _) = getContentType hdrs
        let (_, fileName   ) = getFieldName hdrs

        let info = PartInfo fieldName fileName contentType
        clientHandler info str


------------------------------------------------------------------------------
getContentType :: Headers
               -> (ByteString, Maybe ByteString)
getContentType hdrs = (contentType, boundary)
  where
    contentTypeValue = fromMaybe "text/plain" $
                       getHeader "content-type" hdrs

    eCT = fullyParse contentTypeValue pContentTypeWithParameters
    (contentType, params) = either (const ("text/plain", [])) id eCT

    boundary = findParam "boundary" params


------------------------------------------------------------------------------
getFieldName :: Headers -> (ByteString, Maybe ByteString)
getFieldName hdrs = (fieldName, fileName)
  where
    contentDispositionValue = fromMaybe "" $
                              getHeader "content-disposition" hdrs

    eDisposition = fullyParse contentDispositionValue pValueWithParameters

    (_, dispositionParameters) =
        either (const ("", [])) id eDisposition

    fieldName = fromMaybe "" $ findParam "name" dispositionParameters

    fileName = findParam "filename" dispositionParameters


------------------------------------------------------------------------------
findParam :: (Eq a) => a -> [(a, b)] -> Maybe b
findParam p = fmap snd . find ((== p) . fst)


------------------------------------------------------------------------------
partStream :: InputStream MatchInfo -> IO (InputStream ByteString)
partStream st = StreamsI.sourceToStream go

  where
    go = StreamsI.withDefaultPushback $ do
        x <- Streams.read st >>= maybe (return Nothing) f
        return $! maybe (StreamsI.nullSource, Nothing)
                        (const $! (go, x))
                        x

    f (NoMatch s) = return $ Just s
    f _           = return Nothing




------------------------------------------------------------------------------
-- | Assuming we've already identified the boundary value and run
-- 'bmhEnumeratee' to split the input up into parts which match and parts
-- which don't, run the given 'ByteString' iteratee over each part and grab a
-- list of the resulting values.
--
-- TODO/FIXME: fix description
processParts :: (InputStream ByteString -> IO a)
             -> InputStream MatchInfo
             -> IO [a]
processParts partFunc stream = go id
  where
    part pStream = do
        isLast <- parseFromStream pBoundaryEnd pStream

        if isLast
          then return Nothing
          else do
              !x <- partFunc pStream
              Streams.skipToEof pStream
              return $! Just x

    go !soFar = do
        b <- Streams.atEOF stream
        if b
          then return $ soFar []
          else partStream stream >>=
               part >>=
               maybe (return $ soFar [])
                     (\x -> go (soFar . (x:)))

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


{-
------------------------------------------------------------------------------
-- We need some code to keep track of the files we have already successfully
-- created in case an exception is thrown by the request body enumerator or
-- one of the client iteratees.
data UploadedFilesState = UploadedFilesState {
      -- | This is the file which is currently being written to. If the
      -- calling function gets an exception here, it is responsible for
      -- closing and deleting this file.
      _currentFile :: Maybe (FilePath, Handle)

      -- | .. and these files have already been successfully read and closed.
    , _alreadyReadFiles :: [FilePath]
}


------------------------------------------------------------------------------
emptyUploadedFilesState :: UploadedFilesState
emptyUploadedFilesState = UploadedFilesState Nothing []


------------------------------------------------------------------------------
data UploadedFiles = UploadedFiles (IORef UploadedFilesState)
                                   (MVar ())


------------------------------------------------------------------------------
newUploadedFiles :: MonadIO m => m UploadedFiles
newUploadedFiles = liftIO $ do
    r <- newIORef emptyUploadedFilesState
    m <- newMVar ()
    let u = UploadedFiles r m
    addMVarFinalizer m $ cleanupUploadedFiles u
    return u


------------------------------------------------------------------------------
cleanupUploadedFiles :: (MonadIO m) => UploadedFiles -> m ()
cleanupUploadedFiles (UploadedFiles stateRef _) = liftIO $ do
    state <- readIORef stateRef
    killOpenFile state
    mapM_ killFile $ _alreadyReadFiles state
    writeIORef stateRef emptyUploadedFilesState

  where
    killFile = eatException . removeFile

    killOpenFile state = maybe (return ())
                               (\(fp,h) -> do
                                    eatException $ hClose h
                                    eatException $ removeFile fp)
                               (_currentFile state)


------------------------------------------------------------------------------
openFileForUpload :: (MonadIO m) =>
                     UploadedFiles
                  -> FilePath
                  -> m (FilePath, Handle)
openFileForUpload ufs@(UploadedFiles stateRef _) tmpdir = liftIO $ do
    state <- readIORef stateRef

    -- It should be an error to open a new file with this interface if there
    -- is already a file handle active.
    when (isJust $ _currentFile state) $ do
        cleanupUploadedFiles ufs
        throw $ GenericFileUploadException alreadyOpenMsg

    fph@(_,h) <- makeTempFile tmpdir "snap-"
    hSetBuffering h NoBuffering

    writeIORef stateRef $ state { _currentFile = Just fph }
    return fph

  where
    alreadyOpenMsg =
        T.concat [ "Internal error! UploadedFiles: "
                 , "opened new file with pre-existing open handle" ]


------------------------------------------------------------------------------
closeActiveFile :: (MonadIO m) => UploadedFiles -> m ()
closeActiveFile (UploadedFiles stateRef _) = liftIO $ do
    state <- readIORef stateRef
    let m = _currentFile state
    maybe (return ())
          (\(fp,h) -> do
               eatException $ hClose h
               writeIORef stateRef $
                 state { _currentFile = Nothing
                       , _alreadyReadFiles = fp:(_alreadyReadFiles state) })
          m


------------------------------------------------------------------------------
eatException :: (MonadCatchIO m) => m a -> m ()
eatException m =
    (m >> return ()) `catch` (\(_ :: SomeException) -> return ())
-}

------------------------------------------------------------------------------
withTempFile :: FilePath
             -> String
             -> ((FilePath, Handle) -> IO a)
             -> IO a
withTempFile tmpl temp handler =
    E.mask $ \restore -> bracket make cleanup (restore . handler)

  where
    make           = mkstemp $ tmpl </> (temp ++ "XXXXXXX")
    cleanup (fp,h) = sequence $ map gobble [hClose h, removeFile fp]
    gobble m       = void m `catch` \(_::SomeException) -> return ()
