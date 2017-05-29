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
-- lower-level interface called 'handleMultipart'. That function takes
-- uploaded files and streams them to a consumer of the user's choosing.
--
-- Using these functions requires making \"policy\" decisions which Snap can't
-- really make for users, such as \"what's the largest PDF file a user is
-- allowed to upload?\" and \"should we read form inputs into the parameters
-- mapping?\". Policy is specified on a \"global\" basis (using
-- 'UploadPolicy'), and on a per-file basis (using 'PartUploadPolicy', which
-- allows you to reject or limit the size of certain uploaded
-- @Content-type@s).
--
-- Example usage:
--
-- @
-- {-\# LANGUAGE OverloadedStrings #-}
--
-- module Main where
--
-- import qualified Data.ByteString.Char8 as B8
-- import           Data.Functor          ((\<$>))
-- import           "Snap.Core"             ('Snap.Core.Snap', 'Snap.Core.route', 'Snap.Core.writeBS')
-- import           Snap.Http.Server      (quickHttpServe)
-- import           "Snap.Util.FileUploads"
-- import           System.Posix          (FileOffset, fileSize, getFileStatus)
--
-- uploadForm :: 'Snap.Core.Snap' ()
-- uploadForm = 'Snap.Core.writeBS' \"\<form enctype=\\\"multipart\/form-data\\\" action=\\"\/do-upload\\\" method=\\\"POST\\\">\\
--     \\\<input name=\\\"file\\\" type=\\\"file\\\" \/>\\
--     \\\<input type=\\\"submit\\\" value=\\\"Send File\\\" \/>\\
--     \\\<\/form>\"
--
-- getFileSize :: FilePath -> IO FileOffset
-- getFileSize path = fileSize \<$> getFileStatus path
--
-- -- Upload handler that prints out the uploaded file\'s size.
-- doUpload :: 'Snap.Core.Snap' ()
-- doUpload = do
--   l \<- 'handleFileUploads' \"\/tmp\" 'defaultUploadPolicy'
--        (const $ 'allowWithMaximumSize' ('getMaximumFormInputSize' 'defaultUploadPolicy'))
--        (\\pinfo mbfname -> do fsize \<- either (const $ return 0) getFileSize mbfname
--                              return ('partFileName' pinfo, fsize))
--   'writeBS' . B8.pack . show $ l
--
-- site :: 'Snap.Core.Snap' ()
-- site = 'Snap.Core.route'
--   [ (\"\/upload\",    uploadForm)
--   , (\"\/do-upload\", doUpload)]
--
-- main :: IO ()
-- main = quickHttpServe site
-- @
module Snap.Util.FileUploads
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
  , PartInfo
  , PartDisposition(..)
  , partFieldName
  , partFileName
  , partContentType
  , partHeaders
  , partDisposition

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

    -- *** File upload policy
  , FileUploadPolicy
  , defaultFileUploadPolicy
  , setMaximumFileSize
  , setMaximumNumberOfFiles
  , setSkipFilesWithoutNames
  , setMaximumSkippedFileSize

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


import           Snap.Internal.Util.FileUploads (BadPartException (badPartExceptionReason), FileUploadException, FileUploadPolicy, FormFile (..), FormParam, PartDisposition (..), PartFold, PartInfo (..), PartProcessor, PartUploadPolicy, PolicyViolationException (policyViolationExceptionReason), UploadPolicy, allowWithMaximumSize, defaultFileUploadPolicy, defaultUploadPolicy, disallow, doProcessFormInputs, fileUploadExceptionReason, foldMultipart, getMaximumFormInputSize, getMaximumNumberOfFormInputs, getMinimumUploadRate, getMinimumUploadSeconds, getUploadTimeout, handleFileUploads, handleFormUploads, handleMultipart, setMaximumFileSize, setMaximumFormInputSize, setMaximumNumberOfFiles, setMaximumNumberOfFormInputs, setMaximumSkippedFileSize, setMinimumUploadRate, setMinimumUploadSeconds, setProcessFormInputs, setSkipFilesWithoutNames, setUploadTimeout, storeAsLazyByteString, withTemporaryStore)
