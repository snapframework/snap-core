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


import           Snap.Internal.Util.FileUploads (BadPartException (badPartExceptionReason), FileUploadException, PartInfo (..), PartProcessor, PartUploadPolicy, PolicyViolationException (policyViolationExceptionReason), UploadPolicy, allowWithMaximumSize, defaultUploadPolicy, disallow, doProcessFormInputs, fileUploadExceptionReason, getMaximumFormInputSize, getMaximumNumberOfFormInputs, getMinimumUploadRate, getMinimumUploadSeconds, getUploadTimeout, handleFileUploads, handleMultipart, setMaximumFormInputSize, setMaximumNumberOfFormInputs, setMinimumUploadRate, setMinimumUploadSeconds, setProcessFormInputs, setUploadTimeout)
