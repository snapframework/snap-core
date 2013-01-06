-- | Contains web handlers to serve files from a directory.
module Snap.Util.FileServe
  ( getSafePath
    -- * Configuration for directory serving
  , MimeMap
  , HandlerMap
  , DirectoryConfig(..)
  , simpleDirectoryConfig
  , defaultDirectoryConfig
  , fancyDirectoryConfig
  , defaultIndexGenerator
  , defaultMimeTypes
  , fileType
    -- * File servers
  , serveDirectory
  , serveDirectoryWith
  , serveFile
  , serveFileAs
  ) where

import           Snap.Internal.Util.FileServe
