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

import           Snap.Internal.Util.FileServe (DirectoryConfig (..), HandlerMap, MimeMap, defaultDirectoryConfig, defaultIndexGenerator, defaultMimeTypes, fancyDirectoryConfig, fileType, getSafePath, serveDirectory, serveDirectoryWith, serveFile, serveFileAs, simpleDirectoryConfig)
