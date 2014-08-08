-- | Contains web handlers to serve files from a directory.
--
-- Example usage:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- module Main where
--
-- import "Snap.Core"           (Snap, route)
-- import "Snap.Http.Server"    (quickHttpServe)
-- import "Snap.Util.FileServe"
--
-- site :: Snap ()
-- site = 'Snap.Core.route'
--   [ (\"\/files\",   'serveDirectory'                            \"static\")
--   , (\"\/simple\",  'serveDirectoryWith' 'simpleDirectoryConfig'  \"static\")
--   , (\"\/default\", 'serveDirectoryWith' 'defaultDirectoryConfig' \"static\")
--   , (\"\/fancy\",   'serveDirectoryWith' 'fancyDirectoryConfig'   \"static\")
--   , (\"\/paper\",   'serveFile'                                 \"static\/paper.pdf\")
--   , (\"\/thesis\",  'serveFileAs'        \"application\/pdf\"      \"static\/thesis.pdf\")
--   ]
--
-- main :: IO ()
-- main = 'Snap.Http.Server.quickHttpServe' site
-- @
module Snap.Util.FileServe
  ( -- * Helper functions
    getSafePath
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
