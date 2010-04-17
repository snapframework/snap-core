{-# LANGUAGE OverloadedStrings #-}

-- | Contains web handlers to serve files from a directory.
module Snap.Util.FileServe
(
  fileServe
, fileServe'
, defaultMimeTypes
, MimeMap
) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import           Data.Iteratee.IO (enumHandle)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Posix.Files
import           System.Time

------------------------------------------------------------------------------
import           Snap.Iteratee hiding (drop)
import           Snap.Types


------------------------------------------------------------------------------
-- | A type alias for MIME type 
type MimeMap = Map FilePath ByteString


------------------------------------------------------------------------------
-- | The default set of mime type mappings we use when serving files. Its
-- value:
--
-- > Map.fromList [
-- >   ( ".asc"     , "text/plain"                        ),
-- >   ( ".asf"     , "video/x-ms-asf"                    ),
-- >   ( ".asx"     , "video/x-ms-asf"                    ),
-- >   ( ".avi"     , "video/x-msvideo"                   ),
-- >   ( ".bz2"     , "application/x-bzip"                ),
-- >   ( ".c"       , "text/plain"                        ),
-- >   ( ".class"   , "application/octet-stream"          ),
-- >   ( ".conf"    , "text/plain"                        ),
-- >   ( ".cpp"     , "text/plain"                        ),
-- >   ( ".css"     , "text/css"                          ),
-- >   ( ".cxx"     , "text/plain"                        ),
-- >   ( ".dtd"     , "text/xml"                          ),
-- >   ( ".dvi"     , "application/x-dvi"                 ),
-- >   ( ".gif"     , "image/gif"                         ),
-- >   ( ".gz"      , "application/x-gzip"                ),
-- >   ( ".hs"      , "text/plain"                        ),
-- >   ( ".htm"     , "text/html"                         ),
-- >   ( ".html"    , "text/html"                         ),
-- >   ( ".jar"     , "application/x-java-archive"        ),
-- >   ( ".jpeg"    , "image/jpeg"                        ),
-- >   ( ".jpg"     , "image/jpeg"                        ),
-- >   ( ".js"      , "text/javascript"                   ),
-- >   ( ".log"     , "text/plain"                        ),
-- >   ( ".m3u"     , "audio/x-mpegurl"                   ),
-- >   ( ".mov"     , "video/quicktime"                   ),
-- >   ( ".mp3"     , "audio/mpeg"                        ),
-- >   ( ".mpeg"    , "video/mpeg"                        ),
-- >   ( ".mpg"     , "video/mpeg"                        ),
-- >   ( ".ogg"     , "application/ogg"                   ),
-- >   ( ".pac"     , "application/x-ns-proxy-autoconfig" ),
-- >   ( ".pdf"     , "application/pdf"                   ),
-- >   ( ".png"     , "image/png"                         ),
-- >   ( ".ps"      , "application/postscript"            ),
-- >   ( ".qt"      , "video/quicktime"                   ),
-- >   ( ".sig"     , "application/pgp-signature"         ),
-- >   ( ".spl"     , "application/futuresplash"          ),
-- >   ( ".swf"     , "application/x-shockwave-flash"     ),
-- >   ( ".tar"     , "application/x-tar"                 ),
-- >   ( ".tar.bz2" , "application/x-bzip-compressed-tar" ),
-- >   ( ".tar.gz"  , "application/x-tgz"                 ),
-- >   ( ".tbz"     , "application/x-bzip-compressed-tar" ),
-- >   ( ".text"    , "text/plain"                        ),
-- >   ( ".tgz"     , "application/x-tgz"                 ),
-- >   ( ".torrent" , "application/x-bittorrent"          ),
-- >   ( ".txt"     , "text/plain"                        ),
-- >   ( ".wav"     , "audio/x-wav"                       ),
-- >   ( ".wax"     , "audio/x-ms-wax"                    ),
-- >   ( ".wma"     , "audio/x-ms-wma"                    ),
-- >   ( ".wmv"     , "video/x-ms-wmv"                    ),
-- >   ( ".xbm"     , "image/x-xbitmap"                   ),
-- >   ( ".xml"     , "text/xml"                          ),
-- >   ( ".xpm"     , "image/x-xpixmap"                   ),
-- >   ( ".xwd"     , "image/x-xwindowdump"               ),
-- >   ( ".zip"     , "application/zip"                   ) ]
--
defaultMimeTypes :: MimeMap
defaultMimeTypes = Map.fromList [
  ( ".asc"     , "text/plain"                        ),
  ( ".asf"     , "video/x-ms-asf"                    ),
  ( ".asx"     , "video/x-ms-asf"                    ),
  ( ".avi"     , "video/x-msvideo"                   ),
  ( ".bz2"     , "application/x-bzip"                ),
  ( ".c"       , "text/plain"                        ),
  ( ".class"   , "application/octet-stream"          ),
  ( ".conf"    , "text/plain"                        ),
  ( ".cpp"     , "text/plain"                        ),
  ( ".css"     , "text/css"                          ),
  ( ".cxx"     , "text/plain"                        ),
  ( ".dtd"     , "text/xml"                          ),
  ( ".dvi"     , "application/x-dvi"                 ),
  ( ".gif"     , "image/gif"                         ),
  ( ".gz"      , "application/x-gzip"                ),
  ( ".hs"      , "text/plain"                        ),
  ( ".htm"     , "text/html"                         ),
  ( ".html"    , "text/html"                         ),
  ( ".jar"     , "application/x-java-archive"        ),
  ( ".jpeg"    , "image/jpeg"                        ),
  ( ".jpg"     , "image/jpeg"                        ),
  ( ".js"      , "text/javascript"                   ),
  ( ".log"     , "text/plain"                        ),
  ( ".m3u"     , "audio/x-mpegurl"                   ),
  ( ".mov"     , "video/quicktime"                   ),
  ( ".mp3"     , "audio/mpeg"                        ),
  ( ".mpeg"    , "video/mpeg"                        ),
  ( ".mpg"     , "video/mpeg"                        ),
  ( ".ogg"     , "application/ogg"                   ),
  ( ".pac"     , "application/x-ns-proxy-autoconfig" ),
  ( ".pdf"     , "application/pdf"                   ),
  ( ".png"     , "image/png"                         ),
  ( ".ps"      , "application/postscript"            ),
  ( ".qt"      , "video/quicktime"                   ),
  ( ".sig"     , "application/pgp-signature"         ),
  ( ".spl"     , "application/futuresplash"          ),
  ( ".swf"     , "application/x-shockwave-flash"     ),
  ( ".tar"     , "application/x-tar"                 ),
  ( ".tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( ".tar.gz"  , "application/x-tgz"                 ),
  ( ".tbz"     , "application/x-bzip-compressed-tar" ),
  ( ".text"    , "text/plain"                        ),
  ( ".tgz"     , "application/x-tgz"                 ),
  ( ".torrent" , "application/x-bittorrent"          ),
  ( ".txt"     , "text/plain"                        ),
  ( ".wav"     , "audio/x-wav"                       ),
  ( ".wax"     , "audio/x-ms-wax"                    ),
  ( ".wma"     , "audio/x-ms-wma"                    ),
  ( ".wmv"     , "video/x-ms-wmv"                    ),
  ( ".xbm"     , "image/x-xbitmap"                   ),
  ( ".xml"     , "text/xml"                          ),
  ( ".xpm"     , "image/x-xpixmap"                   ),
  ( ".xwd"     , "image/x-xwindowdump"               ),
  ( ".zip"     , "application/zip"                   ) ]

------------------------------------------------------------------------------
-- | Serves files out of the given directory. The relative path given in
-- 'rqPathInfo' is searched for the given file, and the file is served with the
-- appropriate mime type if it is found. Absolute paths and \"@..@\" are prohibited
-- to prevent files from being served from outside the sandbox.
--
-- Uses 'defaultMimeTypes' to determine the @Content-Type@ based on the file's
-- extension.
fileServe :: FilePath  -- ^ root directory
          -> Snap ()
fileServe = fileServe' defaultMimeTypes


------------------------------------------------------------------------------
-- | Same as 'fileServe', with control over the MIME mapping used.
fileServe' :: MimeMap           -- ^ MIME type mapping
           -> FilePath          -- ^ root directory
           -> Snap ()
fileServe' mm root = do
    req <- getRequest
    let pInfo = S.unpack $ rqPathInfo req
    let mbIfModified = (getHeader "if-modified-since" req >>=
                        parseHttpTime)

    fp <- resolvePath pInfo

    -- check modification time and bug out early if the file is not modified.
    mt <- liftIO $ liftM clock2time $ getModificationTime fp
    maybe (return ()) (chkModificationTime mt) mbIfModified

    -- get the mime type for the file. We know this is a file, otherwise we
    -- would've failed earlier.
    let fn   = takeFileName fp
    let mime = fileType mm fn

    sz <- liftIO $ liftM (fromEnum . fileSize) $ getFileStatus fp

    modifyResponse $ setHeader "Last-Modified" (formatHttpTime mt)
                   . setContentType mime
                   . setContentLength sz
                   . setResponseBody (enumFile fp)

  where
    --------------------------------------------------------------------------
    resolvePath p = do
        -- relative paths only!
        when (not $ isRelative p) pass

        -- check that we don't have any sneaky .. paths
        let dirs = splitDirectories p
        when (elem ".." dirs) pass

        let f = root </> p

        -- check that the file exists
        liftIO (doesFileExist f) >>= flip unless pass

        return f

    --------------------------------------------------------------------------
    chkModificationTime mt lt = when (mt <= lt) notModified

    --------------------------------------------------------------------------
    notModified = finishWith $
                  setResponseStatus 304 "Not Modified" emptyResponse


------------------------------------------------------------------------------
fileType :: MimeMap -> FilePath -> ByteString
fileType mm f =
    if null ext
      then defaultMimeType
      else fromMaybe (fileType mm (drop 1 ext))
                     mbe

  where
    ext             = takeExtensions f
    mbe             = Map.lookup ext mm


------------------------------------------------------------------------------
defaultMimeType :: ByteString
defaultMimeType = "application/octet-stream"


------------------------------------------------------------------------------
enumFile :: FilePath -> Iteratee IO a -> IO (Iteratee IO a)
enumFile fp iter = do
    h  <- liftIO $ openBinaryFile fp ReadMode
    i' <- enumHandle h iter
    return $ do
        x <- i'
        liftIO (hClose h)
        return x


------------------------------------------------------------------------------
clock2time :: ClockTime -> UTCTime
clock2time (TOD x y) =
    posixSecondsToUTCTime $ fromInteger x + fromRational (y % 1000000000000)
