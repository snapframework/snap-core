{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains web handlers to serve files from a directory.
module Snap.Util.FileServe
(
  getSafePath
, fileServe
, fileServe'
, fileServeSingle
, fileServeSingle'
, defaultMimeTypes
, MimeMap
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.Char8 hiding (Done)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Char8 (ByteString)
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isNothing)
import           Prelude hiding (show, Show)
import qualified Prelude
import           System.Directory
import           System.FilePath
import           System.PosixCompat.Files
import           Text.Show.ByteString hiding (runPut)
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Internal.Parsing
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
  ( ".ttf"     , "application/x-font-truetype"       ),
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
-- | Gets a path from the 'Request' using 'rqPathInfo' and makes sure it is
-- safe to use for opening files.  A path is safe if it is a relative path
-- and has no ".." elements to escape the intended directory structure.
getSafePath :: MonadSnap m => m FilePath
getSafePath = do
    req <- getRequest
    let p = S.unpack $ rqPathInfo req

    -- check that we don't have any sneaky .. paths
    let dirs = splitDirectories p
    when (elem ".." dirs) pass
    return p


------------------------------------------------------------------------------
-- | Serves files out of the given directory. The relative path given in
-- 'rqPathInfo' is searched for the given file, and the file is served with the
-- appropriate mime type if it is found. Absolute paths and \"@..@\" are prohibited
-- to prevent files from being served from outside the sandbox.
--
-- Uses 'defaultMimeTypes' to determine the @Content-Type@ based on the file's
-- extension.
fileServe :: MonadSnap m
          => FilePath  -- ^ root directory
          -> m ()
fileServe = fileServe' defaultMimeTypes
{-# INLINE fileServe #-}


------------------------------------------------------------------------------
-- | Same as 'fileServe', with control over the MIME mapping used.
fileServe' :: MonadSnap m
           => MimeMap           -- ^ MIME type mapping
           -> FilePath          -- ^ root directory
           -> m ()
fileServe' mm root = do
    sp <- getSafePath
    let fp   = root </> sp

    -- check that the file exists
    liftIO (doesFileExist fp) >>= flip unless pass

    let fn   = takeFileName fp
    let mime = fileType mm fn
    fileServeSingle' mime fp
{-# INLINE fileServe' #-}


------------------------------------------------------------------------------
-- | Serves a single file specified by a full or relative path.  The
-- path restrictions on fileServe don't apply to this function since
-- the path is not being supplied by the user.
fileServeSingle :: MonadSnap m
                => FilePath          -- ^ path to file
                -> m ()
fileServeSingle fp =
    fileServeSingle' (fileType defaultMimeTypes (takeFileName fp)) fp
{-# INLINE fileServeSingle #-}


------------------------------------------------------------------------------
-- | Same as 'fileServeSingle', with control over the MIME mapping used.
fileServeSingle' :: MonadSnap m
                 => ByteString        -- ^ MIME type mapping
                 -> FilePath          -- ^ path to file
                 -> m ()
fileServeSingle' mime fp = do
    reqOrig <- getRequest

    -- If-Range header must be ignored if there is no Range: header in the
    -- request (RFC 2616 section 14.27)
    let req = if isNothing $ getHeader "range" reqOrig
                then deleteHeader "if-range" reqOrig
                else reqOrig

    -- check "If-Modified-Since" and "If-Range" headers
    let mbH = getHeader "if-modified-since" req
    mbIfModified <- liftIO $ case mbH of
                               Nothing  -> return Nothing
                               (Just s) -> liftM Just $ parseHttpTime s

    -- If-Range header could contain an entity, but then parseHttpTime will
    -- fail and return 0 which means a 200 response will be generated anyways
    mbIfRange <- liftIO $ case getHeader "if-range" req of
                            Nothing  -> return Nothing
                            (Just s) -> liftM Just $ parseHttpTime s

    dbg $ "mbIfModified: " ++ Prelude.show mbIfModified
    dbg $ "mbIfRange: " ++ Prelude.show mbIfRange

    -- check modification time and bug out early if the file is not modified.
    --
    -- TODO: a stat cache would be nice here, but it'd need the date thread
    -- stuff from snap-server to be folded into snap-core
    filestat <- liftIO $ getFileStatus fp
    let mt = modificationTime filestat
    maybe (return $! ()) (\lt -> when (mt <= lt) notModified) mbIfModified

    let sz = fromIntegral $ fileSize filestat
    lm <- liftIO $ formatHttpTime mt

    -- ok, at this point we know the last-modified time and the
    -- content-type. set those.
    modifyResponse $ setHeader "Last-Modified" lm
                   . setHeader "Accept-Ranges" "bytes"
                   . setContentType mime


    -- now check: is this a range request? If there is an 'If-Range' header
    -- with an old modification time we skip this check and send a 200 response
    let skipRangeCheck = maybe (False)
                               (\lt -> mt > lt)
                               mbIfRange

    -- checkRangeReq checks for a Range: header in the request and sends a
    -- partial response if it matches.
    wasRange <- if skipRangeCheck
                  then return False
                  else checkRangeReq req fp sz

    dbg $ "was this a range request? " ++ Prelude.show wasRange

    -- if we didn't have a range request, we just do normal sendfile
    unless wasRange $ do
      modifyResponse $ setResponseCode 200
                     . setContentLength sz
      sendFile fp

  where
    --------------------------------------------------------------------------
    notModified = finishWith $
                  setResponseCode 304 emptyResponse


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
data RangeReq = RangeReq { _rangeFirst :: !Int64
                         , _rangeLast  :: !(Maybe Int64)
                         }
              | SuffixRangeReq { _suffixLength :: !Int64 }
  deriving (Eq, Prelude.Show)


------------------------------------------------------------------------------
rangeParser :: Parser RangeReq
rangeParser = string "bytes=" *>
              (byteRangeSpec <|> suffixByteRangeSpec) <*
              endOfInput
  where
    byteRangeSpec = do
        start <- parseNum
        char '-'
        end   <- option Nothing $ liftM Just parseNum

        return $ RangeReq start end

    suffixByteRangeSpec = liftM SuffixRangeReq $ char '-' *> parseNum


------------------------------------------------------------------------------
checkRangeReq :: (MonadSnap m) => Request -> FilePath -> Int64 -> m Bool
checkRangeReq req fp sz = do
    -- TODO/FIXME: multiple ranges
    dbg $ "checkRangeReq, fp=" ++ fp ++ ", sz=" ++ Prelude.show sz
    maybe (return False)
          (\s -> either (const $ return False)
                        withRange
                        (fullyParse s rangeParser))
          (getHeader "range" req)

  where
    withRange rng@(RangeReq start mend) = do
        dbg $ "withRange: got Range request: " ++ Prelude.show rng
        let end = fromMaybe (sz-1) mend
        dbg $ "withRange: start=" ++ Prelude.show start
                  ++ ", end=" ++ Prelude.show end

        if start < 0 || end < start || start >= sz || end >= sz
           then send416
           else send206 start end

    withRange rng@(SuffixRangeReq nbytes) = do
        dbg $ "withRange: got Range request: " ++ Prelude.show rng
        let end   = sz-1
        let start = sz - nbytes

        dbg $ "withRange: start=" ++ Prelude.show start
                  ++ ", end=" ++ Prelude.show end

        if start < 0 || end < start || start >= sz || end >= sz
           then send416
           else send206 start end

    -- note: start and end INCLUSIVE here
    send206 start end = do
        dbg "inside send206"
        let len = end-start+1
        let crng = S.concat $
                   L.toChunks $
                   L.concat [ "bytes "
                            , show start
                            , "-"
                            , show end
                            , "/"
                            , show sz ]

        modifyResponse $ setResponseCode 206
                       . setHeader "Content-Range" crng
                       . setContentLength len

        dbg $ "send206: sending range (" ++ Prelude.show start
                ++ "," ++ Prelude.show (end+1) ++ ") to sendFilePartial"

        -- end here was inclusive, sendFilePartial is exclusive
        sendFilePartial fp (start,end+1)
        return True


    send416 = do
        dbg "inside send416"
        -- if there's an "If-Range" header in the request, then we just send
        -- back 200
        if getHeader "If-Range" req /= Nothing
           then return False
           else do
               let crng = S.concat $
                          L.toChunks $
                          L.concat ["bytes */", show sz]
               
               modifyResponse $ setResponseCode 416
                              . setHeader "Content-Range" crng
                              . setContentLength 0
                              . deleteHeader "Content-Type"
                              . deleteHeader "Content-Encoding"
                              . deleteHeader "Transfer-Encoding"
                              . setResponseBody (enumBS "")
               
               return True



dbg :: (MonadIO m) => String -> m ()
dbg s = debug $ "FileServe:" ++ s
