{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains web handlers to serve files from a directory.
module Snap.Util.FileServe
(
  getSafePath
, FileServeConfig(..)
, fileServeCfg
, fileServe
, fileServe'
, fileServeSingle
, fileServeSingle'
, defaultMimeTypes
, MimeMap
, defaultIndexGenerator
, HandlerMap
) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.Char8 hiding (Done)
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Internal (c2w)
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid
import           Prelude hiding (show, Show)
import qualified Prelude
import           System.Directory
import           System.FilePath
import           System.PosixCompat.Files
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Internal.Parsing
import           Snap.Iteratee hiding (drop)
import           Snap.Types


------------------------------------------------------------------------------
-- | A type alias for MIME type
type MimeMap = Map FilePath ByteString


------------------------------------------------------------------------------
-- | A type alias for dynamic handlers
type HandlerMap m = Map FilePath (FilePath -> m ())


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
    let mp = urlDecode $ rqPathInfo req

    p <- maybe pass (return . S.unpack) mp

    -- relative paths only!
    when (not $ isRelative p) pass

    -- check that we don't have any sneaky .. paths
    let dirs = splitDirectories p
    when (elem ".." dirs) pass

    return $ joinPath dirs


------------------------------------------------------------------------------
-- | A collection of options for serving static files out of a directory.
data FileServeConfig m = FileServeConfig {
    -- | Files to look for when a directory is requested (e.g., index.html)
    indexFiles      :: [FilePath],

    -- | Handler to generate a directory listing if there is no index.
    indexGenerator  :: FilePath -> m (),

    -- | Map of extensions to pass to dynamic file handlers.  This could be
    -- used, for example, to implement CGI dispatch, pretty printing of source
    -- code, etc.
    dynamicHandlers :: HandlerMap m,

    -- | MIME type map to look up content types.
    mimeTypes       :: MimeMap
    }


------------------------------------------------------------------------------
-- | The ultimate static file and directory server.  Configuration options are
-- passed in a 'FileServeConfig' that captures all of the relevant choices
-- about serving the directory.
fileServeCfg :: MonadSnap m
             => FileServeConfig m  -- ^ Configuration options
             -> FilePath           -- ^ Base directory to serve from
             -> m ()
fileServeCfg cfg base = do
    b <- directory <|> file <|> redir
    when (not b) pass

  where

    idxs     = indexFiles cfg
    generate = indexGenerator cfg
    mimes    = mimeTypes cfg
    dyns     = dynamicHandlers cfg

    -- Serves a file if it exists; passes if not
    serve f = do
        liftIO (doesFileExist f) >>= flip unless pass
        let fname       = takeFileName f
        let staticServe = fileServeSingle' (fileType mimes fname)
        lookupExt staticServe dyns fname f
        return True

    -- Serves a directory via indices if available.  Returns True on success,
    -- False on failure to find an index.  Passes /only/ if the request was
    -- not for a directory (no trailing slash).
    directory = do
        rq  <- getRequest
        unless ("/" `S.isSuffixOf` rqURI rq) pass
        rel <- (base </>) <$> getSafePath
        b   <- liftIO $ doesDirectoryExist rel
        if b then do let serveRel f = serve (rel </> f)
                     foldl' (<|>) pass (Prelude.map serveRel idxs)
                         <|> (generate rel >> return True)
                         <|> return False
             else return False

    -- Serves a file requested by name.  Passes if the file doesn't exist.
    file = serve =<< ((base </>) <$> getSafePath)

    -- If the request is for a directory but lacks a trailing slash, redirects
    -- to the directory name with a trailing slash.
    redir = do
        rel <- (base </>) <$> getSafePath
        liftIO (doesDirectoryExist rel) >>= flip unless pass
        rq <- getRequest
        redirect $ rqURI rq `S.append` "/" `S.append` rqQueryString rq


------------------------------------------------------------------------------
-- | An automatic index generator that copies the Snap look and feel to some
-- extent while remaining fairly small and not relying on external files
-- (which may not be there depending on external request routing).
defaultIndexGenerator :: MonadSnap m => MimeMap -> FilePath -> m ()
defaultIndexGenerator mm d = do
    modifyResponse $ setContentType "text/html"

    rq      <- getRequest

    writeBS "<style type=\"text/css\">body{margin: 0px 0px 0px 0px;"
    writeBS "font-family: sans-serif}div.header{padding:40px 40px 0px"
    writeBS " 40px;background:rgb(25,50,87);background-image:"
    writeBS "-webkit-gradient(linear,left bottom,left top,color-stop("
    writeBS "0.00, rgb(31,62,108)),color-stop(1.00, rgb(19,38,66))"
    writeBS ");background-image:-moz-linear-gradient(center bottom,"
    writeBS "rgb(31,62,108) 0%,rgb(19,38,66) 100%);text-shadow:-1px"
    writeBS " 3px 1px rgb(16,33,57);height:35px;font-size:16pt;"
    writeBS "letter-spacing: 2pt;color: white}div.divider {background:"
    writeBS "rgb(46,93,156);height:10px}div.content{background:rgb("
    writeBS "255,255,255);background-image: -webkit-gradient(linear,"
    writeBS "left bottom, left top,color-stop(0.50, rgb(255,255,255)),"
    writeBS "color-stop(1.00, rgb(224,234,247)));background-image:"
    writeBS "-moz-linear-gradient(center bottom,rgb(255,255,255) 50%,"
    writeBS "rgb(224,234,247) 100%);padding: 40px 40px 40px 40px}div"
    writeBS ".footer{padding:16px 0px 10px 10px;border-top:1px solid"
    writeBS " rgb(194,209,225);color:rgb(160,172,186);height:31px;"
    writeBS "font-family:sans-serif;font-size:10pt;background: rgb(245,"
    writeBS "249,255)}table{width:100%}tr:hover{background:rgb(256,256,"
    writeBS "224)}td{border:dotted thin black;font-family:monospace}"
    writeBS "th{border:solid thin black;background:rgb(28,56,97);"
    writeBS "text-shadow:-1px 3px 1px rgb(16,33,57);color: white}</style>"
    writeBS "<div class=\"header\">Directory Listing: "
    writeBS (rqURI rq)
    writeBS "</div><div class=\"divider\"></div><div class=\"content\">"
    writeBS "<table><tr><th>File Name</th><th>Type</th><th>Last Modified"
    writeBS "</th></tr>"

    when (rqURI rq /= "/") $
        writeBS "<tr><td><a href='../'>..</a></td><td colspan=2>DIR</td></tr>"

    entries <- liftIO $ getDirectoryContents d
    dirs    <- liftIO $ filterM (doesDirectoryExist . (d </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (d </>)) entries

    forM_ (sort $ filter (not . (`elem` ["..", "."])) dirs) $ \f -> do
        writeBS "<tr><td><a href='"
        writeBS (S.pack f)
        writeBS "/'>"
        writeBS (S.pack f)
        writeBS "</a></td><td colspan=2>DIR</td></tr>"

    forM_ (sort files) $ \f -> do
        stat <- liftIO $ getFileStatus (d </> f)
        tm   <- liftIO $ formatHttpTime (modificationTime stat)
        writeBS "<tr><td><a href='"
        writeBS (S.pack f)
        writeBS "'>"
        writeBS (S.pack f)
        writeBS "</a></td><td>"
        writeBS (fileType mm f)
        writeBS "</td><td>"
        writeBS tm
        writeBS "</tr>"

    writeBS "</table></div><div class=\"footer\">Powered by "
    writeBS "<b><a href=\"http://snapframework.com\">Snap</a></b></div>"


------------------------------------------------------------------------------
-- | Serves files out of the given directory. The relative path given in
-- 'rqPathInfo' is searched for the given file, and the file is served with
-- the appropriate mime type if it is found. Absolute paths and \"@..@\" are
-- prohibited to prevent files from being served from outside the sandbox.
--
-- Uses 'defaultMimeTypes' to determine the @Content-Type@ based on the file's
-- extension.
fileServe :: MonadSnap m
          => FilePath  -- ^ root directory
          -> m ()
fileServe = fileServeCfg $ FileServeConfig {
    indexFiles = [],
    indexGenerator = const pass,
    dynamicHandlers = Map.empty,
    mimeTypes = defaultMimeTypes
    }
{-# INLINE fileServe #-}


------------------------------------------------------------------------------
-- | Same as 'fileServe', with control over the MIME mapping used.
fileServe' :: MonadSnap m
           => MimeMap           -- ^ MIME type mapping
           -> FilePath          -- ^ root directory
           -> m ()
fileServe' mm = fileServeCfg $ FileServeConfig {
    indexFiles = [],
    indexGenerator = const pass,
    dynamicHandlers = Map.empty,
    mimeTypes = mm
    }
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
    -- with an old modification time we skip this check and send a 200
    -- response
    let skipRangeCheck = maybe (False)
                               (\lt -> mt > lt)
                               mbIfRange

    -- checkRangeReq checks for a Range: header in the request and sends a
    -- partial response if it matches.
    wasRange <- if skipRangeCheck
                  then return False
                  else liftSnap $ checkRangeReq req fp sz

    dbg $ "was this a range request? " ++ Prelude.show wasRange

    -- if we didn't have a range request, we just do normal sendfile
    unless wasRange $ do
      modifyResponse $ setResponseCode 200
                     . setContentLength sz
      liftSnap $ sendFile fp

  where
    --------------------------------------------------------------------------
    notModified = finishWith $
                  setResponseCode 304 emptyResponse


------------------------------------------------------------------------------
lookupExt :: a -> Map FilePath a -> FilePath -> a
lookupExt def m f =
    if null ext
      then def
      else fromMaybe (lookupExt def m (drop 1 ext)) mbe

  where
    ext             = takeExtensions f
    mbe             = Map.lookup ext m


------------------------------------------------------------------------------
fileType :: MimeMap -> FilePath -> ByteString
fileType = lookupExt defaultMimeType


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
        let crng = toByteString $
                   mconcat [ fromByteString "bytes "
                           , fromShow start
                           , fromWord8 (c2w '-')
                           , fromShow end
                           , fromWord8 (c2w '/')
                           , fromShow sz ]

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
               let crng = toByteString $
                          mconcat [ fromByteString "bytes */"
                                  , fromShow sz ]

               modifyResponse $ setResponseCode 416
                              . setHeader "Content-Range" crng
                              . setContentLength 0
                              . deleteHeader "Content-Type"
                              . deleteHeader "Content-Encoding"
                              . deleteHeader "Transfer-Encoding"
                              . setResponseBody (enumBuilder mempty)

               return True


------------------------------------------------------------------------------
dbg :: (MonadIO m) => String -> m ()
dbg s = debug $ "FileServe:" ++ s
