{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains web handlers to serve files from a directory.
module Snap.Util.FileServe
(
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

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           Control.Applicative
import           Control.Exception (SomeException, evaluate)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Internal (c2w)
import           Data.Int
import           Data.List
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#if MIN_VERSION_base(4,6,0)
import           Prelude hiding (show, Show)
#else
import           Prelude hiding (catch, show, Show)
#endif
import qualified Prelude
import           System.Directory
import           System.FilePath
import           System.PosixCompat.Files
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Internal.Debug
import           Snap.Internal.Parsing
import           Snap.Iteratee hiding (drop)


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

    return $! joinPath dirs


------------------------------------------------------------------------------
-- | A type alias for dynamic handlers
type HandlerMap m = HashMap FilePath (FilePath -> m ())


------------------------------------------------------------------------------
-- | A type alias for MIME type
type MimeMap = HashMap FilePath ByteString


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
-- >   ( ".ico"     , "image/x-icon"                      ),
-- >   ( ".jar"     , "application/x-java-archive"        ),
-- >   ( ".jpeg"    , "image/jpeg"                        ),
-- >   ( ".jpg"     , "image/jpeg"                        ),
-- >   ( ".js"      , "text/javascript"                   ),
-- >   ( ".json"    , "application/json"                  ),
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
-- >   ( ".svg"     , "image/svg+xml"                     ),
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
  ( ".ico"     , "image/x-icon"                      ),
  ( ".jar"     , "application/x-java-archive"        ),
  ( ".jpeg"    , "image/jpeg"                        ),
  ( ".jpg"     , "image/jpeg"                        ),
  ( ".js"      , "text/javascript"                   ),
  ( ".json"    , "application/json"                  ),
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
  ( ".svg"     , "image/svg+xml"                     ),
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
-- | A collection of options for serving static files out of a directory.
data DirectoryConfig m = DirectoryConfig {
    -- | Files to look for when a directory is requested (e.g., index.html)
    indexFiles      :: [FilePath],

    -- | Handler to generate a directory listing if there is no index.
    indexGenerator  :: FilePath -> m (),

    -- | Map of extensions to pass to dynamic file handlers.  This could be
    -- used, for example, to implement CGI dispatch, pretty printing of source
    -- code, etc.
    dynamicHandlers :: HandlerMap m,

    -- | MIME type map to look up content types.
    mimeTypes       :: MimeMap,

    -- | Handler that is called before a file is served.  It will only be
    -- called when a file is actually found, not for generated index pages.
    preServeHook    :: FilePath -> m ()
    }


------------------------------------------------------------------------------
-- | Style information for the default directory index generator.
snapIndexStyles :: ByteString
snapIndexStyles =
    S.intercalate "\n"
        [ "body { margin: 0px 0px 0px 0px; font-family: sans-serif }"
        , "div.header {"
        ,     "padding: 40px 40px 0px 40px; height:35px;"
        ,     "background:rgb(25,50,87);"
        ,     "background-image:-webkit-gradient("
        ,         "linear,left bottom,left top,"
        ,         "color-stop(0.00, rgb(31,62,108)),"
        ,         "color-stop(1.00, rgb(19,38,66)));"
        ,     "background-image:-moz-linear-gradient("
        ,         "center bottom,rgb(31,62,108) 0%,rgb(19,38,66) 100%);"
        ,     "text-shadow:-1px 3px 1px rgb(16,33,57);"
        ,     "font-size:16pt; letter-spacing: 2pt; color:white;"
        ,     "border-bottom:10px solid rgb(46,93,156) }"
        , "div.content {"
        ,     "background:rgb(255,255,255);"
        ,     "background-image:-webkit-gradient("
        ,         "linear,left bottom, left top,"
        ,         "color-stop(0.50, rgb(255,255,255)),"
        ,         "color-stop(1.00, rgb(224,234,247)));"
        ,     "background-image:-moz-linear-gradient("
        ,         "center bottom, white 50%, rgb(224,234,247) 100%);"
        ,     "padding: 40px 40px 40px 40px }"
        , "div.footer {"
        ,     "padding: 16px 0px 10px 10px; height:31px;"
        ,     "border-top: 1px solid rgb(194,209,225);"
        ,     "color: rgb(160,172,186); font-size:10pt;"
        ,     "background: rgb(245,249,255) }"
        , "table { max-width:100%; margin: 0 auto;" `S.append`
          " border-collapse: collapse; }"
        , "tr:hover { background:rgb(256,256,224) }"
        , "td { border:0; font-family:monospace; padding: 2px 0; }"
        , "td.filename, td.type { padding-right: 2em; }"
        , "th { border:0; background:rgb(28,56,97);"
        ,      "text-shadow:-1px 3px 1px rgb(16,33,57); color: white}"
        ]


------------------------------------------------------------------------------
-- | An automatic index generator, which is fairly small and does not rely on
-- any external files (which may not be there depending on external request
-- routing).
--
-- A 'MimeMap' is passed in to display the types of files in the directory
-- listing based on their extension.  Preferably, this is the same as the map
-- in the 'DirectoryConfig'
--
-- The styles parameter allows you to apply styles to the directory listing.
-- The listing itself consists of a table, containing a header row using
-- th elements, and one row per file using td elements, so styles for those
-- pieces may be attached to the appropriate tags.
defaultIndexGenerator :: MonadSnap m
                      => MimeMap    -- ^ MIME type mapping for reporting types
                      -> ByteString -- ^ Style info to insert in header
                      -> FilePath   -- ^ Directory to generate index for
                      -> m ()
defaultIndexGenerator mm styles d = do
    modifyResponse $ setContentType "text/html; charset=utf-8"
    rq      <- getRequest

    let uri   = uriWithoutQueryString rq
    let pInfo = rqPathInfo rq

    writeBS "<!DOCTYPE html>\n<html>\n<head>"
    writeBS "<title>Directory Listing: "
    writeBS uri
    writeBS "</title>"
    writeBS "<style type='text/css'>"
    writeBS styles
    writeBS "</style></head><body>"
    writeBS "<div class=\"header\">Directory Listing: "
    writeBS uri
    writeBS "</div><div class=\"content\">"
    writeBS "<table><tr><th>File Name</th><th>Type</th><th>Last Modified"
    writeBS "</th></tr>"

    when (pInfo /= "") $
        writeBS "<tr><td><a href='../'>..</a></td><td colspan=2>DIR</td></tr>"

    entries <- liftIO $ getDirectoryContents d
    dirs    <- liftIO $ filterM (doesDirectoryExist . (d </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (d </>)) entries

    forM_ (sort $ filter (not . (`elem` ["..", "."])) dirs) $ \f0 -> do
        f <- liftIO $ liftM (\s -> T.encodeUtf8 s `mappend` "/") $ packFn f0
        writeBS "<tr><td class='filename'><a href='"
        writeBS f
        writeBS "'>"
        writeBS f
        writeBS "</a></td><td class='type' colspan=2>DIR</td></tr>"

    forM_ (sort files) $ \f0 -> do
        f <- liftIO $ liftM T.encodeUtf8 $ packFn f0
        stat <- liftIO $ getFileStatus (d </> f0)
        tm   <- liftIO $ formatHttpTime (modificationTime stat)
        writeBS "<tr><td class='filename'><a href='"
        writeBS f
        writeBS "'>"
        writeBS f
        writeBS "</a></td><td class='type'>"
        writeBS (fileType mm f0)
        writeBS "</td><td>"
        writeBS tm
        writeBS "</tr>"

    writeBS "</table></div><div class=\"footer\">Powered by "
    writeBS "<b><a href=\"http://snapframework.com\">Snap</a></b></div>"
    writeBS "</body>"
  where
    packFn fp = do
        tryFirst [ T.decodeUtf8
                 , T.decodeUtf16LE
                 , T.decodeUtf16BE
                 , T.decodeUtf32LE
                 , T.decodeUtf32BE
                 , const (T.pack fp) ]
      where
        tryFirst []     = error "No valid decoding"
        tryFirst (f:fs) =
            evaluate (f bs) `catch` \(_::SomeException) -> tryFirst fs

        bs = S.pack fp

------------------------------------------------------------------------------
-- | A very simple configuration for directory serving.  This configuration
-- uses built-in MIME types from 'defaultMimeTypes', and has no index files,
-- index generator, dynamic file handlers, or 'preServeHook'.
simpleDirectoryConfig :: MonadSnap m => DirectoryConfig m
simpleDirectoryConfig = DirectoryConfig {
    indexFiles = [],
    indexGenerator = const pass,
    dynamicHandlers = Map.empty,
    mimeTypes = defaultMimeTypes,
    preServeHook = const $ return ()
    }


------------------------------------------------------------------------------
-- | A reasonable default configuration for directory serving.  This
-- configuration uses built-in MIME types from 'defaultMimeTypes', serves
-- common index files @index.html@ and @index.htm@, but does not autogenerate
-- directory indexes, nor have any dynamic file handlers. The 'preServeHook'
-- will not do anything.
defaultDirectoryConfig :: MonadSnap m => DirectoryConfig m
defaultDirectoryConfig = DirectoryConfig {
    indexFiles = ["index.html", "index.htm"],
    indexGenerator = const pass,
    dynamicHandlers = Map.empty,
    mimeTypes = defaultMimeTypes,
    preServeHook = const $ return ()
    }


------------------------------------------------------------------------------
-- | A more elaborate configuration for file serving.  This configuration
-- uses built-in MIME types from 'defaultMimeTypes', serves common index files
-- @index.html@ and @index.htm@, and autogenerates directory indexes with a
-- Snap-like feel.  It still has no dynamic file handlers, nor 'preServeHook',
-- which should be added as needed.
--
-- Files recognized as indexes include @index.html@, @index.htm@,
-- @default.html@, @default.htm@, @home.html@
fancyDirectoryConfig :: MonadSnap m => DirectoryConfig m
fancyDirectoryConfig = DirectoryConfig {
    indexFiles = ["index.html", "index.htm"],
    indexGenerator = defaultIndexGenerator defaultMimeTypes snapIndexStyles,
    dynamicHandlers = Map.empty,
    mimeTypes = defaultMimeTypes,
    preServeHook = const $ return ()
    }


------------------------------------------------------------------------------
-- | Serves static files from a directory using the default configuration
-- as given in 'defaultDirectoryConfig'.
serveDirectory :: MonadSnap m
               => FilePath           -- ^ Directory to serve from
               -> m ()
serveDirectory = serveDirectoryWith defaultDirectoryConfig
{-# INLINE serveDirectory #-}


------------------------------------------------------------------------------
-- | Serves static files from a directory.  Configuration options are
-- passed in a 'DirectoryConfig' that captures various choices about desired
-- behavior.  The relative path given in 'rqPathInfo' is searched for a
-- requested file, and the file is served with the appropriate mime type if it
-- is found. Absolute paths and \"@..@\" are prohibited to prevent files from
-- being served from outside the sandbox.
serveDirectoryWith :: MonadSnap m
                   => DirectoryConfig m  -- ^ Configuration options
                   -> FilePath           -- ^ Directory to serve from
                   -> m ()
serveDirectoryWith cfg base = do
    b <- directory <|> file <|> redir
    when (not b) pass

  where

    idxs     = indexFiles cfg
    generate = indexGenerator cfg
    mimes    = mimeTypes cfg
    dyns     = dynamicHandlers cfg
    pshook   = preServeHook cfg

    -- Serves a file if it exists; passes if not
    serve f = do
        liftIO (doesFileExist f) >>= flip unless pass
        let fname          = takeFileName f
        let staticServe f' = pshook f >> serveFileAs (fileType mimes fname) f'
        lookupExt staticServe dyns fname f >> return True <|> return False

    -- Serves a directory via indices if available.  Returns True on success,
    -- False on failure to find an index.  Passes /only/ if the request was
    -- not for a directory (no trailing slash).
    directory = do
        rq  <- getRequest
        let uri = uriWithoutQueryString rq
        unless ("/" `S.isSuffixOf` uri) pass
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
        let uri = uriWithoutQueryString rq
        let qss = queryStringSuffix rq
        let u = S.concat [uri, "/", qss]
        redirect u


------------------------------------------------------------------------------
-- | Serves a single file specified by a full or relative path.  If the file
-- does not exist, throws an exception (not that it does /not/ pass to the
-- next handler).   The path restrictions on 'serveDirectory' don't apply to
-- this function since the path is not being supplied by the user.
serveFile :: MonadSnap m
          => FilePath          -- ^ path to file
          -> m ()
serveFile fp = serveFileAs (fileType defaultMimeTypes (takeFileName fp)) fp
{-# INLINE serveFile #-}


------------------------------------------------------------------------------
-- | Same as 'serveFile', with control over the MIME mapping used.
serveFileAs :: MonadSnap m
            => ByteString        -- ^ MIME type
            -> FilePath          -- ^ path to file
            -> m ()
serveFileAs mime fp = do
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
lookupExt :: a -> HashMap FilePath a -> FilePath -> a
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

        return $! RangeReq start end

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


------------------------------------------------------------------------------
uriWithoutQueryString :: Request -> ByteString
uriWithoutQueryString rq = S.takeWhile (/= '?') uri
  where
    uri   = rqURI rq


------------------------------------------------------------------------------
queryStringSuffix :: Request -> ByteString
queryStringSuffix rq = S.concat [ s, qs ]
  where
    qs = rqQueryString rq
    s  = if S.null qs then "" else "?"
