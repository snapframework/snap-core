{-# LANGUAGE OverloadedStrings #-}
-- | Add <http://www.w3.org/TR/cors/ CORS> (cross-origin resource sharing)
-- headers to a Snap application. CORS headers can be added either conditionally
-- or unconditionally to the entire site, or you can apply CORS headers to a
-- single route.
--
-- To use in a snaplet, simply use 'wrapSite':
--
-- @
-- wrapSite $ applyCORS defaultOptions
-- @
module Snap.Util.CORS
  ( -- * Applying CORS to a specific response
    applyCORS

    -- * Option Specification
  , CORSOptions(..)
  , defaultOptions

    -- ** Origin lists
  , OriginList(..)
  , OriginSet, mkOriginSet, origins

    -- * Internals
  , HashableURI(..), HashableMethod (..)
  ) where

import Control.Applicative
import Control.Monad (join, when)
import Data.CaseInsensitive (CI)
import Data.Hashable (Hashable(..))
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.URI (URI (..), URIAuth (..),  parseURI)

import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString.Char8 as S
import qualified Data.CaseInsensitive as CI
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import qualified Snap.Core as Snap
import Snap.Internal.Parsing (pTokens)

-- | A set of origins. RFC 6454 specifies that origins are a scheme, host and
-- port, so the 'OriginSet' wrapper around a 'HashSet.HashSet' ensures that each
-- 'URI' constists of nothing more than this.
newtype OriginSet = OriginSet { origins :: HashSet.HashSet HashableURI }

-- | Used to specify the contents of the @Access-Control-Allow-Origin@ header.
data OriginList
  = Everywhere
  -- ^ Allow any origin to access this resource. Corresponds to
  -- @Access-Control-Allow-Origin: *@
  | Nowhere
  -- ^ Do not allow cross-origin requests
  | Origins OriginSet
  -- ^ Allow cross-origin requests from these origins.

-- | Specify the options to use when building CORS headers for a response. Most
-- of these options are 'Snap.Handler' actions to allow you to conditionally
-- determine the setting of each header.
data CORSOptions m = CORSOptions
  { corsAllowOrigin :: m OriginList
  -- ^ Which origins are allowed to make cross-origin requests.

  , corsAllowCredentials :: m Bool
  -- ^ Whether or not to allow exposing the response when the omit credentials
  -- flag is unset.

  , corsExposeHeaders :: m (HashSet.HashSet (CI S.ByteString))
  -- ^ A list of headers that are exposed to clients. This allows clients to
  -- read the values of these headers, if the response includes them.

  , corsAllowedMethods :: m (HashSet.HashSet HashableMethod)
  -- ^ A list of request methods that are allowed.

  , corsAllowedHeaders :: HashSet.HashSet S.ByteString -> m (HashSet.HashSet S.ByteString)
  -- ^ An action to determine which of the request headers are allowed.
  -- This action is supplied the parsed contents of
  -- @Access-Control-Request-Headers@.
  }

-- | Liberal default options. Specifies that:
--
-- * All origins may make cross-origin requests
-- * @allow-credentials@ is true.
-- * No extra headers beyond simple headers are exposed.
-- * @GET@, @POST@, @PUT@, @DELETE@ and @HEAD@ are all allowed.
-- * All request headers are allowed.
--
-- All options are determined unconditionally.
defaultOptions :: Monad m => CORSOptions m
defaultOptions = CORSOptions
  { corsAllowOrigin = return Everywhere
  , corsAllowCredentials = return True
  , corsExposeHeaders = return HashSet.empty
  , corsAllowedMethods = return $! defaultAllowedMethods
  , corsAllowedHeaders = return
  }

defaultAllowedMethods :: HashSet.HashSet HashableMethod
defaultAllowedMethods = HashSet.fromList $ map HashableMethod
        [ Snap.GET, Snap.POST, Snap.PUT, Snap.DELETE, Snap.HEAD ]


-- | Apply CORS headers to a specific request. This is useful if you only have
-- a single action that needs CORS headers, and you don't want to pay for
-- conditional checks on every request.
--
-- You should note that 'applyCORS' needs to be used before you add any
-- 'Snap.method' combinators. For example, the following won't do what you want:
--
-- > method POST $ applyCORS defaultOptions $ myHandler
--
-- This fails to work as CORS requires an @OPTIONS@ request in the preflighting
-- stage, but this would get filtered out. Instead, use
--
-- > applyCORS defaultOptions $ method POST $ myHandler
applyCORS :: Snap.MonadSnap m => CORSOptions m -> m () -> m ()
applyCORS options m =
  (join . fmap decodeOrigin <$> getHeader "Origin") >>= maybe m corsRequestFrom

 where
  corsRequestFrom origin = do
    originList <- corsAllowOrigin options
    if origin `inOriginList` originList
       then Snap.method Snap.OPTIONS (preflightRequestFrom origin)
              <|> handleRequestFrom origin
       else m

  preflightRequestFrom origin = do
    maybeMethod <- fmap (parseMethod . S.unpack) <$>
                     getHeader "Access-Control-Request-Method"

    case maybeMethod of
      Nothing -> m

      Just method -> do
        allowedMethods <- corsAllowedMethods options

        if method `HashSet.member` allowedMethods
          then do
            maybeHeaders <-
              fromMaybe (Just HashSet.empty) . fmap splitHeaders
                <$> getHeader "Access-Control-Request-Headers"

            case maybeHeaders of
              Nothing -> m
              Just headers -> do
                allowedHeaders <- corsAllowedHeaders options headers

                if not $ HashSet.null $
                     headers `HashSet.difference` allowedHeaders
                   then m
                   else do
                     addAccessControlAllowOrigin origin
                     addAccessControlAllowCredentials

                     commaSepHeader
                       "Access-Control-Allow-Headers"
                       id (HashSet.toList allowedHeaders)

                     commaSepHeader
                       "Access-Control-Allow-Methods"
                       (S.pack . show) (HashSet.toList allowedMethods)

          else m

  handleRequestFrom origin = do
    addAccessControlAllowOrigin origin
    addAccessControlAllowCredentials

    exposeHeaders <- corsExposeHeaders options
    when (not $ HashSet.null exposeHeaders) $
      commaSepHeader
        "Access-Control-Expose-Headers"
        CI.original (HashSet.toList exposeHeaders)

    m

  addAccessControlAllowOrigin origin =
    addHeader "Access-Control-Allow-Origin"
              (encodeUtf8 $ Text.pack $ show origin)

  addAccessControlAllowCredentials = do
    allowCredentials <- corsAllowCredentials options
    when (allowCredentials) $
      addHeader "Access-Control-Allow-Credentials" "true"

  decodeOrigin :: S.ByteString -> Maybe URI
  decodeOrigin = fmap simplifyURI . parseURI . Text.unpack . decodeUtf8

  addHeader k v = Snap.modifyResponse (Snap.addHeader k v)

  commaSepHeader k f vs =
    case vs of
      [] -> return ()
      _  -> addHeader k $ S.intercalate ", " (map f vs)

  getHeader = Snap.getsRequest . Snap.getHeader

  splitHeaders = either (const Nothing) (Just . HashSet.fromList) .
    Attoparsec.parseOnly pTokens

mkOriginSet :: [URI] -> OriginSet
mkOriginSet = OriginSet . HashSet.fromList .
              map (HashableURI . simplifyURI)

simplifyURI :: URI -> URI
simplifyURI uri = uri { uriAuthority =
                          fmap simplifyURIAuth (uriAuthority uri)
                       , uriPath = ""
                       , uriQuery = ""
                       , uriFragment = ""
                       }
 where simplifyURIAuth auth = auth { uriUserInfo = "" }

--------------------------------------------------------------------------------
parseMethod :: String -> HashableMethod
parseMethod "GET"     = HashableMethod Snap.GET
parseMethod "POST"    = HashableMethod Snap.POST
parseMethod "HEAD"    = HashableMethod Snap.HEAD
parseMethod "PUT"     = HashableMethod Snap.PUT
parseMethod "DELETE"  = HashableMethod Snap.DELETE
parseMethod "TRACE"   = HashableMethod Snap.TRACE
parseMethod "OPTIONS" = HashableMethod Snap.OPTIONS
parseMethod "CONNECT" = HashableMethod Snap.CONNECT
parseMethod "PATCH"   = HashableMethod Snap.PATCH
parseMethod s         = HashableMethod $ Snap.Method (S.pack s)

--------------------------------------------------------------------------------
-- | A @newtype@ over 'URI' with a 'Hashable' instance.
newtype HashableURI = HashableURI URI
  deriving (Eq)

instance Show HashableURI where
  show (HashableURI u) = show u

instance Hashable HashableURI where
  hashWithSalt s (HashableURI (URI scheme authority path query fragment)) =
    s `hashWithSalt`
    scheme `hashWithSalt`
    fmap hashAuthority authority `hashWithSalt`
    path `hashWithSalt`
    query `hashWithSalt`
    fragment

   where
    hashAuthority (URIAuth userInfo regName port) =
          s `hashWithSalt`
          userInfo `hashWithSalt`
          regName `hashWithSalt`
          port

inOriginList :: URI -> OriginList -> Bool
_ `inOriginList` Nowhere = False
_ `inOriginList` Everywhere = True
origin `inOriginList` (Origins (OriginSet xs)) =
  HashableURI origin `HashSet.member` xs


--------------------------------------------------------------------------------
newtype HashableMethod = HashableMethod Snap.Method
  deriving (Eq)

instance Hashable HashableMethod where
  hashWithSalt s (HashableMethod Snap.GET)        = s `hashWithSalt` (0 :: Int)
  hashWithSalt s (HashableMethod Snap.HEAD)       = s `hashWithSalt` (1 :: Int)
  hashWithSalt s (HashableMethod Snap.POST)       = s `hashWithSalt` (2 :: Int)
  hashWithSalt s (HashableMethod Snap.PUT)        = s `hashWithSalt` (3 :: Int)
  hashWithSalt s (HashableMethod Snap.DELETE)     = s `hashWithSalt` (4 :: Int)
  hashWithSalt s (HashableMethod Snap.TRACE)      = s `hashWithSalt` (5 :: Int)
  hashWithSalt s (HashableMethod Snap.OPTIONS)    = s `hashWithSalt` (6 :: Int)
  hashWithSalt s (HashableMethod Snap.CONNECT)    = s `hashWithSalt` (7 :: Int)
  hashWithSalt s (HashableMethod Snap.PATCH)      = s `hashWithSalt` (8 :: Int)
  hashWithSalt s (HashableMethod (Snap.Method m)) =
    s `hashWithSalt` (9 :: Int) `hashWithSalt` m

instance Show HashableMethod where
  show (HashableMethod m) = show m
