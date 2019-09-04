{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | New Cookie implementation.

module Snap.Cookie where

------------------------------------------------------------------------------
import           Control.Lens.TH (makeLenses)
import           Data.ByteString (ByteString, empty)
import           Data.Default
import           Data.Time.Clock (UTCTime)


------------------------------------------------------------------------------
-- | SameSite strictness policy.
data SameSite = Lax | Strict
                deriving (Eq, Show)


------------------------------------------------------------------------------
-- | A datatype representing an HTTP cookie.
data Cookie = Cookie {
      -- | The name of the cookie.
      _cookieName     :: !ByteString

      -- | The cookie's string value.
    , _cookieValue    :: !ByteString

      -- | The cookie's expiration value, if it has one.
    , _cookieExpires  :: !(Maybe UTCTime)

      -- | The cookie's \"domain\" value, if it has one.
    , _cookieDomain   :: !(Maybe ByteString)

      -- | The cookie path.
    , _cookiePath     :: !(Maybe ByteString)

      -- | Tag as secure cookie?
    , _cookieSecure   :: !Bool

      -- | HTTP only?
    , _cookieHttpOnly :: !Bool

      -- | SameSite strictness policy.
    , _cookieSameSite :: !(Maybe SameSite)
} deriving (Eq, Show)


makeLenses ''Cookie


------------------------------------------------------------------------------
instance Default Cookie where
  def = Cookie empty empty Nothing Nothing Nothing False False Nothing
