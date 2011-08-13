{-# LANGUAGE BangPatterns #-}

-- | An opaque data type for HTTP headers. Intended to be imported qualified,
-- i.e:
--
-- > import           Snap.Types.Headers (Headers)
-- > import qualified Snap.Types.Headers as H
-- >
-- > foo :: Headers
-- > foo = H.empty

module Snap.Types.Headers
  ( -- * Headers type
    Headers

    -- * Headers creation
  , empty

    -- * Predicates
  , null
  , member

    -- * Lookup
  , lookup
  , lookupWithDefault

    -- * Adding/setting headers
  , insert
  , set

    -- * Deleting
  , delete

    -- * Traversal
  , fold

    -- * Lists
  , toList
  , fromList

  ) where

import           Data.ByteString.Char8 (ByteString)
import           Data.CaseInsensitive   (CI)
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding (null, lookup)

------------------------------------------------------------------------------
newtype Headers = H { unH :: Map (CI ByteString) [ByteString] }
  deriving (Show)

------------------------------------------------------------------------------
empty :: Headers
empty = H (Map.empty)


------------------------------------------------------------------------------
null :: Headers -> Bool
null = Map.null . unH
{-# INLINE null #-}


------------------------------------------------------------------------------
member :: CI ByteString -> Headers -> Bool
member k = Map.member k . unH
{-# INLINE member #-}


------------------------------------------------------------------------------
lookup :: CI ByteString -> Headers -> Maybe [ByteString]
lookup k (H m) = Map.lookup k m
{-# INLINE lookup #-}


------------------------------------------------------------------------------
lookupWithDefault :: ByteString -> CI ByteString -> Headers -> [ByteString]
lookupWithDefault d k (H m) = Map.findWithDefault [d] k m


------------------------------------------------------------------------------
insert :: CI ByteString -> ByteString -> Headers -> Headers
insert k v (H m) = H $ Map.insertWith' (flip (++)) k [v] m


------------------------------------------------------------------------------
set :: CI ByteString -> ByteString -> Headers -> Headers
set k v (H m) = H $ Map.insert k [v] m


------------------------------------------------------------------------------
delete :: CI ByteString -> Headers -> Headers
delete k (H m) = H $ Map.delete k m


------------------------------------------------------------------------------
fold :: (a -> CI ByteString -> [ByteString] -> a)
     -> a
     -> Headers
     -> a
fold f a (H m) = Map.foldlWithKey f a m


------------------------------------------------------------------------------
toList :: Headers -> [(CI ByteString, ByteString)]
toList (H m) = (Map.foldlWithKey f id m) []
  where
    f !dl k vs = dl . ((map (\v -> (k,v)) vs) ++)


------------------------------------------------------------------------------
fromList :: [(CI ByteString, ByteString)] -> Headers
fromList = foldl' f empty
  where
    f m (k,v) = insert k v m
