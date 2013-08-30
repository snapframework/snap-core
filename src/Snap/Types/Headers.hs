{-# LANGUAGE OverloadedStrings #-}

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
  , foldl'
  , foldr

    -- * Lists
  , toList
  , fromList

  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.CaseInsensitive  (CI)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as Map
import qualified Data.List             as List
import           Data.Maybe            (isJust)
import           Prelude               hiding (foldr, lookup, null)

------------------------------------------------------------------------------
newtype Headers = H { unH :: HashMap (CI ByteString) ByteString }
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
member k = f . unH
  where
    f m = isJust $ Map.lookup k m
{-# INLINE member #-}


------------------------------------------------------------------------------
lookup :: CI ByteString -> Headers -> Maybe ByteString
lookup k (H m) = Map.lookup k m
{-# INLINE lookup #-}


------------------------------------------------------------------------------
lookupWithDefault :: ByteString -> CI ByteString -> Headers -> ByteString
lookupWithDefault d k (H m) = Map.lookupDefault d k m


------------------------------------------------------------------------------
insert :: CI ByteString -> ByteString -> Headers -> Headers
insert k v (H m) = H $ Map.insertWith concatHeaderValues k v m


------------------------------------------------------------------------------
set :: CI ByteString -> ByteString -> Headers -> Headers
set k v (H m) = H $ Map.insert k v m


------------------------------------------------------------------------------
delete :: CI ByteString -> Headers -> Headers
delete k (H m) = H $ Map.delete k m


------------------------------------------------------------------------------
foldl' :: (a -> CI ByteString -> ByteString -> a)
       -> a
       -> Headers
       -> a
foldl' f a (H m) = Map.foldlWithKey' f a m


------------------------------------------------------------------------------
foldr :: (CI ByteString -> ByteString -> a -> a)
      -> a
      -> Headers
      -> a
foldr f a (H m) = Map.foldrWithKey f a m


------------------------------------------------------------------------------
toList :: Headers -> [(CI ByteString, ByteString)]
toList = Map.toList . unH


------------------------------------------------------------------------------
fromList :: [(CI ByteString, ByteString)] -> Headers
fromList = List.foldl' f empty
  where
    f m (k, v) = insert k v m


------------------------------------------------------------------------------
concatHeaderValues :: ByteString -> ByteString -> ByteString
concatHeaderValues new old = S.concat [old, ",", new]
