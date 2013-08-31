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
import qualified Data.List             as List
import           Data.Maybe            (fromMaybe, isJust)
import           Prelude               hiding (foldr, lookup, null)

------------------------------------------------------------------------------
newtype Headers = H { unH :: [(CI ByteString, ByteString)] }
  deriving (Show)


------------------------------------------------------------------------------
empty :: Headers
empty = H []


------------------------------------------------------------------------------
null :: Headers -> Bool
null = List.null . unH
{-# INLINE null #-}


------------------------------------------------------------------------------
member :: CI ByteString -> Headers -> Bool
member k = f . unH
  where
    f m = List.any ((k ==) . fst) m
{-# INLINE member #-}


------------------------------------------------------------------------------
lookup :: CI ByteString -> Headers -> Maybe ByteString
lookup k (H m) = List.lookup k m
{-# INLINE lookup #-}


------------------------------------------------------------------------------
lookupWithDefault :: ByteString -> CI ByteString -> Headers -> ByteString
lookupWithDefault d k m = fromMaybe d $ lookup k m


------------------------------------------------------------------------------
insert :: CI ByteString -> ByteString -> Headers -> Headers
insert k v (H m) = H $ (k, v):m


------------------------------------------------------------------------------
set :: CI ByteString -> ByteString -> Headers -> Headers
set k v (H m) = H $ go m
  where
    go []                        = [(k,v)]
    go (x@(k',_):xs) | k == k'   = (k,v) : List.filter ((k /=) . fst) xs
                     | otherwise = x : go xs


------------------------------------------------------------------------------
delete :: CI ByteString -> Headers -> Headers
delete k (H m) = H $ List.filter ((k /=) . fst) m


------------------------------------------------------------------------------
foldl' :: (a -> CI ByteString -> ByteString -> a)
       -> a
       -> Headers
       -> a
foldl' f a (H m) = List.foldl' f' a m
  where
    f' v (x,y) = f v x y


------------------------------------------------------------------------------
foldr :: (CI ByteString -> ByteString -> a -> a)
      -> a
      -> Headers
      -> a
foldr f a (H m) = List.foldr (uncurry f) a m


------------------------------------------------------------------------------
toList :: Headers -> [(CI ByteString, ByteString)]
toList = unH


------------------------------------------------------------------------------
fromList :: [(CI ByteString, ByteString)] -> Headers
fromList = H


------------------------------------------------------------------------------
concatHeaderValues :: ByteString -> ByteString -> ByteString
concatHeaderValues new old = S.concat [old, ",", new]
