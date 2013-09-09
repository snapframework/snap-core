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
  , unsafeInsert
  , set

    -- * Deleting
  , delete

    -- * Traversal
  , foldl'
  , foldr
  , foldedFoldl'
  , foldedFoldr

    -- * Lists
  , toList
  , fromList

  , unsafeFromCaseFoldedList
  , unsafeToCaseFoldedList

  ) where

------------------------------------------------------------------------------
import           Control.Arrow               (first)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as S
import           Data.CaseInsensitive        (CI)
import qualified Data.CaseInsensitive        as CI
import qualified Data.CaseInsensitive.Unsafe as CI
import qualified Data.List                   as List
import           Data.Maybe                  (fromMaybe)
import           Prelude                     hiding (foldr, lookup, null)

------------------------------------------------------------------------------
newtype Headers = H { unH :: [(ByteString, ByteString)] }
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
member k0 = f . unH
  where
    k   = CI.foldedCase k0
    f m = List.any ((k ==) . fst) m
{-# INLINE member #-}


------------------------------------------------------------------------------
lookup :: CI ByteString -> Headers -> Maybe ByteString
lookup k (H m) = List.lookup (CI.foldedCase k) m
{-# INLINE lookup #-}


------------------------------------------------------------------------------
lookupWithDefault :: ByteString -> CI ByteString -> Headers -> ByteString
lookupWithDefault d k m = fromMaybe d $ lookup k m


------------------------------------------------------------------------------
-- | Insert a key-value into the headers map. If the key already exists in the
-- map, the values are catenated with ", ".
insert :: CI ByteString -> ByteString -> Headers -> Headers
insert k0 v (H m) = H $! go id m
  where
    k = CI.foldedCase k0

    go dl []                       = dl [(k, v)]
    go dl (z@(x,y):xs) | k == x    = dl ((k, concatHeaderValues v y):xs)
                       | otherwise = go (dl . (z:)) xs


------------------------------------------------------------------------------
-- | Insert a key-value into the headers map, without checking whether the
-- header already exists. The key /must/ be already case-folded, or none of the
-- lookups will work!
unsafeInsert :: ByteString -> ByteString -> Headers -> Headers
unsafeInsert k v (H hdrs) = H ((k,v):hdrs)


------------------------------------------------------------------------------
set :: CI ByteString -> ByteString -> Headers -> Headers
set k0 v (H m) = H $ go m
  where
    k = CI.foldedCase k0

    go []                        = [(k,v)]
    go (x@(k',_):xs) | k == k'   = (k,v) : List.filter ((k /=) . fst) xs
                     | otherwise = x : go xs


------------------------------------------------------------------------------
delete :: CI ByteString -> Headers -> Headers
delete k (H m) = H $ List.filter ((k' /=) . fst) m
  where
    k' = CI.foldedCase k


------------------------------------------------------------------------------
foldl' :: (a -> CI ByteString -> ByteString -> a)
       -> a
       -> Headers
       -> a
foldl' f a (H m) = List.foldl' f' a m
  where
    f' v (x,y) = f v (CI.unsafeMk x) y


------------------------------------------------------------------------------
foldedFoldl' :: (a -> ByteString -> ByteString -> a)
             -> a
             -> Headers
             -> a
foldedFoldl' f a (H m) = List.foldl' f' a m
  where
    f' v (x,y) = f v x y


------------------------------------------------------------------------------
foldr :: (CI ByteString -> ByteString -> a -> a)
      -> a
      -> Headers
      -> a
foldr f a (H m) = List.foldr f' a m
  where
    f' (x, y) v = f (CI.unsafeMk x) y v


------------------------------------------------------------------------------
foldedFoldr :: (ByteString -> ByteString -> a -> a)
            -> a
            -> Headers
            -> a
foldedFoldr f a (H m) = List.foldr (uncurry f) a m


------------------------------------------------------------------------------
toList :: Headers -> [(CI ByteString, ByteString)]
toList = map (first CI.unsafeMk) . unH


------------------------------------------------------------------------------
fromList :: [(CI ByteString, ByteString)] -> Headers
fromList = H . map (first CI.foldedCase)


------------------------------------------------------------------------------
unsafeFromCaseFoldedList :: [(ByteString, ByteString)] -> Headers
unsafeFromCaseFoldedList = H


------------------------------------------------------------------------------
unsafeToCaseFoldedList :: Headers -> [(ByteString, ByteString)]
unsafeToCaseFoldedList = unH


------------------------------------------------------------------------------
concatHeaderValues :: ByteString -> ByteString -> ByteString
concatHeaderValues new old = S.concat [old, ",", new]
