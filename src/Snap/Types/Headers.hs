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
import           Prelude                     (Bool (..), Eq (..), Maybe (..), Show (..), fst, id, map, otherwise, uncurry, ($), ($!), (.))
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | A key-value map that represents a collection of HTTP header fields. Keys
-- are case-insensitive.
newtype Headers = H { unH :: [(ByteString, ByteString)] }
  deriving (Show)


------------------------------------------------------------------------------
-- | An empty collection of HTTP header fields.
--
-- Example:
--
-- @
-- ghci> H.'empty'
-- H {unH = []}
-- @
empty :: Headers
empty = H []


------------------------------------------------------------------------------
-- | Is a given collection of HTTP header fields empty?
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> H.'null' H.'empty'
-- True
-- ghci> H.'null' $ H.'fromList' [(\"Host\", \"localhost\")]
-- False
-- @
null :: Headers -> Bool
null = List.null . unH
{-# INLINE null #-}


------------------------------------------------------------------------------
-- | Does this collection of HTTP header fields contain a given field?
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> H.'member' \"host\" $ H.'fromList' [(\"Host\", \"localhost\")]
-- True
-- ghci> H.'member' \"Accept\" $ H.'fromList' [(\"Host\", \"localhost\")]
-- False
-- @
member :: CI ByteString -> Headers -> Bool
member k0 = f . unH
  where
    k   = CI.foldedCase k0
    f m = List.any ((k ==) . fst) m
{-# INLINE member #-}


------------------------------------------------------------------------------
-- | Look up the value of a given HTTP header field.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> H.'lookup' \"host\" $ H.'fromList' [(\"Host\", \"localhost\")]
-- Just \"localhost\"
-- ghci> H.'lookup' \"Accept\" $ H.'fromList' [(\"Host\", \"localhost\")]
-- Nothing
-- @
lookup :: CI ByteString -> Headers -> Maybe ByteString
lookup k (H m) = List.lookup (CI.foldedCase k) m
{-# INLINE lookup #-}


------------------------------------------------------------------------------
-- | Look up the value of a given HTTP header field or return the provided
-- default value when that header field is not present.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> let hdrs = H.'fromList' [(\"Host\", \"localhost\")]
-- ghci> H.'lookupWithDefault' \"host\" \"127.0.0.1\" $ hdrs
-- \"localhost\"
-- ghci> H.'lookupWithDefault' \"Accept\" \"text\/plain\" $ hdrs
-- \"text\/plain\"
-- @
lookupWithDefault :: ByteString -> CI ByteString -> Headers -> ByteString
lookupWithDefault d k m = fromMaybe d $ lookup k m


------------------------------------------------------------------------------
-- | Insert a key-value pair into the headers map. If the key already exists in
-- the map, the values are catenated with ", ".
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> let hdrs = H.'insert' \"Accept\" \"text\/plain\" $ H.'empty'
-- ghci> hdrs
-- H {unH = [(\"accept\",\"text\/plain\")]}
-- ghci> H.'insert' \"Accept\" \"text\/html\" $ hdrs
-- H {unH = [(\"accept\",\"text\/plain,text\/html\")]}
-- @
insert :: CI ByteString -> ByteString -> Headers -> Headers
insert k0 v (H m) = H $! go id m
  where
    k = CI.foldedCase k0

    go dl []                       = dl [(k, v)]
    go dl (z@(x,y):xs) | k == x    = dl ((k, concatHeaderValues v y):xs)
                       | otherwise = go (dl . (z:)) xs

    concatHeaderValues :: ByteString -> ByteString -> ByteString
    concatHeaderValues new old = S.concat [old, ",", new]


------------------------------------------------------------------------------
-- | Insert a key-value pair into the headers map, without checking whether the
-- header already exists. The key /must/ be already case-folded, or none of the
-- lookups will work!
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> let hdrs = H.'unsafeInsert' \"accept\" \"text\/plain\" $ H.'empty'
-- ghci> hdrs
-- H {unH = [(\"accept\",\"text\/plain\")]}
-- ghci> let hdrs' = H.'unsafeInsert' \"accept\" \"text\/html\" $ hdrs
-- ghci> hdrs'
-- H {unH = [(\"accept\",\"text\/html\"), (\"accept\",\"text\/plain\")]}
-- ghci> H.'lookup' \"accept\" hdrs'
-- Just \"text\/html\"
-- @
unsafeInsert :: ByteString -> ByteString -> Headers -> Headers
unsafeInsert k v (H hdrs) = H ((k,v):hdrs)


------------------------------------------------------------------------------
-- | Set the value of a HTTP header field to a given value, replacing the old
-- value.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> H.'set' \"accept\" \"text\/plain\" $ H.'empty'
-- H {unH = [(\"accept\",\"text\/plain\")]}
-- ghci> H.'set' \"accept\" \"text\/html\" $ H.'fromList' [(\"Accept\", \"text\/plain\")]
-- H {unH = [(\"accept\",\"text\/html\")]}
-- @
set :: CI ByteString -> ByteString -> Headers -> Headers
set k0 v (H m) = H $ go m
  where
    k = CI.foldedCase k0

    go []                        = [(k,v)]
    go (x@(k',_):xs) | k == k'   = (k,v) : List.filter ((k /=) . fst) xs
                     | otherwise = x : go xs


------------------------------------------------------------------------------
-- | Delete all key-value pairs associated with the given key from the headers
-- map.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> H.'delete' \"accept\" $ H.'fromList' [(\"Accept\", \"text\/plain\")]
-- H {unH = []}
-- @
delete :: CI ByteString -> Headers -> Headers
delete k (H m) = H $ List.filter ((k' /=) . fst) m
  where
    k' = CI.foldedCase k


------------------------------------------------------------------------------
-- | Strict left fold over all key-value pairs in the headers map.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import "Data.Monoid"
-- ghci> let hdrs = H.'fromList' [(\"Accept\", \"text\/plain\"), (\"Accept\", \"text\/html\")]
-- ghci> let f (cntr, acc) _ val = (cntr+1, val <> \";\" <> acc)
-- ghci> H.'foldl'' f (0, \"\") hdrs
-- (2,\"text\/html;text\/plain;\")
-- @
foldl' :: (a -> CI ByteString -> ByteString -> a)
       -> a
       -> Headers
       -> a
foldl' f a (H m) = List.foldl' f' a m
  where
    f' v (x,y) = f v (CI.unsafeMk x) y


------------------------------------------------------------------------------
-- | Same as 'foldl'', but the key parameter is of type 'ByteString' instead of
-- 'CI' 'ByteString'. The key is case-folded (lowercase).
foldedFoldl' :: (a -> ByteString -> ByteString -> a)
             -> a
             -> Headers
             -> a
foldedFoldl' f a (H m) = List.foldl' f' a m
  where
    f' v (x,y) = f v x y


------------------------------------------------------------------------------
-- | Right fold over all key-value pairs in the headers map.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import "Data.Monoid"
-- ghci> let hdrs = H.'fromList' [(\"Accept\", \"text\/plain\"), (\"Accept\", \"text\/html\")]
-- ghci> let f _ val (cntr, acc) = (cntr+1, val <> \";\" <> acc)
-- ghci> H.'foldr' f (0, \"\") hdrs
-- (2,\"text\/plain;text\/html;\")
-- @
foldr :: (CI ByteString -> ByteString -> a -> a)
      -> a
      -> Headers
      -> a
foldr f a (H m) = List.foldr f' a m
  where
    f' (x, y) v = f (CI.unsafeMk x) y v


------------------------------------------------------------------------------
-- | Same as 'foldr', but the key parameter is of type 'ByteString' instead of
-- 'CI' 'ByteString'. The key is case-folded (lowercase).
foldedFoldr :: (ByteString -> ByteString -> a -> a)
            -> a
            -> Headers
            -> a
foldedFoldr f a (H m) = List.foldr (uncurry f) a m


------------------------------------------------------------------------------
-- | Convert a 'Headers' value to a list of key-value pairs.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> let l = [(\"Accept\", \"text\/plain\"), (\"Accept\", \"text\/html\")]
-- ghci> H.'toList' . H.'fromList' $ l
-- [(\"accept\",\"text\/plain\"),(\"accept\",\"text\/html\")]
-- @
toList :: Headers -> [(CI ByteString, ByteString)]
toList = map (first CI.unsafeMk) . unH


------------------------------------------------------------------------------
-- | Build a 'Headers' value from a list of key-value pairs.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> H.'fromList' [(\"Accept\", \"text\/plain\"), (\"Accept\", \"text\/html\")]
-- H {unH = [(\"accept\",\"text\/plain\"),(\"accept\",\"text\/html\")]}
-- @
fromList :: [(CI ByteString, ByteString)] -> Headers
fromList = H . map (first CI.foldedCase)


------------------------------------------------------------------------------
-- | Like 'fromList', but the keys are assumed to be already case-folded (in
-- lowercase).
unsafeFromCaseFoldedList :: [(ByteString, ByteString)] -> Headers
unsafeFromCaseFoldedList = H


------------------------------------------------------------------------------
-- | Like 'toList', but does not convert the keys to 'CI' 'ByteString', so key
-- comparisons will be case-sensitive.
unsafeToCaseFoldedList :: Headers -> [(ByteString, ByteString)]
unsafeToCaseFoldedList = unH
