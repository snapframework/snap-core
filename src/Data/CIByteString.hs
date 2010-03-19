{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.CIByteString
 ( CIByteString
 , toCI
 , unCI
 ) where

-- for IsString instance
import           Data.ByteString.Char8 ()
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString as S
import           Data.Char
import           Data.String

-- | A case-insensitive newtype wrapper for ByteString
data CIByteString = CIByteString { unCI        :: !ByteString
                                 , _lowercased :: !ByteString }

toCI :: ByteString -> CIByteString
toCI s = CIByteString s t
  where
    t = lowercase s

instance Show CIByteString where
    show (CIByteString s _) = show s

lowercase :: ByteString -> ByteString
lowercase = S.map (c2w . toLower . w2c)

instance Eq CIByteString where
    (CIByteString _ a) == (CIByteString _ b) = a == b
    (CIByteString _ a) /= (CIByteString _ b) = a /= b

instance Ord CIByteString where
    (CIByteString _ a) <= (CIByteString _ b) = a <= b

instance IsString CIByteString where
    fromString = toCI . fromString
