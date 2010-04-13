{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | "Data.CIByteString" is a module containing 'CIByteString', a wrapper for
-- 'ByteString' which provides case-insensitive (ASCII-wise) 'Ord' and 'Eq'
-- instances.
--
-- 'CIByteString' also has an 'IsString' instance, so if you use the
-- \"OverloadedStrings\" LANGUAGE pragma you can write case-insensitive string
-- literals, e.g.:
--
-- @
-- \> let a = \"Foo\" in
--   putStrLn $ (show $ unCI a) ++ \"==\\\"FoO\\\" is \" ++ show (a == \"FoO\")
-- \"Foo\"==\"FoO\" is True
-- @

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

f = putStrLn $ (show $ unCI a) ++ " == " ++ (show $ unCI b) ++ " is " ++ show r
  where
    a = "foo"
    b = "FoO"
    r = a == b


-- | A case-insensitive newtype wrapper for 'ByteString'
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
