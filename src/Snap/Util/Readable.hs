module Snap.Util.Readable
  ( Readable(..)
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Text.Read
import           Data.Word


------------------------------------------------------------------------------
-- | Monadic analog to Read that uses ByteString instead of String.
class Readable a where
    fromBS :: Monad m => ByteString -> m a


------------------------------------------------------------------------------
-- | Fails if the input wasn't parsed completely.
checkComplete :: Monad m => (t, Text) -> m t
checkComplete (a,rest)
  | T.null rest = return a
  | otherwise   = fail "Readable: could not parse completely"


instance Readable ByteString where
    fromBS = return
instance Readable Text where
    fromBS = return . decodeUtf8
instance Readable Int where
    fromBS = either fail checkComplete . decimal . decodeUtf8
instance Readable Integer where
    fromBS = either fail checkComplete . decimal . decodeUtf8
instance Readable Double where
    fromBS = either fail checkComplete . double . decodeUtf8

instance Readable Int8 where
    fromBS = either fail checkComplete . decimal . decodeUtf8
instance Readable Int16 where
    fromBS = either fail checkComplete . decimal . decodeUtf8
instance Readable Int32 where
    fromBS = either fail checkComplete . decimal . decodeUtf8
instance Readable Int64 where
    fromBS = either fail checkComplete . decimal . decodeUtf8

instance Readable Word8 where
    fromBS = either fail checkComplete . decimal . decodeUtf8
instance Readable Word16 where
    fromBS = either fail checkComplete . decimal . decodeUtf8
instance Readable Word32 where
    fromBS = either fail checkComplete . decimal . decodeUtf8
instance Readable Word64 where
    fromBS = either fail checkComplete . decimal . decodeUtf8
