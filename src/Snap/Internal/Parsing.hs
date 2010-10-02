{-# LANGUAGE OverloadedStrings #-}

module Snap.Internal.Parsing where

import           Control.Monad
import           Data.Attoparsec.Char8 hiding (Done)
import qualified Data.Attoparsec.Char8 as Atto
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Nums.Careless.Int (int)
import           Data.Int

------------------------------------------------------------------------------
fullyParse :: ByteString -> Parser a -> Either String a
fullyParse s p =
    case r' of
      (Fail _ _ e)    -> Left e
      (Partial _)     -> Left "parse failed"
      (Atto.Done _ x) -> Right x
  where
    r  = parse p s
    r' = feed r ""


------------------------------------------------------------------------------
parseNum :: Parser Int64
parseNum = liftM int $ Atto.takeWhile1 Atto.isDigit
