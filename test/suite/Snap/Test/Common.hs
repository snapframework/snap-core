{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Test.Common where

import           Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Internal (c2w)
import           Test.QuickCheck
import           Test.QuickCheck.Gen


instance Arbitrary S.ByteString where
    arbitrary = liftM (S.pack . map c2w) arbitrary

instance Arbitrary L.ByteString where
    arbitrary = do
        n      <- choose(0,5)
        chunks <- replicateM n arbitrary
        return $ L.fromChunks chunks

