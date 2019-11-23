{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Snap.Test.Common
  ( coverEqInstance
  , coverOrdInstance
  , coverReadInstance
  , coverShowInstance
  , coverTypeableInstance
  , forceSameType
  , expectException
  , expectExceptionH
  , liftQ
  , eatException
  , waitabit
  , seconds
  ) where
------------------------------------------------------------------------------
import           Control.Concurrent          (threadDelay)
import           Control.DeepSeq             (deepseq)
import           Control.Exception.Lifted    (SomeException (..), catch, evaluate, try)
import           Control.Monad               (Monad ((>>), return), liftM, replicateM)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString             as S
import           Data.ByteString.Internal    (c2w)
import qualified Data.ByteString.Lazy        as L
import           Data.Typeable               (Typeable, typeOf)
import           Prelude                     (Either (..), Eq (..), IO, Int, Num (..), Ord (..), Ordering (..), Read (..), Show (..), fail, map, seq, ($), (.), (^))
import           Test.QuickCheck             (Arbitrary (arbitrary), choose)
import           Test.QuickCheck.Monadic     (PropertyM)
import qualified Test.QuickCheck.Monadic     as QC
------------------------------------------------------------------------------

------------------------------------------------------------------------------
instance Arbitrary S.ByteString where
    arbitrary = liftM (S.pack . map c2w) arbitrary

instance Arbitrary L.ByteString where
    arbitrary = do
        n      <- choose(0,5)
        chunks <- replicateM n arbitrary
        return $ L.fromChunks chunks


------------------------------------------------------------------------------
eatException :: (MonadBaseControl IO m) => m a -> m ()
eatException a = (a >> return ()) `catch` handler
  where
    handler :: (MonadBaseControl IO m) => SomeException -> m ()
    handler _ = return ()


------------------------------------------------------------------------------
forceSameType :: a -> a -> a
forceSameType _ a = a


------------------------------------------------------------------------------
-- | Kill the false negative on derived show instances.
coverShowInstance :: (Monad m, Show a) => a -> m ()
coverShowInstance x = a `deepseq` b `deepseq` c `deepseq` return ()
  where
    a = showsPrec 0 x ""
    b = show x
    c = showList [x] ""


------------------------------------------------------------------------------
coverReadInstance :: (MonadIO m, Read a) => a -> m ()
coverReadInstance x = do
    liftIO $ eatException $ evaluate $ forceSameType [(x,"")] $ readsPrec 0 ""
    liftIO $ eatException $ evaluate $ forceSameType [([x],"")] $ readList ""


------------------------------------------------------------------------------
coverEqInstance :: (Monad m, Eq a) => a -> m ()
coverEqInstance x = a `seq` b `seq` return ()
  where
    a = x == x
    b = x /= x


------------------------------------------------------------------------------
coverOrdInstance :: (Monad m, Ord a) => a -> m ()
coverOrdInstance x = a `deepseq` b `deepseq` return ()
  where
    a = [ x < x
        , x >= x
        , x > x
        , x <= x
        , compare x x == EQ ]

    b = min a $ max a a


------------------------------------------------------------------------------
coverTypeableInstance :: (Monad m, Typeable a) => a -> m ()
coverTypeableInstance a = typeOf a `seq` return ()


------------------------------------------------------------------------------
expectException :: IO a -> PropertyM IO ()
expectException m = do
    e <- liftQ $ try m
    case e of
      Left (z::SomeException)  -> (forceList $ show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"


------------------------------------------------------------------------------
expectExceptionH :: IO a -> IO ()
expectExceptionH act = do
    e <- try act
    case e of
      Left (z::SomeException) -> (forceList $ show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"


------------------------------------------------------------------------------
forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `seq` forceList xs


------------------------------------------------------------------------------
liftQ :: forall a m . (Monad m) => m a -> PropertyM m a
liftQ = QC.run


------------------------------------------------------------------------------
waitabit :: IO ()
waitabit = threadDelay $ 2*seconds


------------------------------------------------------------------------------
seconds :: Int
seconds = (10::Int) ^ (6::Int)

