{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Control.Monad.Sample (
  -- * Typeclasses
    MonadSample(..), flushSamples, sampleReservoir
  -- * Instances
  , ConduitSample(..), conduitSample
  , RefSample(..)
  , FoldSampleT(.., FoldSample, runFoldSample), FoldSample
  , foldSample
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Bifunctor
import           Data.Conduit
import           Data.Foldable
import           Data.Functor.Identity
import           Data.MonoTraversable
import           Data.Tuple
import           Numeric.Opto.Ref
import qualified Data.Sequence                   as Seq
import qualified Data.Sequences                  as O
import qualified Data.Vector.Generic             as VG
import qualified Data.Vector.Generic.Mutable     as VG
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC

-- | 'MonadPlus' to imply that an empty pool is empty forever unless you
-- re-fill it first.
class MonadPlus m => MonadSample r m | m -> r where
    sample  :: m r
    -- | Should never "fail"
    sampleN :: VG.Vector v r => Int -> m (v r)
    sampleN = fmap VG.fromList . go
      where
        go 0 = pure []
        go n = ((:) <$> sample <*> go (n - 1)) <|> pure []

flushSamples :: MonadSample r m => m [r]
flushSamples = many sample

-- | Pulls samples until it cannot pull any more, and returns N shuffled
-- items from the pool.  Is O(N) memory.
sampleReservoir
    :: forall m v r. (PrimMonad m, MonadSample r m, VG.Vector v r)
    => Int
    -> MWC.Gen (PrimState m)
    -> m (v r)
sampleReservoir k g = do
    xs <- VG.thaw =<< sampleN k
    MWC.uniformShuffleM xs g
    void . optional . for_ [k+1 ..] $ \i -> do
      x <- sample
      j <- MWC.uniformR (1, i) g
      when (j <= k) $
        VG.unsafeWrite xs (j-1) x
    VG.freeze xs

newtype ConduitSample i o m a = ConduitSample
    { runConduitSample :: ConduitT i o m (Maybe a) }
  deriving (Functor)
  deriving (Applicative, Monad, Alternative, MonadPlus, MonadIO) via (MaybeT (ConduitT i o m))

instance PrimMonad m => PrimMonad (ConduitSample i o m) where
    type PrimState (ConduitSample i o m) = PrimState m
    primitive = lift . primitive

instance MonadTrans (ConduitSample i o) where
    lift = ConduitSample . lift . fmap Just

conduitSample :: ConduitT i o m a -> ConduitSample i o m a
conduitSample = ConduitSample . fmap Just

instance Monad m => MonadSample i (ConduitSample i o m) where
    sample = ConduitSample await

newtype RefSample v r m a = RefSample
    { runRefSample :: v -> m (Maybe a) }
  deriving (Functor)
  deriving (Applicative, Monad, Alternative, MonadPlus, MonadIO) via (MaybeT (ReaderT v m))

instance PrimMonad m => PrimMonad (RefSample v r m) where
    type PrimState (RefSample v r m) = PrimState m
    primitive = lift . primitive

instance MonadTrans (RefSample v r) where
    lift x = RefSample $ \_ -> Just <$> x

instance (Monad m, Ref m rs v, Element rs ~ r, O.IsSequence rs, O.Index rs ~ Int) => MonadSample r (RefSample v r m) where
    sample = RefSample $ \v ->
      updateRef' v $ \xs -> case O.uncons xs of
        Nothing      -> (mempty, Nothing)
        Just (y, ys) -> (ys    , Just y)
    sampleN n = RefSample $ \v ->
      updateRef' v (second (Just . VG.fromList . O.unpack) . O.splitAt n)

newtype FoldSampleT rs m a = FoldSampleT
    { runFoldSampleT :: rs -> m (Maybe a, rs) }
  deriving (Functor)
  deriving (Applicative, Monad, Alternative, MonadPlus, MonadIO) via (MaybeT (StateT rs m))

instance MonadTrans (FoldSampleT rs) where
    lift x = FoldSampleT $ \rs -> ((,rs) . Just) <$> x

instance PrimMonad m => PrimMonad (FoldSampleT rs m) where
    type PrimState (FoldSampleT rs m) = PrimState m
    primitive = lift . primitive

type FoldSample rs = FoldSampleT rs Identity

pattern FoldSample :: (rs -> (Maybe a, rs)) -> FoldSample rs a
pattern FoldSample { runFoldSample } <- ((\sf -> runIdentity . runFoldSampleT sf)->runFoldSample)
  where
    FoldSample = foldSample

foldSample :: Monad m => (rs -> (Maybe a, rs)) -> FoldSampleT rs m a
foldSample = FoldSampleT . fmap return

instance (Monad m, Element rs ~ r, O.IsSequence rs, O.Index rs ~ Int) => MonadSample r (FoldSampleT rs m) where
    sample = foldSample $ \xs -> case O.uncons xs of
      Nothing      -> (Nothing, mempty)
      Just (y, ys) -> (Just y , ys)
    sampleN n = foldSample $
      first (Just . VG.fromList . O.unpack) . O.splitAt n
