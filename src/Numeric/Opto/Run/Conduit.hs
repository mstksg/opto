{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Numeric.Opto.Run.Conduit
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Conduits that are useful for sampling and running optimizers.
module Numeric.Opto.Run.Conduit (
  -- * Sampling conduits
    shuffling
  , shufflingN
  , sinkSampleReservoir
  , samplingN
  , skipSampling
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import           Data.Foldable
import           Data.Maybe
import qualified Data.Conduit.Combinators           as C
import qualified Data.Vector                        as V
import qualified Data.Vector.Generic                as VG
import qualified Data.Vector.Generic.Mutable        as VG
import qualified System.Random.MWC                  as MWC
import qualified System.Random.MWC.Distributions    as MWC

-- | Outputs a shuffled version of the input stream.  Keeps entire input
-- stream in memory.
--
-- NOTE: Pulls the entire input stream into memory first before outputting
-- anything.
shuffling
    :: PrimMonad m
    => MWC.Gen (PrimState m)
    -> ConduitT a a m ()
shuffling g = do
    v <- C.sinkVector @V.Vector
    C.yieldMany =<< MWC.uniformShuffle v g

-- | Takes the first N items out of the input stream, shuffles them
-- in-memory, and outputs the shuffled result.
--
-- Leaves the rest of the items in the stream.
--
-- Use 'forever' to repeat multiple times until the stream is exhausted.
shufflingN
    :: PrimMonad m
    => Int
    -> MWC.Gen (PrimState m)
    -> ConduitT a a m ()
shufflingN n g = do
    v <- C.sinkVectorN @V.Vector n
    C.yieldMany =<< MWC.uniformShuffle v g

-- | Process an entire stream, and keep N random and shuffled items from
-- that stream.  Is O(N) memory.
sinkSampleReservoir
    :: forall m v a o. (PrimMonad m, VG.Vector v a)
    => Int
    -> MWC.Gen (PrimState m)
    -> ConduitT a o m (v a)
sinkSampleReservoir k g = do
    xs <- VG.thaw . VG.fromList . catMaybes =<< replicateM k await
    void . runMaybeT . for_ [k+1 ..] $ \i -> do
      x <- MaybeT await
      lift . lift $ do
        j <- MWC.uniformR (1, i) g
        when (j <= k) $
          VG.unsafeWrite xs (j - 1) x
    lift $ VG.freeze xs

-- | Process an entire stream, and yield N random items from that stream.
-- Is O(N) memory.
--
-- NOTE: Exhausts the entire input stream first before outputting anything,
-- but never keeps the entire original stream in memory.
samplingN
    :: PrimMonad m
    => Int
    -> MWC.Gen (PrimState m)
    -> ConduitT a a m ()
samplingN k = C.yieldMany <=< sinkSampleReservoir @_ @V.Vector k

-- | Drops and lets items through randomly with a given probability.
skipSampling
    :: PrimMonad m
    => Double               -- ^ probability of pass-through
    -> MWC.Gen (PrimState m)
    -> ConduitT a a m ()
skipSampling λ g = go
  where
    go = do
      n <- MWC.geometric0 λ g
      C.drop n
      mx <- await
      case mx of
        Just x  -> yield x >> go
        Nothing -> return ()
