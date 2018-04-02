{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run.Conduit (
    RunOpts(..)
  , runOptoConduit
  , runOptoConduitChunk
  , shuffling
  , shufflingN
  , sinkSampleReservoir
  , samplingN
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Sample
import           Data.Conduit
import           Data.Conduit.Combinators
import           Data.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Run
import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as VG
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC

-- | With 'RunOpts', a chunk size, an initial input, and a /sampling/
-- optimizer, give a conduit that processes upstream samples and outputs
-- all of the updated values as they come along.
--
-- Returns the updated optimizer state.
runOptoConduit
    :: Monad m
    => RunOpts (SampleConduit r a m) a
    -> a
    -> OptoM (SampleConduit r a m) v a
    -> ConduitT r a m (OptoM (SampleConduit r a m) v a)
runOptoConduit ro = runOptoConduitChunk ro'
  where
    ro' = ro { roStopCond = \d x -> do
                 SampleConduit . fmap Just $ yield x
                 roStopCond ro d x
             }

-- | With 'RunOpts', a chunk size, an initial input, and a /sampling/
-- optimizer, give a conduit that processes upstream samples and outputs
-- all of the updated values after the optimizer finishes.
--
-- Returns the updated optimizer state.
runOptoConduitChunk
    :: Monad m
    => RunOpts (SampleConduit r a m) a
    -> a
    -> OptoM (SampleConduit r a m) v a
    -> ConduitT r a m (OptoM (SampleConduit r a m) v a)
runOptoConduitChunk ro x0 o0 = do
    (x, o) <- fmap (fromMaybe (x0, o0))
            . runSampleConduit
            $ runOptoMany ro x0 o0
    yield x
    return o

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
    v <- sinkVector @V.Vector
    yieldMany =<< MWC.uniformShuffle v g

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
    v <- sinkVectorN @V.Vector n
    yieldMany =<< MWC.uniformShuffle v g

-- | Process an entire stream, and keep N random and shuffled items from
-- that stream.  Is O(N) memory.
sinkSampleReservoir
    :: forall m v a o. (PrimMonad m, VG.Vector v a)
    => Int
    -> MWC.Gen (PrimState m)
    -> ConduitT a o m (v a)
sinkSampleReservoir n = fmap (fromMaybe VG.empty)
                      . runSampleConduit
                      . sampleReservoir n

-- | Process an entire stream, and yield N random items from that stream.
-- Is O(N) memory.
--
-- NOTE: Exhausts the entire input stream first before outputting anything,
-- but never keeps the entire stream in memory.
samplingN
    :: PrimMonad m
    => Int
    -> MWC.Gen (PrimState m)
    -> ConduitT a a m ()
samplingN k = yieldMany <=< sinkSampleReservoir @_ @V.Vector k
