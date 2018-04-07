{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run.Conduit (
  -- * Running 'Opto's
    RunOpts(..)
  , runOptoConduit
  , runOptoConduit_
  , runOptoConduitChunk
  , runOptoConduitChunk_
  -- * Sampling conduits
  , shuffling
  , shufflingN
  , sinkSampleReservoir
  , samplingN
  , skipSampling
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Sample
import           Data.Conduit
import           Data.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Run
import qualified Data.Conduit.Combinators        as C
import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as VG
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC

-- | With 'RunOpts', a chunk size, an initial input, and a /sampling/
-- optimizer, give a conduit that processes upstream samples and outputs
-- every single one of the updated values as they are generated.
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

-- | 'runOptoConduit', without returning the updated optimizer state.
runOptoConduit_
    :: Monad m
    => RunOpts (SampleConduit r a m) a
    -> a
    -> OptoM (SampleConduit r a m) v a
    -> ConduitT r a m ()
runOptoConduit_ ro x0 = void . runOptoConduit ro x0

-- | With 'RunOpts', a chunk size, an initial input, and a /sampling/
-- optimizer, give a conduit that processes upstream samples and outputs
-- the updated value only /after/ the optimizer finishes.
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

-- | 'runOptoConduitChunk', without returning the updated optimizer state.
runOptoConduitChunk_
    :: Monad m
    => RunOpts (SampleConduit r a m) a
    -> a
    -> OptoM (SampleConduit r a m) v a
    -> ConduitT r a m ()
runOptoConduitChunk_ ro x0 = void . runOptoConduitChunk ro x0

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
