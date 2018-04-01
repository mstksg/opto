{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run.Conduit (
    runOptoConduit
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

runOptoConduit
    :: Monad m
    => RunOpts (SampleConduit i a m) a
    -> a
    -> OptoM (SampleConduit i a m) v a
    -> ConduitT i a m (OptoM (SampleConduit i a m) v a)
runOptoConduit ro x0 o0 = do
    (x, o) <- fmap (fromMaybe (x0, o0))
            . runSampleConduit
            $ runOptoMany ro x0 o0
    yield x
    return o

-- | Outputs a shuffled version of the input stream.  Keeps entire input
-- stream in memory.
--
-- NOTE: Pulls the input stream first before outputting anything.
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

sinkSampleReservoir
    :: forall m v a o. (PrimMonad m, VG.Vector v a)
    => Int
    -> MWC.Gen (PrimState m)
    -> ConduitT a o m (v a)
sinkSampleReservoir n = fmap (fromMaybe VG.empty)
                      . runSampleConduit
                      . sampleReservoir n

samplingN
    :: PrimMonad m
    => Int
    -> MWC.Gen (PrimState m)
    -> ConduitT a a m ()
samplingN k = yieldMany <=< sinkSampleReservoir @_ @V.Vector k
