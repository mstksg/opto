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
  -- -- * Running 'Opto's
  -- -- ** Single threaded
  --   RunOpts(..)
  -- , runOptoConduit
  -- , runOptoConduit_
  -- , runOptoConduitChunk
  -- , runOptoConduitChunk_
  -- -- ** Parallel
  -- , ParallelOpts(..)

  -- * Sampling conduits
    shuffling
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
import qualified Data.Conduit.Combinators           as C
import qualified Data.Vector                        as V
import qualified Data.Vector.Generic                as VG
import qualified System.Random.MWC                  as MWC
import qualified System.Random.MWC.Distributions    as MWC

---- | With 'RunOpts', a chunk size, an initial input, and a /sampling/
---- optimizer, give a conduit that processes upstream samples and outputs
---- the updated value only /after/ the optimizer finishes.
----
---- Returns the updated optimizer state.
--runOptoParallelConduitChunk
--    :: MonadUnliftIO m
--    => RunOpts (SampleConduit r a m) a
--    -> ParallelOpts
--    -> a
--    -> OptoM (SampleConduit r a m) v a
--    -> ConduitT r a m (OptoM (SampleConduit r a m) v a)
--runOptoParallelConduitChunk ro po x0 o0 = do
--    tq <- lift newTQueueIO
--    replicateM_ 100 $ do
--      x <- await
--      mapM_ (lift . atomically . writeTQueue tq) x
--    undefined
--    -- forkIO $ _
--    -- _

    -- (x, o) <- fmap (fromMaybe (x0, o0))
    --         . runSampleConduit
    --         $ runOptoMany ro x0 o0
    -- yield x
    -- return o


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
--
-- Returns the entire stream shuffled if the stream has less than
-- N elements.
sinkSampleReservoir
    :: forall m v a o. (PrimMonad m, VG.Vector v a)
    => Int
    -> MWC.Gen (PrimState m)
    -> ConduitT a o m (v a)
sinkSampleReservoir k = fmap (fromMaybe VG.empty)
                      . runConduitSample
                      . sampleReservoir k

-- | Process an entire stream, and yield N random items from that stream.
-- Is O(N) memory.
--
-- Yields the entire input stream shuffled if the stream has less than
-- N elements.
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
