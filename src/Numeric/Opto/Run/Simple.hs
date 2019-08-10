{-# LANGUAGE RankNTypes #-}

module Numeric.Opto.Run.Simple (
  -- * Simple conduit runners
    sampleCollect, trainReport
  -- * Simple callbacks for runners
  , dispEpoch, dispBatch
  , simpleReport
  ) where


import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Data.Conduit
import           Data.MonoTraversable
import           Data.Time
import           Numeric.Opto.Run.Conduit
import           Text.Printf
import qualified Data.Conduit.Combinators  as C
import qualified System.Random.MWC         as MWC

-- | Simple reporter to give to 'sampleCollect':
--
-- > [Epoch 1]
-- > [Epoch 2]
-- > [Epoch 3]
dispEpoch :: MonadIO m => Int -> m ()
dispEpoch = liftIO . printf "[Epoch %d]\n"

-- | Simple reporter to give to 'trainReport':
--
-- > (Batch 1)
-- > (Batch 2)
-- > (Batch 3)
dispBatch :: MonadIO m => Int -> m ()
dispBatch = liftIO . printf "(Batch %d)\n"

-- | A sample conduit 'Source' that yields shuffled items in a collection
-- repeatedly.  Deposits the yielded items in a 'TBQueue', and takes
-- a callback for reporting each new epoch (cycle of items in the
-- collection).
sampleCollect
    :: (PrimMonad m, MonadIO m, MonoFoldable t)
    => TBQueue (Element t)          -- ^ Queue to deposit samples
    -> Maybe Int                    -- ^ Number of epochs (Nothing for forever)
    -> (Int -> m ())                -- ^ Report each new epoch.  See 'dispEpoch' for a simple one.
    -> t                            -- ^ Collection to source
    -> MWC.Gen (PrimState m)        -- ^ Random shuffling generator
    -> ConduitM i (Element t) m ()
sampleCollect sampleQueue n report train g =
      forM_ iterator (\e -> lift (report e) >> C.yieldMany train .| shuffling g)
   .| C.iterM (liftIO . atomically . writeTBQueue sampleQueue)
  where
    iterator = case n of
      Nothing -> [1..]
      Just i  -> [1..i]

-- | Simple reporting callback to provide 'trainReport'.
simpleReport
    :: MonadIO m
    => Maybe [i]                -- ^ A "validation set" that's consistent across all batches.
    -> ([i] -> a -> String)     -- ^ A function to run to evaluate a set.
    -> NominalDiffTime          -- ^ Time it took to train
    -> [i]                      -- ^ Set of batch points
    -> a                        -- ^ New predictor
    -> m ()
simpleReport testSet testNet t chnk net = liftIO $ do
    printf "Trained on %d points in %s.\n"
      (length chnk)
      (show t)
    printf "Training>\t%s\n" trainScore
    forM_ testScore $ \ts ->
      printf "Validation>\t%s\n" ts
  where
    trainScore = testNet chnk net
    testScore  = (`testNet` net) <$> testSet

-- | A conduit that processes "trained" items and outputs a report every
-- batch, based on samples accumulated in a 'TBQueue'.  Meant to be used
-- alongside 'sampleCollect'.  It passes through all of its input.
--
-- @
--     'sampleCollect' sampleQueue epochs 'dispEpoch' samples
--  .| 'optoConduit' ro model0 optimizer
--  .| 'trainReport' sampleQueue 'dispBatch' 2500 ('simpleReport' ('Just' valSet) runTest)
--  .| sink -- (other stuff you would want to do with trained models)
-- @
trainReport
    :: (MonadIO m, NFData a)
    => TBQueue i                -- ^ Queue to acquire used training samples
    -> (Int -> m ())            -- ^ Report each new batch.  See 'dispBatch' for a simple one.
    -> Int                      -- ^ Number of samples to go through before running reporting function
    -> (NominalDiffTime -> [i] -> a -> m ())    -- ^ Reporting function.  See 'simpleReport' for a simple one.
    -> ConduitT a a m ()
trainReport sampleQueue db n reportFunc =
      forever (C.drop (n - 1) *> (mapM_ yield =<< await))
   .| mapM_ report [1..]
  where
    report b = do
      lift $ db b
      t0   <- liftIO getCurrentTime
      net' <- mapM (liftIO . evaluate . force) =<< await
      chnk <- liftIO . atomically $ flushTBQueue sampleQueue
      t1   <- liftIO getCurrentTime
      forM_ net' $ \net -> do
        lift $ reportFunc (t1 `diffUTCTime` t0) chnk net
        yield net
