{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Numeric.Opto.Run.Simple (
  -- * Simple conduit runners
    sampleCollect, trainReport
  -- * Simple callbacks for runners
  , dispEpoch, dispBatch
  , simpleReport
  -- * Integrated runners
  , simpleRunner
  , SimpleOpts(..)
  , SOOptimizer(..)
  ) where


import           Control.Concurrent hiding (yield)
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Conduit
import           Data.Default
import           Data.Functor
import           Data.Kind
import           Data.MonoTraversable
import           Data.Time
import           Numeric.Opto.Core
import           Numeric.Opto.Run
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
    -> ConduitT i (Element t) m ()
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
-- report batch, based on samples accumulated in a 'TBQueue'.  Meant to be used
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

-- | Options for 'simpleRunner'.  'def' gives sensible defaults.
data SimpleOpts m i a b = SO
    { soEpochs    :: Maybe Int          -- ^ How many epochs (default: Nothing, forever)
    , soDispEpoch :: Int -> m ()        -- ^ Display a new epoch to the screen (default: "[Epoch %d]")
    , soDispBatch :: Int -> m ()        -- ^ Display a new report batch to the screen (defualt: "(Batch %d)")
    , soTestSet   :: Maybe [i]          -- ^ Test set to validate results (default: Nothing)
    , soSkipSamps :: Int                -- ^ Number of samples to skip per report batch (default: 1000)
    , soEvaluate  :: [i] -> a -> String     -- ^ Evaluate a model against samples and report accuracy. (default: do nothing)
    , soSink      :: ConduitT a Void m b    -- ^ Collect the optimized values (default: 'C.sinkNull', ignore them)
    }

-- | Choose a concurrency strategy for your runner.
data SOOptimizer :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type where
    -- | Single-threaded
    SOSingle     :: SOOptimizer m (ConduitT i a m) i a
    -- | Parallel
    SOParallel   :: MonadUnliftIO m => ParallelOpts m a -> SOOptimizer m m i a
    -- | Parallel chunked
    SOParChunked :: MonadUnliftIO m => ParallelOpts m a -> SOOptimizer m (StateT [i] m) i a

-- | Run an 'SOOptimizer' concurrency strategy, transforming a sample
-- source.
runSOOptimizer
    :: MonadIO m
    => SOOptimizer m n i a
    -> RunOpts m a
    -> a
    -> Opto n i a
    -> ConduitT () i m ()
    -> ConduitT () a m ()
runSOOptimizer = \case
    SOSingle        -> \ro x0 o -> (.| optoConduit ro x0 o)
    SOParallel po   -> \ro x0 o -> optoConduitPar ro po x0 o
    SOParChunked po -> \ro x0 o -> optoConduitParChunk ro po x0 o

instance (MonadIO m, Default b) => Default (SimpleOpts m i a b) where
    def = SO
      { soEpochs    = Nothing
      , soDispEpoch = dispEpoch
      , soDispBatch = dispBatch
      , soTestSet   = Nothing
      , soSkipSamps = 1000
      , soEvaluate  = \_ _ -> "<unevaluated>"
      , soSink      = def <$ C.sinkNull
      }

-- | Integrate 'sampleCollect' and 'trainReport' together, automatically
-- generating the sample queue and supplying all callbacks based on the
-- 'SimpleOpts'.
simpleRunner
    :: forall m t a b n. (MonadIO m, PrimMonad m, NFData a, MonoFoldable t)
    => SimpleOpts m (Element t) a b     -- ^ Options
    -> t                                -- ^ Collection of samples
    -> SOOptimizer m n (Element t) a    -- ^ Choice of optimizer concurrency strategy
    -> RunOpts m a                      -- ^ Runner options
    -> a                                -- ^ Initial value
    -> Opto n (Element t) a             -- ^ Optimizer
    -> MWC.Gen (PrimState m)            -- ^ Random generator
    -> m b
simpleRunner SO{..} samps soo ro x0 o g = do
    sampleQueue <- liftIO . atomically $ newTBQueue (fromIntegral soSkipSamps)
    skipAmt     <- liftIO $ case soo of
      SOSingle        -> pure soSkipSamps
      SOParallel po   -> getSkip (poSplit po)
      SOParChunked po -> getSkip (poSplit po)

    let source    = sampleCollect sampleQueue soEpochs soDispEpoch samps g
        optimizer = runSOOptimizer soo ro x0 o source
        reporter  = simpleReport soTestSet soEvaluate

    runConduit $ optimizer
              .| trainReport sampleQueue soDispBatch skipAmt reporter
              .| soSink
  where
    getSkip s = getNumCapabilities <&> \n ->
      max 0 $ (soSkipSamps `div` (n * s)) - 1
