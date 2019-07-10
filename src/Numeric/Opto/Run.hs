{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      : Numeric.Opto.Run
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Functions to /run/ optimiziers.
module Numeric.Opto.Run (
  -- * Options
    RunOpts(..)
  , hoistRunOpts
  , ParallelOpts(..)
  -- * Single-threaded
  , runOpto, evalOpto
  , runOptoNonSampling, evalOptoNonSampling
  -- ** Sampling methods
  , optoConduit, optoConduit_
  , foldOpto, foldOpto_
  -- * Parallel
  , evalOptoParallel
  , evalOptoParallelChunk
  -- ** Sampling Methods
  , optoConduitParallel
  , optoConduitParallelChunk
  ) where

import           Control.Applicative
import           Control.Concurrent.STM.TBMQueue
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import           Data.Conduit.TQueue
import           Data.Default
import           Data.Functor.Contravariant
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
import           GHC.Natural
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update
import           UnliftIO
import           UnliftIO.Concurrent
import qualified Data.Conduit                    as C
import qualified Data.List.NonEmpty              as NE
import qualified Data.Sequences                  as O

-- | Options for running an optimizer.
data RunOpts m a = RO
    { -- | Stop condition; will stop when 'True' (default = never stop)
      roStopCond :: Diff a -> a -> m Bool
      -- | Reporting function (default = no report)
    , roReport   :: a -> m ()
      -- | Number of batches to run (Nothing = run forever) (default = Nothing).
    , roLimit    :: Maybe Int  -- ^ number of batches to run (Nothing = run forever) (default = Nothing)
      -- | Size of batching updates (1 = no batching) (default = 1)
    , roBatch    :: Int
      -- | Frequency that 'roReport' will be called (batches per report)
      -- (Nothing = never report) (default = Just 1)
      --
      -- When run in parallel, this is instead the frequency in
      -- aggregations per report.
    , roFreq     :: Maybe Int  -- ^ batches per report (Nothing = never report) (default = Just 1).
    }

-- | Options for running an optimizer in a concurrent setting.
data ParallelOpts = PO
    { -- | Number of threads (Nothing = max capacity) (default = Nothing)
      poThreads   :: Maybe Int
      -- ^ How many batches thread will process before regrouping (default = 1000)
    , poSplit :: Int
    }

instance Applicative m => Default (RunOpts m a) where
    def = RO
      { roStopCond = \_ _ -> pure False
      , roReport   = \_   -> pure ()
      , roLimit    = Nothing
      , roBatch    = 1
      , roFreq     = Just 1
      }

instance Default ParallelOpts where
    def = PO
      { poThreads = Nothing
      , poSplit   = 1000
      }

instance Contravariant (RunOpts m) where
    contramap f ro = ro
        { roStopCond = \d -> roStopCond ro (f d) . f
        , roReport   = roReport ro . f
        }

hoistRunOpts
    :: (forall x. m x -> n x)
    -> RunOpts m a
    -> RunOpts n a
hoistRunOpts f ro = ro
    { roStopCond = \d -> f . roStopCond ro d
    , roReport   = f . roReport ro
    }

runOpto
    :: Monad m
    => RunOpts m a
    -> m (Maybe r)
    -> a
    -> Opto m v r a
    -> m (a, Opto m v r a)
runOpto ro sampler x0 o = runOpto_ ro sampler x0 o (liftA2 (,))
{-# INLINE runOpto #-}

runOptoNonSampling
    :: Monad m
    => RunOpts m a
    -> a
    -> Opto m v () a
    -> m (a, Opto m v () a)
runOptoNonSampling ro = runOpto ro (pure (Just ()))
{-# INLINE runOptoNonSampling #-}

evalOpto
    :: Monad m
    => RunOpts m a
    -> m (Maybe r)
    -> a
    -> Opto m v r a
    -> m a
evalOpto ro sampler x0 o = runOpto_ ro sampler x0 o const
{-# INLINE evalOpto #-}

evalOptoNonSampling
    :: Monad m
    => RunOpts m a
    -> a
    -> Opto m v () a
    -> m a
evalOptoNonSampling ro = evalOpto ro (pure (Just ()))
{-# INLINE evalOptoNonSampling #-}

runOpto_
    :: forall m v r a q. Monad m
    => RunOpts m a
    -> m (Maybe r)
    -> a
    -> Opto m v r a
    -> (m a -> m (Opto m v r a) -> m q)
    -> m q
runOpto_ RO{..} sampler x0 MkOpto{..} f = do
    rS <- thawRef oInit
    rX <- thawRef @_ @a @v x0
    optoLoop OL
      { olLimit       = roLimit
      , olBatch       = roBatch
      , olReportFreq  = roFreq
      , olInitialize  = thawRef @_ @a @v
      , olUpdate      = (.*+=)
      , olRead        = freezeRef
      , olVar         = rX
      , olSample      = sampler
      , olUpdateState = oUpdate rS
      , olStopCond    = roStopCond
      , olReportAct   = roReport
      }
    f (freezeRef rX) (flip MkOpto oUpdate <$> freezeRef rS)
{-# INLINE runOpto_ #-}

data OptoLoop m v r a c = OL
    { olLimit       :: Maybe Int
    , olBatch       :: Int
    , olReportFreq  :: Maybe Int
    , olInitialize  :: a -> m v
    , olUpdate      :: v -> (c, a) -> m ()
    , olRead        :: v -> m a
    , olVar         :: v
    , olSample      :: m (Maybe r)
    , olUpdateState :: r -> a -> m (c, a)
    , olStopCond    :: Diff a -> a -> m Bool
    , olReportAct   :: a -> m ()
    }

optoLoop
    :: forall m v r a c. (Monad m, Scaling c a)
    => OptoLoop m v r a c
    -> m ()
optoLoop OL{..} = go 0
  where
    go !i = when (limCheck i) $ do
      !x <- olRead olVar
      (exhausted, cg) <- batcher x
      forM_ cg $ \(c, g) -> do
        olUpdate olVar (c, g)
        x' <- olRead olVar
        when (reportCheck i) $
          olReportAct x'
        stopper <- olStopCond (c .* g) x'
        when (not exhausted && not stopper) $
          go (i + 1)
    limCheck = case olLimit of
      Nothing -> const True
      Just l  -> (< l)
    reportCheck = case olReportFreq of
      Nothing -> const False
      Just r  -> \i -> (i + 1) `mod` r == 0
    batcher
      | olBatch <= 1 = fmap (\y -> (isNothing y, y)) . runMaybeT . batchSingle
      | otherwise    = batchLoop
    batchSingle !x = lift . (`olUpdateState` x) =<< MaybeT olSample
    batchLoop !x = do
      v <- olInitialize addZero
      k <- fmap isNothing . runMaybeT . replicateM olBatch $
          lift . olUpdate v =<< batchSingle x
      (k,) . Just . (scaleOne @c @a,) <$> olRead v
{-# INLINE optoLoop #-}

optoConduit
    :: Monad m
    => RunOpts m a
    -> a
    -> Opto (ConduitT r a m) v r a
    -> ConduitT r a m (Opto (ConduitT r a m) v r a)
optoConduit ro x0 o = runOpto_ ro' C.await x0 o (const id)
  where
    ro' = (hoistRunOpts lift ro)
        { roReport = \x -> C.yield x *> lift (roReport ro x) }
{-# INLINE optoConduit #-}

optoConduit_
    :: Monad m
    => RunOpts m a
    -> a
    -> Opto (ConduitT r a m) v r a
    -> ConduitT r a m ()
optoConduit_ ro x0 = void . optoConduit ro x0
{-# INLINE optoConduit_ #-}

-- | 'runOptoSample' specialized for 'FoldSampleT': give it a collection of
-- items @rs@, and it will process each item @r@.  Returns the optimized
-- @a@, the leftover @rs@, and a closure 'Opto' that can be resumed.

foldOpto
    :: (Monad m, O.IsSequence rs, r ~ Element rs)
    => RunOpts m a
    -> a
    -> Opto (StateT rs m) v r a
    -> rs
    -> m (a, rs, Opto (StateT rs m) v r a)
foldOpto ro x0 o = fmap shuffle
                 . runStateT (runOpto (hoistRunOpts lift ro) sampleState x0 o)
  where
    shuffle ((x', o'), rs) = (x', rs, o')

foldOpto_
    :: (Monad m, O.IsSequence rs, r ~ Element rs)
    => RunOpts m a
    -> a
    -> Opto (StateT rs m) v r a
    -> rs
    -> m (a, rs)
foldOpto_ ro x0 o = runStateT (evalOpto (hoistRunOpts lift ro) sampleState x0 o)

sampleState
    :: (Monad m, O.IsSequence rs)
    => StateT rs m (Maybe (Element rs))
sampleState = state $ \xs -> case O.uncons xs of
  Nothing      -> (Nothing, mempty)
  Just (y, ys) -> (Just y , ys    )

evalOptoParallel
    :: forall m v r a. (MonadUnliftIO m, Fractional a)
    => RunOpts m a
    -> ParallelOpts
    -> m (Maybe r)
    -> a
    -> Opto m v r a
    -> m a
evalOptoParallel ro@RO{..} PO{..} sampler x0 o = do
    n       <- maybe getNumCapabilities pure poThreads
    hitStop <- newIORef Nothing
    gas     <- mapM newMVar (fromIntegral <$> roLimit)
    let reportCheck = case roFreq of
          Nothing -> const False
          Just r  -> \i -> (i + 1) `mod` (r `div` (n * poSplit)) == 0
        loop !x !i = do
          xs <- fmap catMaybes . replicateConcurrently n $ do
            lim   <- maybe (pure poSplit) getGas gas
            if lim > 0
              then Just <$> do
                let ro' = ro
                      { roLimit    = Just lim
                      , roReport   = \_ -> pure ()
                      , roStopCond = \d x' -> do
                          sc <- roStopCond d x'
                          sc <$ when sc (writeIORef hitStop (Just x'))
                      , roFreq     = Nothing
                      }
                evalOpto ro' sampler x o
              else pure Nothing
          readIORef hitStop >>= \case
            Nothing    -> case NE.nonEmpty xs of
              Just xs' -> do
                let !x' = mean xs'
                when (reportCheck i) $
                  roReport x'
                loop x' (i + 1)
              Nothing  -> pure x
            Just found -> pure found
    loop x0 0
  where
    getGas :: MVar Natural -> m Int
    getGas = flip modifyMVar $ \n -> case n `minusNaturalMaybe` fromIntegral poSplit of
      Nothing -> pure (0, fromIntegral n)
      Just g  -> pure (g, poSplit       )

evalOptoParallelChunk
    :: forall m v r a rs. (MonadUnliftIO m, Fractional a, O.IsSequence rs, r ~ Element rs)
    => RunOpts m a
    -> ParallelOpts
    -> (Int -> m rs)
    -> a
    -> Opto (StateT rs m) v r a
    -> m a
evalOptoParallelChunk ro@RO{..} PO{..} sampler x0 o = do
    n       <- maybe getNumCapabilities pure poThreads
    hitStop <- newIORef Nothing
    gas     <- mapM newMVar (fromIntegral <$> roLimit)
    let reportCheck = case roFreq of
          Nothing -> const False
          Just r  -> \i -> (i + 1) `mod` (r `div` (n * poSplit)) == 0
        loop !x !i = do
          xs <- fmap catMaybes . replicateConcurrently n $ do
            lim   <- maybe (pure poSplit) getGas gas
            items <- sampler lim
            if onull items
              then Just . fst <$> do
                let ro' = ro
                      { roLimit    = Nothing
                      , roReport   = \_ -> pure ()
                      , roStopCond = \d x' -> do
                          sc <- roStopCond d x'
                          sc <$ when sc (writeIORef hitStop (Just x'))
                      , roFreq     = Nothing
                      }
                foldOpto_ ro' x o items
              else pure Nothing
          readIORef hitStop >>= \case
            Nothing    -> case NE.nonEmpty xs of
              Just xs' -> do
                let !x' = mean xs'
                when (reportCheck i) $
                  roReport x'
                loop x' (i + 1)
              Nothing  -> pure x
            Just found -> pure found
    loop x0 0
  where
    getGas :: MVar Natural -> m Int
    getGas = flip modifyMVar $ \n -> case n `minusNaturalMaybe` fromIntegral poSplit of
      Nothing -> pure (0, fromIntegral n)
      Just g  -> pure (g, poSplit       )

optoConduitParallel
    :: forall m v r a. (MonadUnliftIO m, Fractional a)
    => RunOpts m a
    -> ParallelOpts
    -> a
    -> Opto m v r a
    -> ConduitT () r m ()
    -> ConduitT () a m ()
optoConduitParallel ro po x0 o src = do
    n <- lift . maybe getNumCapabilities pure . poThreads $ po
    let buff = fromIntegral $ n * poSplit po
    inQueue  <- atomically $ newTBMQueue buff
    outVar   <- newEmptyMVar
    sem      <- atomically $ newEmptyTMVar
    let ro' = ro
          { roReport = \x -> do
              putMVar outVar (False, x)
              roReport ro x
          }
    void . lift . forkIO $ runConduit (src .| sinkTBMQueue inQueue)
    void . lift . forkIO $ do
        x <- evalOptoParallel ro' po (atomically (readTMVar sem *> readTBMQueue inQueue)) x0 o
        putMVar outVar (True, x)

    let loop = do
          atomically $ putTMVar sem ()
          (done, r) <- takeMVar outVar
          atomically $ takeTMVar sem      -- wait until yield before continuing
          C.yield r
          unless done loop

    loop

optoConduitParallelChunk
    :: forall m v r a. (MonadUnliftIO m, Fractional a)
    => RunOpts m a
    -> ParallelOpts
    -> a
    -> Opto (StateT [r] m) v r a
    -> ConduitT () r m ()
    -> ConduitT () a m ()
optoConduitParallelChunk ro po x0 o src = do
    n <- lift . maybe getNumCapabilities pure . poThreads $ po
    let buff = fromIntegral $ n * poSplit po
    inQueue  <- atomically $ newTBMQueue buff
    outVar   <- newEmptyMVar
    sem      <- atomically $ newEmptyTMVar
    let ro' = ro
          { roReport = \x -> do
              putMVar outVar (False, x)
              roReport ro x
          }
        readChunk i = catMaybes <$> replicateM i (readTMVar sem *> readTBMQueue inQueue)
    void . lift . forkIO $ runConduit (src .| sinkTBMQueue inQueue)
    void . lift . forkIO $ do
        x <- evalOptoParallelChunk ro' po (atomically . readChunk) x0 o
        putMVar outVar (True, x)

    let loop = do
          atomically $ putTMVar sem ()
          (done, r) <- takeMVar outVar
          atomically $ takeTMVar sem      -- wait until yield before continuing
          C.yield r
          unless done loop
    loop

mean :: (Foldable1 t, Fractional a) => t a -> a
mean = go . foldMap1 (`Sum2` 1)
  where
    go (Sum2 x n) = x / fromInteger n
    {-# INLINE go #-}
{-# INLINE mean #-}

data Sum2 a b = Sum2 !a !b

instance (Num a, Num b) => Semigroup (Sum2 a b) where
    Sum2 x1 y1 <> Sum2 x2 y2 = Sum2 (x1 + x2) (y1 + y2)
    {-# INLINE (<>) #-}

instance (Num a, Num b) => Monoid (Sum2 a b) where
    mappend = (<>)
    {-# INLINE mappend #-}
    mempty = Sum2 0 0
    {-# INLINE mempty #-}
