{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}

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
  , hoistParallelOpts
  -- * Single-threaded
  , opto
  , opto'
  , optoNonSampling'
  , optoNonSampling
  -- ** Sampling methods
  , optoConduit, optoConduit'
  , optoFold, optoFold'
  -- * Parallel
  , optoPar
  , optoParChunk
  , optoParNonSampling
  -- ** Sampling Methods
  , optoConduitPar
  , optoConduitParChunk
  -- * Util
  , mean
  ) where

import           Control.Applicative
import           Control.Concurrent.STM.TBMQueue
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import           Data.Conduit
import           Data.Conduit.TQueue
import           Data.Default
import           Data.Functor
import           Data.Functor.Contravariant
import           Data.Functor.Invariant
import           Data.List
import           Data.List.NonEmpty              (NonEmpty(..))
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

-- | Options for running an optimizer.
data RunOpts m a = RO
    { -- | Stop condition; will stop when 'True' (default = never stop)
      roStopCond :: Diff a -> a -> m Bool
      -- | Reporting function (default = no report)
    , roReport   :: a -> m ()
      -- | Number of batches to run (Nothing = run forever) (default = Nothing).
    , roLimit    :: Maybe Int
      -- | Size of batching updates (1 = no batching) (default = 1)
    , roBatch    :: Int
      -- | Frequency that 'roReport' will be called (batches per report)
      -- (Nothing = never report) (default = Just 1)
    , roFreq     :: Maybe Int  -- ^ batches per report (Nothing = never report) (default = Just 1).
    }

-- | Options for running an optimizer in a concurrent setting.
data ParallelOpts m a = PO
    { -- | Number of threads (Nothing = max capacity) (default = Nothing)
      poThreads :: Maybe Int
      -- | How many batches thread will process before regrouping (default = 1000)
    , poSplit   :: Int
      -- | How to recombine a pool of updated results into a single result
      -- (default = @'pure' '.' 'mean'@)
    , poCombine :: NonEmpty a -> m a
      -- | For conduit runners, whether or not conduit is in "pull-based"
      -- mode, where optimization doesn't happen until requested
      -- downstream.  This is ignored if not running via conduit (default
      -- = True)
    , poPull    :: Bool
    }

instance Applicative m => Default (RunOpts m a) where
    def = RO
      { roStopCond = \_ _ -> pure False
      , roReport   = \_   -> pure ()
      , roLimit    = Nothing
      , roBatch    = 1
      , roFreq     = Just 1
      }

instance (Applicative m, Fractional a) => Default (ParallelOpts m a) where
    def = PO
      { poThreads = Nothing
      , poSplit   = 1000
      , poCombine = pure . mean
      , poPull    = True
      }

instance Contravariant (RunOpts m) where
    contramap f ro = ro
      { roStopCond = \d -> roStopCond ro (f d) . f
      , roReport   = roReport ro . f
      }

instance Invariant (RunOpts m) where
    invmap _ g = contramap g

instance Functor m => Invariant (ParallelOpts m) where
    invmap f g po = po
      { poCombine = fmap f . poCombine po . fmap g
      }

-- | Map over the underlying monad of a 'RunOpts'.
hoistRunOpts
    :: (forall x. m x -> n x)
    -> RunOpts m a
    -> RunOpts n a
hoistRunOpts f ro = ro
    { roStopCond = \d -> f . roStopCond ro d
    , roReport   = f . roReport ro
    }

-- | Map over the underlying monad of a 'ParallelOpts'.
hoistParallelOpts
    :: (forall x. m x -> n x)
    -> ParallelOpts m a
    -> ParallelOpts n a
hoistParallelOpts f po = po
    { poCombine = f . poCombine po
    }

-- | Run an optimizer on some input, given a monadic action to produce each
-- new sample.  When the action produces 'Nothing', the running immediately
-- terminates even if the stop condition has not yet been met.
opto
    :: Monad m
    => RunOpts m a      -- ^ Runner options
    -> m (Maybe r)      -- ^ Produce new sample.
    -> a                -- ^ Value to optimize
    -> Opto m v r a     -- ^ Optimizer
    -> m a
opto ro sampler x0 o = opto_ ro sampler x0 o const
{-# INLINE opto #-}

-- | Run a non-sampling optimizer on some input until the stop condition is
-- met.
optoNonSampling
    :: Monad m
    => RunOpts m a      -- ^ Runner options
    -> a                -- ^ Value to optimize
    -> Opto m v () a    -- ^ Non-sampling optimizer
    -> m a
optoNonSampling ro = opto ro (pure (Just ()))
{-# INLINE optoNonSampling #-}

-- | A version of 'opto' that also returns an updated optimizer state that
-- can be resumed.
opto'
    :: Monad m
    => RunOpts m a      -- ^ Runner options
    -> m (Maybe r)      -- ^ Produce new sample.
    -> a                -- ^ Value to optimize
    -> Opto m v r a     -- ^ Optimizer
    -> m (a, Opto m v r a)
opto' ro sampler x0 o = opto_ ro sampler x0 o (liftA2 (,))
{-# INLINE opto' #-}

-- | A version of 'optoNonSampling' that also returns an updated optimizer state that
-- can be resumed.
optoNonSampling'
    :: Monad m
    => RunOpts m a      -- ^ Runner options
    -> a                -- ^ Value to optimize
    -> Opto m v () a    -- ^ Non-sampling optimizer
    -> m (a, Opto m v () a)
optoNonSampling' ro = opto' ro (pure (Just ()))
{-# INLINE optoNonSampling' #-}

opto_
    :: forall m v r a q. Monad m
    => RunOpts m a
    -> m (Maybe r)
    -> a
    -> Opto m v r a
    -> (m a -> m (Opto m v r a) -> m q)
    -> m q
opto_ RO{..} sampler x0 MkOpto{..} f = do
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
{-# INLINE opto_ #-}

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

-- | Given an optimizer and some initial value, produce a 'ConduitT' that
-- takes in samples and outputs each successively optimized versions of the
-- value.  This essentially is a convenient wrapper over 'opto'.
--
-- To get the /final/ optimized result after a stream has terminated,
-- compose this with a sink like 'C.last'.
--
-- @
-- 'optoConduit' ro x0 o .| 'C.last'
--   :: ConduitT r o m (Maybe a)
--
-- 'optoConduit' ro x0 o .| 'C.lastDef' x0
--   :: ConduitT r o m a
-- @
optoConduit
    :: Monad m
    => RunOpts m a                  -- ^ Runner options
    -> a                            -- ^ Value to optimize
    -> Opto (ConduitT r a m) v r a  -- ^ Optimizer
    -> ConduitT r a m ()
optoConduit ro x0 = void . optoConduit' ro x0
{-# INLINE optoConduit #-}

-- | A version of 'optoConduit' that also returns an updated optimizer state that
-- can be resumed.
optoConduit'
    :: Monad m
    => RunOpts m a                  -- ^ Runner options
    -> a                            -- ^ Value to optimize
    -> Opto (ConduitT r a m) v r a  -- ^ Optimizer
    -> ConduitT r a m (Opto (ConduitT r a m) v r a)
optoConduit' ro x0 o = opto_ ro' C.await x0 o (const id)
  where
    ro' = (hoistRunOpts lift ro)
        { roStopCond = \d x -> C.yield x *> lift (roStopCond ro d x) }
{-# INLINE optoConduit' #-}

-- | Convenient wrapper over 'opto' to allow consumption over a list of
-- samples.
optoFold
    :: Monad m
    => RunOpts m a                  -- ^ Runner options
    -> a                            -- ^ Value to optimize
    -> Opto (StateT [r] m) v r a    -- ^ Optimizer
    -> [r]                          -- ^ List of samples to optimize over
    -> m (a, [r])
optoFold ro x0 o = runStateT (opto (hoistRunOpts lift ro) sampleState x0 o)
{-# INLINE optoFold #-}

-- | A version of 'optoFold'' that also returns an updated optimizer state that
-- can be resumed.
optoFold'
    :: Monad m
    => RunOpts m a                  -- ^ Runner options
    -> a                            -- ^ Value to optimize
    -> Opto (StateT [r] m) v r a    -- ^ Optimizer
    -> [r]                          -- ^ List of samples to optimize over
    -> m (a, [r], Opto (StateT [r] m) v r a)
optoFold' ro x0 o = fmap shuffle
                  . runStateT (opto' (hoistRunOpts lift ro) sampleState x0 o)
  where
    shuffle ((x', o'), rs) = (x', rs, o')
    {-# INLINE shuffle #-}
{-# INLINE optoFold' #-}

sampleState :: Monad m => StateT [r] m (Maybe r)
sampleState = state $ maybe (Nothing, []) (first Just) . uncons
{-# INLINE sampleState #-}

-- | Run an optimizer in parallel on multiple threads on some value, given
-- a (thread-safe) monadic action to produce each new sample.
--
-- It does this by repeatedly:
--
-- 1.   Splitting into multiple threads (based on 'poThreads')
-- 2.   Running 'opto' (single-threaded optimiztion) on each thread,
--      independently, from the same initial value.
-- 3.   After 'poSplit' items have been processed, all threads wait on each
--      other to stop.  After each thread is done, each thread's optimized
--      value is then aggregated using 'poCombine' (by default, it takes
--      the mean).
-- 4.   This new optimized combined value is then used to begin the cycle
--      again.
--
-- When action produces 'Nothing' for /all/ threads, the running
-- immediately terminates on all threads and returns even if the stop
-- condition has not yet been met.  If the stop condition is met, the value
-- given to the stop condition will be used as the final result, ignoring
-- all other thread pools.
optoPar
    :: forall m v r a. MonadUnliftIO m
    => RunOpts m a          -- ^ Runner options
    -> ParallelOpts m a     -- ^ Parallelization options
    -> m (Maybe r)          -- ^ Produce new sample (should be thread-safe)
    -> a                    -- ^ Value to optimize
    -> Opto m v r a         -- ^ Optimizer
    -> m a
optoPar ro po sampler x0 o = optoPar_ ro po x0 $ \hitStop lim x -> do
    if lim > 0
      then Just <$> do
        let ro' = ro
              { roLimit    = Just lim
              , roReport   = \_ -> pure ()
              , roStopCond = \d x' -> do
                  sc <- roStopCond ro d x'
                  sc <$ when sc (writeIORef hitStop (Just x'))
              , roFreq     = Nothing
              }
        opto ro' sampler x o
      else pure Nothing
{-# INLINE optoPar #-}

-- | Run a non-sampling optimizer in parallel on multiple threads on some
-- value until the stop condition is met.
--
-- See 'optoPar' for a detailed description of how parallel optimization is
-- implemented.
optoParNonSampling
    :: MonadUnliftIO m
    => RunOpts m a          -- ^ Runner options
    -> ParallelOpts m a     -- ^ Parallelization options
    -> a                    -- ^ Value to optimize
    -> Opto m v () a        -- ^ Non-sampling optimizer
    -> m a
optoParNonSampling ro po = optoPar ro po (pure (Just ()))
{-# INLINE optoParNonSampling #-}

-- | A version of 'optoPar' that performs a batch fetch for each thread's
-- entire sample pool /before/ beginning parallel optimization.  This can
-- be useful if the sampling is faster in batch amounts.
optoParChunk
    :: forall m v r a. MonadUnliftIO m
    => RunOpts m a                  -- ^ Runner options
    -> ParallelOpts m a             -- ^ Parallelization options
    -> (Int -> m [r])               -- ^ Batched fetch of samples. Input
                                    --   is how many samples the action
                                    --   expects to receive, although it is
                                    --   okay if a lower amount is given
                                    --   due to an exhausted sample pool.
    -> a                            -- ^ Value to optimize
    -> Opto (StateT [r] m) v r a    -- ^ Optimizer
    -> m a
optoParChunk ro po sampler x0 o = optoPar_ ro po x0 $ \hitStop lim x -> do
    items <- sampler lim
    if onull items
      then pure Nothing
      else Just . fst <$> do
        let ro' = ro
              { roLimit    = Nothing
              , roReport   = \_ -> pure ()
              , roStopCond = \d x' -> do
                  sc <- roStopCond ro d x'
                  sc <$ when sc (writeIORef hitStop (Just x'))
              , roFreq     = Nothing
              }
        optoFold ro' x o items
{-# INLINE optoParChunk #-}

optoPar_
    :: forall m a. MonadUnliftIO m
    => RunOpts m a
    -> ParallelOpts m a
    -> a
    -> (IORef (Maybe a) -> Int -> a -> m (Maybe a))
    -> m a
optoPar_ RO{..} PO{..} x0 runner = do
    n       <- maybe getNumCapabilities pure poThreads
    hitStop <- newIORef Nothing
    gas     <- mapM newMVar (fromIntegral <$> roLimit)
    optoParLoop OPL
      { oplThreads = n
      , oplFreq    = roFreq
      , oplSplit   = poSplit
      , oplHitStop = hitStop
      , oplGas     = gas
      , oplReport  = roReport
      , oplRunner  = runner hitStop
      , oplCombine = poCombine
      , oplInitial = x0
      }
{-# INLINE optoPar_ #-}

data OptoParLoop m a = OPL
    { oplThreads :: Int
    , oplFreq    :: Maybe Int
    , oplSplit   :: Int
    , oplHitStop :: IORef (Maybe a)
    , oplGas     :: Maybe (MVar Natural)
    , oplReport  :: a -> m ()
    , oplRunner  :: Int -> a -> m (Maybe a)
    , oplCombine :: NonEmpty a -> m a
    , oplInitial :: a
    }

optoParLoop
    :: MonadUnliftIO m
    => OptoParLoop m a
    -> m a
optoParLoop OPL{..} = go 0 oplInitial
  where
    go !i !x = do
      xs <- fmap catMaybes . replicateConcurrently oplThreads $
        flip oplRunner x =<< maybe (pure oplSplit) getGas oplGas
      readIORef oplHitStop >>= \case
        Nothing    -> case NE.nonEmpty xs of
          Just xs' -> do
            !x' <- oplCombine xs'
            when (reportCheck i) $
              oplReport x'
            go (i + 1) x'
          Nothing  -> pure x
        Just found -> pure found
    reSplit = oplFreq <&> \r -> max 1 (r `div` (oplThreads * oplSplit))
    reportCheck = case reSplit of
      Nothing -> const False
      Just r  -> \i -> (i + 1) `mod` r == 0
    getGas = flip modifyMVar $ \n -> case n `minusNaturalMaybe` fromIntegral oplSplit of
      Nothing -> pure (0, fromIntegral n)
      Just g  -> pure (g, oplSplit      )
{-# INLINE optoParLoop #-}

-- | Given an optimizer, some initial value, and a conduit /source/,
-- returns a conduit sorce that outputs succesively optimized versions of
-- the value after each thread recombination, where each version is
-- optimized using parallel multi-threaded optimization.
--
-- See 'optoPar' for a detailed description on how parallel
-- optimization is implemented.
--
-- Note that, unlike 'optoConduit', which is a conduit, this is a conduit
-- (source) /transformer/.  It takes a source outputting /samples/ and
-- returns a /new/ source of /optimized values/.
--
-- A value is emitted after every thread recombination/call of 'poCombine'.
optoConduitPar
    :: forall m v r a. MonadUnliftIO m
    => RunOpts m a
    -> ParallelOpts m a
    -> a
    -> Opto m v r a
    -> ConduitT () r m ()
    -> ConduitT () a m ()
optoConduitPar ro po x0 o = optoConduitPar_ ro po $ \sem inQueue outVar -> do
    let ro' = ro
          { roReport = \x -> do
              putMVar outVar (False, x)
              roReport ro x
          }
        readQueue = do
          sem
          atomically $ readTBMQueue inQueue
    optoPar ro' po readQueue x0 o
{-# INLINE optoConduitPar #-}

optoConduitParChunk
    :: forall m v r a. MonadUnliftIO m
    => RunOpts m a
    -> ParallelOpts m a
    -> a
    -> Opto (StateT [r] m) v r a
    -> ConduitT () r m ()
    -> ConduitT () a m ()
optoConduitParChunk ro po x0 o = optoConduitPar_ ro po $ \sem inQueue outVar -> do
    let ro' = ro
          { roReport = \x -> do
              putMVar outVar (False, x)
              roReport ro x
          }
        readChunk i = fmap catMaybes . replicateM i $ do
          sem
          atomically $ readTBMQueue inQueue
    optoParChunk ro' po readChunk x0 o
{-# INLINE optoConduitParChunk #-}

optoConduitPar_
    :: forall m r a. MonadUnliftIO m
    => RunOpts m a
    -> ParallelOpts m a
    -> (m () -> TBMQueue r -> MVar (Bool, a) -> m a)
    -> ConduitT () r m ()
    -> ConduitT () a m ()
optoConduitPar_ ro po runner src = do
    n <- lift . maybe getNumCapabilities pure . poThreads $ po
    let buff0 = n * poSplit po
        buff  = fromIntegral . maybe buff0 (min buff0) $ roLimit ro
    inQueue  <- atomically $ newTBMQueue buff
    outVar   <- newEmptyMVar
    sem      <- forM (guard @Maybe (poPull po)) $ \_ -> newEmptyMVar @_ @()
    lift $ do
      void . forkIO $ runConduit (src .| sinkTBMQueue inQueue)
      void . forkIO $ do
        x <- runner (mapM_ readMVar sem) inQueue outVar
        putMVar outVar (True, x)

    let loop = do
          mapM_ (`putMVar` ()) sem
          (done, r) <- takeMVar outVar
          mapM_ takeMVar sem      -- wait until yield before continuing
          C.yield r
          unless done loop
    loop
{-# INLINE optoConduitPar_ #-}


-- | The mean of the values in a non-empty container.
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
