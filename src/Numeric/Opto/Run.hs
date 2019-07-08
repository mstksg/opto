{-# LANGUAGE BangPatterns        #-}
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
  , ParallelOpts(..)
  -- -- * Sampling
  -- , runOptoSample
  -- , evalOptoSample
  -- -- ** Specific Sampling Monads
  -- , optoConduit, optoConduit_
  -- , refOpto, refOpto_
  -- , foldOpto, foldOpto_
  -- -- * Non-Sampling
  -- , runOpto
  -- , evalOpto
  -- , evalOpto, runOpto
  -- , evalOptoAlt, runOptoAlt
  -- -- * Parallel
  -- , evalOptoParallel
  -- , runOptoParallel
  ) where

-- import           Control.Monad.IO.Unlift
-- import           Data.Functor
-- import           UnliftIO.Async
-- import           UnliftIO.Concurrent
-- import           UnliftIO.IORef
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Sample
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor
import           Data.Conduit
import           Data.Default
import           Data.Maybe
import           Data.MonoTraversable
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update
import qualified Data.Conduit                     as C
import qualified Data.Sequences                   as O

-- | Options for running an optimizer.
data RunOpts m a = RO
    { roStopCond :: Diff a -> a -> m Bool
    , roReport   :: a -> m ()  -- ^ reporting function
    , roLimit    :: Maybe Int  -- ^ number of batches to run (Nothing = run forever)
    , roBatch    :: Int        -- ^ batching updates
    , roFreq     :: Maybe Int  -- ^ batches per report (Nothing = never report)
    }

-- | Options for running an optimizer in a concurrent setting.
data ParallelOpts = PO
    { poThreads   :: Maybe Int   -- ^ Number of threads. Will default to max capacity
    , poSplitRuns :: Int         -- ^ How much each thread will process before regrouping
    }

instance Applicative m => Default (RunOpts m a) where
    def = RO
      { roStopCond = \_ _ -> pure False
      , roReport   = \_   -> pure ()
      , roLimit    = Nothing
      , roBatch    = 1
      , roFreq     = Just 1
      }

--runOptoSample
--    :: MonadSample r m
--    => RunOpts m a
--    -> a
--    -> OptoM m v a
--    -> m (a, OptoM m v a)
--runOptoSample ro x o = runOptoSample_ ro x o (liftA2 (,))
--{-# INLINE runOptoSample #-}

--evalOptoSample
--    :: MonadSample r m
--    => RunOpts m a
--    -> a
--    -> OptoM m v a
--    -> m a
--evalOptoSample ro x o = runOptoSample_ ro x o const
--{-# INLINE evalOptoSample #-}

--runOptoSample_
--    :: forall m v a r q. MonadSample r m
--    => RunOpts m a
--    -> a
--    -> OptoM m v a
--    -> (m a -> m (OptoM m v a) -> m q)
--    -> m q
--runOptoSample_ RO{..} x0 MkOptoM{..} f = do
--    rSs <- thawRefs oInit
--    rX  <- thawRef @_ @a @v x0
--    optoLoop roLimit roBatch roFreq
--        (thawRef @_ @a @v)
--        (.*+=)
--        freezeRef
--        rX
--        (oUpdate rSs)
--        roStopCond
--        roReport
--    f (freezeRef rX) (flip MkOptoM oUpdate <$> pullRefs rSs)
--{-# INLINE runOptoSample_ #-}

runOpto
    :: Monad m
    => RunOpts m a
    -> m (Maybe r)
    -> a
    -> OptoM m v r a
    -> m (a, OptoM m v r a)
runOpto ro sampler x0 o = runOpto_ ro sampler x0 o (liftA2 (,))
{-# INLINE runOpto #-}

runOptoNonSampling
    :: Monad m
    => RunOpts m a
    -> a
    -> OptoM m v () a
    -> m (a, OptoM m v () a)
runOptoNonSampling ro = runOpto ro (pure (Just ()))
{-# INLINE runOptoNonSampling #-}

evalOpto
    :: Monad m
    => RunOpts m a
    -> m (Maybe r)
    -> a
    -> OptoM m v r a
    -> m a
evalOpto ro sampler x0 o = runOpto_ ro sampler x0 o const
{-# INLINE evalOpto #-}

evalOptoNonSampling
    :: Monad m
    => RunOpts m a
    -> a
    -> OptoM m v () a
    -> m a
evalOptoNonSampling ro = evalOpto ro (pure (Just ()))
{-# INLINE evalOptoNonSampling #-}

runOpto_
    :: forall m v a r q. Monad m
    => RunOpts m a
    -> m (Maybe r)
    -> a
    -> OptoM m v r a
    -> (m a -> m (OptoM m v r a) -> m q)
    -> m q
runOpto_ RO{..} sampler x0 MkOptoM{..} f = do
    rSs <- thawRefs oInit
    rX  <- thawRef @_ @a @v x0
    optoLoop OL
      { olLimit       = roLimit
      , olBatch       = roBatch
      , olReportFreq  = roFreq
      , olInitialize  = thawRef @_ @a @v
      , olUpdate      = (.*+=)
      , olRead        = freezeRef
      , olVar         = rX
      , olSample      = sampler
      , olUpdateState = oUpdate rSs
      , olStopCond    = roStopCond
      , olReportAct   = roReport
      }
    f (freezeRef rX) (flip MkOptoM oUpdate <$> pullRefs rSs)
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
        when (reportCheck i) $
          olReportAct =<< olRead olVar
        stopper <- olStopCond (c .* g) x
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
    => RunOpts (ConduitT r a m) a
    -> a
    -> OptoM (ConduitT r a m) v r a
    -> ConduitT r a m (OptoM (ConduitT r a m) v r a)
optoConduit ro x0 o = runOpto_ ro' C.await x0 o (const id)
  where
    ro' = ro { roReport = \x -> C.yield x *> roReport ro x
             }
{-# INLINE optoConduit #-}

optoConduit_
    :: Monad m
    => RunOpts (ConduitT r a m) a
    -> a
    -> OptoM (ConduitT r a m) v r a
    -> ConduitT r a m ()
optoConduit_ ro x0 = void . optoConduit ro x0
{-# INLINE optoConduit_ #-}

---- | 'runOptoSample' specialized for 'FoldSampleT': give it a collection of
---- items @rs@, and it will process each item @r@.  Returns the optimized
---- @a@, the leftover @rs@, and a closure 'OptoM' that can be resumed.
--foldOpto
--    :: (Monad m, O.IsSequence rs, O.Index rs ~ Int)
--    => RunOpts (FoldSampleT rs m) a
--    -> a
--    -> OptoM (FoldSampleT rs m) v a
--    -> rs
--    -> m (a, rs, OptoM (FoldSampleT rs m) v a)
--foldOpto ro x0 o = fmap shuffle
--                 . runFoldSampleT (runOptoSample ro x0 o)
--  where
--    shuffle (Nothing, rs)       = (x0, rs, o )
--    shuffle (Just (x', o'), rs) = (x', rs, o')
--    {-# INLINE shuffle #-}
--{-# INLINE foldOpto #-}

---- | 'evalOptoSample' specialized for 'FoldSampleT'.  Basically 'foldOpto',
---- without returning the resumable closure.
--foldOpto_
--    :: (Monad m, O.IsSequence rs, O.Index rs ~ Int)
--    => RunOpts (FoldSampleT rs m) a
--    -> a
--    -> OptoM (FoldSampleT rs m) v a
--    -> rs
--    -> m (a, rs)
--foldOpto_ ro x0 o = (fmap . first) (fromMaybe x0)
--                  . runFoldSampleT (evalOptoSample ro x0 o)
--{-# INLINE foldOpto_ #-}

---- | 'runOptoSample' specialized for 'RefSample': give it a mutable
---- reference @vrs@ to a collection of items @rs@, and it will process each
---- item @r@ while emptying out the mutable reference.
----
---- It will terminate and return a result as soon as the reference is empty.
---- You can add more items to the mutable reference as it is being
---- processed, and it will process it as long as it hasn't yet completed.
----
---- Returns the optimized @a@, and a closure 'OptoM' that can be resumed.
--refOpto
--    :: (Monad m, Ref m rs vrs, Element rs ~ r, O.IsSequence rs, O.Index rs ~ Int)
--    => RunOpts (RefSample vrs r m) a
--    -> a
--    -> OptoM (RefSample vrs r m) va a
--    -> vrs
--    -> m (a, OptoM (RefSample vrs r m) va a)
--refOpto ro x0 o = fmap (fromMaybe (x0, o))
--                . runRefSample (runOptoSample ro x0 o)
--{-# INLINE refOpto #-}

--refOpto_
--    :: (Monad m, Ref m rs vrs, Element rs ~ r, O.IsSequence rs, O.Index rs ~ Int)
--    => RunOpts (RefSample vrs r m) a
--    -> a
--    -> OptoM (RefSample vrs r m) va a
--    -> vrs
--    -> m a
--refOpto_ ro x0 o = fmap (fromMaybe x0)
--                 . runRefSample (evalOptoSample ro x0 o)
--{-# INLINE refOpto_ #-}




-- mean :: (Foldable t, Fractional a) => t a -> a
-- mean = go . foldMap (`Sum2` 1)
--   where
--     go (Sum2 x n) = x / fromInteger n
--     {-# INLINE go #-}
-- {-# INLINE mean #-}

-- data Sum2 a b = Sum2 !a !b

-- instance (Num a, Num b) => Semigroup (Sum2 a b) where
--     Sum2 x1 y1 <> Sum2 x2 y2 = Sum2 (x1 + x2) (y1 + y2)
--     {-# INLINE (<>) #-}

-- instance (Num a, Num b) => Monoid (Sum2 a b) where
--     mappend = (<>)
--     {-# INLINE mappend #-}
--     mempty = Sum2 0 0
--     {-# INLINE mempty #-}

-- -- | Like 'evalOptoParallel', but returns closures (one for each thread)
-- -- from which one can "resume" an optimizer from where it leaves off with
-- -- its state in each thread.
-- runOptoParallel
--     :: (MonadUnliftIO m, Fractional a)
--     => RunOpts m a
--     -> ParallelOpts
--     -> a
--     -> OptoM m v a
--     -> m (a, [OptoM m v a])
-- runOptoParallel ro PO{..} x0 o0 = do
--     n <- maybe getNumCapabilities pure poThreads
--     hitStop <- newIORef Nothing
--     let os0 = replicate n o0
--     fmap (maybe (x0, os0) fst)
--         . runMaybeT
--         . flip execStateT ((x0, os0), 0)
--         . many
--         . StateT $ \((x,os), i) -> do
--       maybe (pure ()) (const empty) =<< readIORef hitStop
--       m <- case roLimit ro of
--         Nothing -> pure poSplitRuns
--         Just l  -> do
--           guard (i < l) $>
--             if i + poSplitRuns <= l
--               then poSplitRuns
--               else l - i
--       (xs, os') <- lift . fmap unzip . forConcurrently os $ \o ->
--         runOpto (ro { roLimit    = Just m
--                     , roStopCond = \d y -> do
--                         s <- roStopCond ro d y
--                         when s $ writeIORef hitStop $ Just y
--                         return s
--                     }
--                 ) x o
--       newX <- fromMaybe (mean xs) <$> readIORef hitStop
--       return ((), ((newX, os'), i + m))

-- -- | Optimize an @a@ times in parallel, and aggregate all the results
-- -- together by taking the mean.
-- evalOptoParallel
--     :: (MonadUnliftIO m, Fractional a)
--     => RunOpts m a
--     -> ParallelOpts
--     -> a
--     -> OptoM m v a
--     -> m a
-- evalOptoParallel ro po x = fmap fst . runOptoParallel ro po x
