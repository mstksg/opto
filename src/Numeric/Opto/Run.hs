{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

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
  -- * Single-threaded
    RunOpts(..), noStop
  , runOpto, evalOpto
  , runOptoAlt, evalOptoAlt
  -- * Parallel
  , ParallelOpts(..)
  , runOptoParallel
  , evalOptoParallel
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import           Data.Functor
import           Data.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update
import           UnliftIO.Async
import           UnliftIO.Concurrent
import           UnliftIO.IORef

-- | Options for running an optimizer.
data RunOpts m a = RO
    { roStopCond :: Diff a -> a -> m Bool
    , roLimit    :: Maybe Int
    , roBatch    :: Maybe Int
    }

-- | Options for running an optimizer in a concurrent setting.
data ParallelOpts = PO { poThreads   :: Maybe Int
                       , poSplitRuns :: Int         -- ^ how much each thread will process before regrouping
                       }


-- | Construct a 'RunOpts' without a stopping condition.
noStop :: Applicative m => Maybe Int -> Maybe Int -> RunOpts m a
noStop = RO (\_ _ -> pure False)

-- | A version of 'runOpto', but allowing the stopping condition to take
-- advantage of an underlying 'Alternative' instance, where 'empty'/'mzero'
-- will quit immediately.
runOptoAlt
    :: forall m v a. MonadPlus m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
runOptoAlt ro x o = runOptoAlt_ ro x o (liftA2 (,))

-- | A version of 'evalOpto', but allowing the stopping condition to take
-- advantage of an underlying 'Alternative' instance, where 'empty'/'mzero'
-- will quit immediately.
evalOptoAlt
    :: forall m v a. MonadPlus m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m a
evalOptoAlt ro x o = runOptoAlt_ ro x o const

runOptoAlt_
    :: forall m v a r. MonadPlus m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> (m a -> m (OptoM m v a) -> m r)
    -> m r
runOptoAlt_ RO{..} x0 MkOptoM{..} f = do
    rSs <- thawRefs oInit
    rX  <- thawRef @m @a @v x0
    optoLoop roLimit roBatch
        (thawRef @m @a @v)
        (.*+=)
        freezeRef
        rX
        (oUpdate rSs)
        roStopCond
    f (freezeRef rX)
      (flip MkOptoM oUpdate <$> pullRefs rSs)
{-# INLINE runOptoAlt_ #-}

-- | Optimize an @a@ many times, until the stopping condition or limit is
-- reached.  Returns a frozen updated optimizer state.
runOpto
    :: forall m v a. Monad m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
runOpto ro x0 o = runOpto_ ro x0 o (liftA2 (,))

-- | Optimize an @a@ many times, until the stopping condition or limit is
-- reached.  Returns a frozen updated optimizer state.
evalOpto
    :: forall m v a. Monad m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m a
evalOpto ro x0 o = runOpto_ ro x0 o const

runOpto_
    :: forall m v a r. Monad m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> (m a -> m (OptoM m v a) -> m r)
    -> m r
runOpto_ RO{..} x0 MkOptoM{..} f = do
    rSs <- thawRefs oInit
    rX  <- thawRef @m @a @v x0
    _ <- runMaybeT $ optoLoop roLimit roBatch
            (lift . thawRef @m @a @v)
            (\v -> lift . (v .*+=))
            (lift . freezeRef)
            rX
            (lift . oUpdate rSs)
            (\d -> lift . roStopCond d)
    f (freezeRef rX)
      (flip MkOptoM oUpdate <$> pullRefs rSs)
{-# INLINE runOpto_ #-}

optoLoop
    :: forall m v a c. (Alternative m, Monad m, Scaling c a)
    => Maybe Int
    -> Maybe Int
    -> (a -> m v)
    -> (v -> (c, a) -> m ())
    -> (v -> m a)
    -> v
    -> (a -> m (c, a))
    -> (Diff a -> a -> m Bool)
    -> m ()
optoLoop lim batch initXVar updateXVar readXVar xVar updateState stop = repeatLim $ do
    x <- readXVar xVar
    (c, g) <- repeatBatch x
    updateXVar xVar (c, g)
    guard . not =<< stop (c .* g) x
  where
    repeatLim = case lim of
      Nothing -> void . many
      Just l  -> replicateM_ l
    repeatBatch !x = case batch of
      Nothing -> updateState x
      Just b  -> do
        v <- initXVar addZero
        -- TODO: should state be reset?
        replicateM_ b $
          updateXVar v =<< updateState x
        (scaleOne @c @a,) <$> readXVar v
{-# INLINE optoLoop #-}

mean :: (Foldable t, Fractional a) => t a -> a
mean = go . foldMap (`Sum2` 1)
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

-- | Optimize an @a@ times in parallel, and aggregate all the results
-- together by taking the mean.  Freezes and returns all of the updated
-- optimizer states from each thread.
runOptoParallel
    :: (MonadUnliftIO m, Fractional a)
    => RunOpts m a
    -> ParallelOpts
    -> a
    -> OptoM m v a
    -> m (a, [OptoM m v a])
runOptoParallel ro PO{..} x0 o0 = do
    n <- maybe getNumCapabilities pure poThreads
    hitStop <- newIORef Nothing
    let os0 = replicate n o0
    fmap (maybe (x0, os0) fst)
        . runMaybeT
        . flip execStateT ((x0, os0), 0)
        . many
        . StateT $ \((x,os), i) -> do
      maybe (pure ()) (const empty) =<< readIORef hitStop
      m <- case roLimit ro of
        Nothing -> pure poSplitRuns
        Just l  -> do
          guard (i < l) $>
            if i + poSplitRuns <= l
              then poSplitRuns
              else l - i
      (xs, os') <- lift . fmap unzip . forConcurrently os $ \o ->
        runOpto (ro { roLimit    = Just m
                    , roStopCond = \d y -> do
                        s <- roStopCond ro d y
                        when s $ writeIORef hitStop $ Just y
                        return s
                    }
                ) x o
      newX <- fromMaybe (mean xs) <$> readIORef hitStop
      return ((), ((newX, os'), i + m))

evalOptoParallel
    :: (MonadUnliftIO m, Fractional a)
    => RunOpts m a
    -> ParallelOpts
    -> a
    -> OptoM m v a
    -> m a
evalOptoParallel ro po x = fmap fst . runOptoParallel ro po x
