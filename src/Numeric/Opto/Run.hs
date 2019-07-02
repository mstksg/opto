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
  , runOptoMany, evalOptoMany
  , runOpto, evalOpto
  -- * Parallel
  , ParallelOpts(..)
  , runOptoManyParallel
  , runOptoParallel
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update
import           UnliftIO.Async
import           UnliftIO.Concurrent
import           UnliftIO.IORef

-- | Options for running an optimizer.
data RunOpts m a = RO { roStopCond :: Diff a -> a -> m Bool
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

-- | Optimize an @a@ many times, until the stopping condition or limit is
-- reached.  Return the updated optimizer state.
runOptoMany
    :: forall m v a. Alternative m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
runOptoMany RO{..} x0 MkOptoM{..} = do
    rSs <- thawRefs oInit
    rX  <- thawRef @m @a @v x0
    optoLoop roLimit roBatch
        (thawRef @m @a @v)
        (.*+=)
        freezeRef
        rX
        (oUpdate rSs)
        roStopCond
    o' <- flip MkOptoM oUpdate <$> pullRefs rSs
    (, o') <$> freezeRef rX

-- | Optimize an @a@ many times, until the stopping condition or limit is
-- reached.  Forget the updated optimizer state.
evalOptoMany
    :: forall m v a. Alternative m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m a
evalOptoMany ro x0 = fmap fst . runOptoMany ro x0

-- | Perform a single optimizer step, returning the updated optimizer
-- state.
runOpto
    :: forall m v a. Monad m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
runOpto RO{..} x0 MkOptoM{..} = do
    rSs <- thawRefs oInit
    rX  <- thawRef @m @a @v x0
    _ <- runMaybeT $ optoLoop roLimit roBatch
            (lift . thawRef @m @a @v)
            (\v -> lift . (v .*+=))
            (lift . freezeRef)
            rX
            (lift . oUpdate rSs)
            (\d -> lift . roStopCond d)
    o' <- flip MkOptoM oUpdate <$> pullRefs rSs
    (, o') <$> freezeRef rX

-- | Perform a single optimizer step, forgetting the updated optimizer
-- state.
evalOpto
    :: forall m v a. Monad m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m a
evalOpto ro x0 = fmap fst . runOpto ro x0

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
    repeatBatch x = case batch of
      Nothing -> updateState x
      Just b  -> do
        v <- initXVar addZero
        -- TODO: should state be reset?
        replicateM_ b $
          updateXVar v =<< updateState x
        (scaleOne @c @a,) <$> readXVar v

mean :: (Foldable t, Fractional a) => t a -> a
mean = go . foldMap (`Sum2` 1)
  where
    go (Sum2 x n) = x / fromInteger n

data Sum2 a b = Sum2 !a !b

instance (Num a, Num b) => Semigroup (Sum2 a b) where
    Sum2 x1 y1 <> Sum2 x2 y2 = Sum2 (x1 + x2) (y1 + y2)

instance (Num a, Num b) => Monoid (Sum2 a b) where
    mappend = (<>)
    mempty = Sum2 0 0

-- | Optimize an @a@ many times in parallel, and aggregate all the results
-- together by taking the mean.  Returns all of the updated optimizer
-- states from each thread.
runOptoManyParallel
    :: (MonadUnliftIO m, MonadPlus m, Fractional a)
    => RunOpts m a
    -> ParallelOpts
    -> a
    -> OptoM m v a
    -> m (a, [OptoM m v a])
runOptoManyParallel ro PO{..} x0 o0 = do
    n <- maybe getNumCapabilities pure poThreads
    hitStop <- newIORef Nothing
    let os0 = replicate n o0
    fmap fst . flip execStateT ((x0, os0), 0) . many . StateT $ \((x,os), i) -> do
      maybe (pure ()) (const empty) =<< readIORef hitStop
      m <- case roLimit ro of
        Nothing -> pure poSplitRuns
        Just l  -> do
          guard $ i < l
          pure $ if i + poSplitRuns <= l
                   then poSplitRuns
                   else l - i
      (xs, os') <- fmap unzip . forConcurrently os $ \o ->
        runOptoMany (ro { roLimit    = Just m
                        , roStopCond = \d y -> do
                            s <- roStopCond ro d y
                            when s $ writeIORef hitStop $ Just y
                            return s
                        }
                    ) x o
      newX <- fromMaybe (mean xs) <$> readIORef hitStop
      return ((), ((newX, os'), i + m))

-- | Preform a single optimization batch in parallel, returning all of the
-- updated optimizer states from each thread.
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
          guard $ i < l
          pure $ if i + poSplitRuns <= l
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

