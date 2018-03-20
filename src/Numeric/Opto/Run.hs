{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run (
    iterateOptoM, iterateOptoM_
  , iterateOpto, iterateOpto_
  , iterateSamplingUntil, iterateSamplingUntil_
  , iterateSampling, iterateSampling_
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Sample
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update
import           Unsafe.Coerce


iterateOptoM
    :: forall m v a. Monad m
    => Grad m a                     -- ^ Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m (a, OptoM m v a)           -- ^ Optimized value with updated optimizer
iterateOptoM gr stop lim x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT . repeatLim lim $ do
      (x, step) <- lift $ do
        x <- readRef rX
        (c, g) <- update rSs x
        rX .*+= (c, g)
        return (x, c .* g)
      guard . not =<< lift (stop step x)
    s <- pullRefs rSs
    let o' = MkOptoM s oUpdate
    (, o') <$> readRef rX
  where
    update = oUpdate gr

repeatLim :: Alternative m => Maybe Int -> m a -> m [a]
repeatLim Nothing  = many
repeatLim (Just l) = replicateM l

iterateOptoM_
    :: forall m v a. Monad m
    => Grad m a                     -- ^ Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m a                          -- ^ Optimized value
iterateOptoM_ gr stop lim x0 = fmap fst . iterateOptoM gr stop lim x0

iterateOpto
    :: (a -> Diff a)                -- ^ Gradient
    -> (Diff a -> a -> Bool)        -- ^ Stopping condition
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> (forall s'. Opto s' v a)     -- ^ Pure optimizer
    -> (a, Opto s v a)              -- ^ Optimized value with updated optimizer
iterateOpto gr stop lim x0 o0 = runST $ do
    (y', o') <- iterateOptoM (pure . gr) (\st -> pure . stop st) lim x0 o0
    return (y', unsafeCoerce o')        -- is this safe?  probably.

iterateOpto_
    :: (a -> Diff a)                -- ^ Gradient
    -> (Diff a -> a -> Bool)        -- ^ Stopping condition
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> (forall s'. Opto s' v a)     -- ^ Pure optimizer
    -> a                            -- ^ Optimized value
iterateOpto_ gr stop lim x0 o0 = fst $ iterateOpto gr stop lim x0 o0

iterateSamplingUntil
    :: forall m v r a. MonadSample r m
    => GradSample m r a             -- ^ (Sampling) Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m (a, OptoM m v a)           -- ^ Optimized value with updated optimizer
iterateSamplingUntil gr stop x0 MkOptoM{..} = do
    rS <- initRefs oInit
    rX <- newRef @_ @a @v x0
    _ <- many $ do
      y      <- readRef rX
      (c, g) <- update rS =<< readRef rX
      rX .*+= (c, g)
      guard . not =<< stop (c .* g) y
    s <- pullRefs rS
    let o' = MkOptoM s oUpdate
    (, o') <$> readRef rX
  where
    update = oUpdate (sampling gr)

iterateSamplingUntil_
    :: forall m v r a. MonadSample r m
    => GradSample m r a             -- ^ (Sampling) Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m a                          -- ^ Optimized value
iterateSamplingUntil_ gr stop x0 = fmap fst . iterateSamplingUntil gr stop x0

iterateSampling
    :: MonadSample r m
    => GradSample m r a             -- ^ (Sampling) gradient
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m (a, OptoM m v a)           -- ^ Optimized value with updated optimizer
iterateSampling gr = iterateSamplingUntil gr (\_ _ -> pure False)

iterateSampling_
    :: MonadSample r m
    => GradSample m r a             -- ^ (Sampling) gradient
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m a                          -- ^ Optimized value
iterateSampling_ gr x0 = fmap fst . iterateSampling gr x0
