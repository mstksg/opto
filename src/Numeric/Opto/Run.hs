{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run (
    runOptoManyUntil, runOptoManyUntil_
  , runOptoMany, runOptoMany_
  , runOptoUntil, runOptoUntil_
  , runOpto, runOpto_
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

optoLoop
    :: (Alternative m, Monad m, Scaling c a)
    => Maybe Int
    -> m a
    -> ((c, a) -> m ())
    -> (a -> m (c, a))
    -> (Diff a -> a -> m Bool)
    -> m ()
optoLoop lim readX updateX updateState stop = repeatLim $ do
    x <- readX
    (c, g) <- updateState x
    updateX (c, g)
    guard . not =<< stop (c .* g) x
  where
    repeatLim = case lim of
      Nothing -> void . many
      Just l  -> replicateM_ l

runOptoManyUntil
    :: forall m v a. Alternative m
    => Grad m a                     -- ^ Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m (a, OptoM m v a)           -- ^ Optimized value with updated optimizer
runOptoManyUntil gr stop lim x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    optoLoop lim (readRef rX) (rX .*+=) (update rSs) stop
    o' <- flip MkOptoM oUpdate <$> pullRefs rSs
    (, o') <$> readRef rX
  where
    update = oUpdate gr

runOptoManyUntil_
    :: forall m v a. Alternative m
    => Grad m a                     -- ^ Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m a                          -- ^ Optimized value with updated optimizer
runOptoManyUntil_ gr stop lim x0 = fmap fst . runOptoManyUntil gr stop lim x0

runOptoMany
    :: forall m v a. Alternative m
    => Grad m a                     -- ^ Gradient
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m (a, OptoM m v a)           -- ^ Optimized value with updated optimizer
runOptoMany = flip runOptoManyUntil (\_ _ -> pure False)

runOptoMany_
    :: forall m v a. Alternative m
    => Grad m a                     -- ^ Gradient
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m a                          -- ^ Optimized value with updated optimizer
runOptoMany_ = flip runOptoManyUntil_ (\_ _ -> pure False)

runOptoUntil
    :: forall m v a. Monad m
    => Grad m a                     -- ^ Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m (a, OptoM m v a)           -- ^ Optimized value with updated optimizer
runOptoUntil gr stop lim x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT $ optoLoop lim (lift (readRef rX))
                                  (lift . (rX .*+=))
                                  (lift . update rSs)
                                  (\d -> lift . stop d)
    o' <- flip MkOptoM oUpdate <$> pullRefs rSs
    (, o') <$> readRef rX
  where
    update = oUpdate gr

runOptoUntil_
    :: forall m v a. Monad m
    => Grad m a                     -- ^ Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m a                          -- ^ Optimized value with updated optimizer
runOptoUntil_ gr stop lim x0 = fmap fst . runOptoUntil gr stop lim x0

runOpto
    :: forall m v a. Monad m
    => Grad m a                     -- ^ Gradient
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m (a, OptoM m v a)           -- ^ Optimized value with updated optimizer
runOpto = flip runOptoUntil (\_ _ -> pure False)

runOpto_
    :: forall m v a. Monad m
    => Grad m a                     -- ^ Gradient
    -> Maybe Int                    -- ^ step limit
    -> a                            -- ^ Initial value
    -> OptoM m v a                  -- ^ Optimizer
    -> m a                          -- ^ Optimized value with updated optimizer
runOpto_ = flip runOptoUntil_ (\_ _ -> pure False)

