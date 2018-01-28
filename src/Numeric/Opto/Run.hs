{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run (
    iterateOptoM, iterateOpto
  , iterateSamplingUntil
  , iterateSampling
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
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateOptoM gr stop x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT . many $ do
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

iterateOpto
    :: (a -> Diff a)                -- ^ Gradient
    -> (Diff a -> a -> Bool)        -- ^ Stopping condition
    -> a
    -> (forall s'. Opto s' v a)
    -> (a, Opto s v a)
iterateOpto gr stop y0 o0 = runST $ do
    (y', o') <- iterateOptoM (pure . gr) (\st -> pure . stop st) y0 o0
    return (y', unsafeCoerce o')        -- is this safe?  probably.

iterateSamplingUntil
    :: forall m v r a. MonadSample r m
    => GradSample m r a              -- ^ Gradient
    -> (Diff a -> a -> m Bool)       -- ^ Stopping condition
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
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

iterateSampling
    :: MonadSample r m
    => GradSample m r a
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateSampling gr = iterateSamplingUntil gr (\_ _ -> pure False)
