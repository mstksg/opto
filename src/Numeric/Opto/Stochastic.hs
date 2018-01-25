{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}

module Numeric.Opto.Stochastic (
    MonadSample(..)
  , fromCopying, fromPure
  , fromStateless, fromStatelessM
  , iterateStochUntilM
  , iterateStochM
  , sgdM, sgd
  , adam, adamM
  , adaMax, adaMaxM
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Default
import           Data.Primitive.MutVar
import           Numeric.Opto.Core       (OptoM(..), Step)
import           Numeric.Opto.Ref
import           Numeric.Opto.Sample
import           Numeric.Opto.Step
import qualified Numeric.Opto.Core       as OC

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a, MonadSample r m)
    => s
    -> (r -> a -> s -> m (c, Step a, s))
    -> OptoM m v a
fromCopying s0 update = OC.fromCopying s0 $ \x s -> do
    r <- sample
    update r x s

fromPure
    :: (PrimMonad m, ScalingInPlace m v c a, MonadSample r m)
    => s
    -> (r -> a -> s -> (c, Step a, s))
    -> OptoM m v a
fromPure s0 update = fromCopying s0 (\r x -> pure . update r x)

fromStatelessM
    :: (ScalingInPlace m v c a, MonadSample r m)
    => (r -> a -> m (c, Step a))
    -> OptoM m v a
fromStatelessM update =
    MkOptoM { oInit = EmptyRef
            , oUpdate = \case ~EmptyRef -> \x -> do
                                 r <- sample
                                 update r x
            }

fromStateless
    :: (ScalingInPlace m v c a, MonadSample r m)
    => (r -> a -> (c, Step a))
    -> OptoM m v a
fromStateless update = fromStatelessM (\r -> pure . update r)

iterateStochUntilM
    :: forall m v r a. (MonadSample r m)
    => (Step a -> a -> m Bool)       -- ^ step, current
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateStochUntilM stop x0 MkOptoM{..} = do
    rS <- newRef oInit
    rX <- newRef @_ @a @v x0
    _ <- many $ do
      y      <- readRef rX
      (c, g) <- oUpdate rS =<< readRef rX
      rX .*+= (c, g)
      guard . not =<< stop (c .* g) y
    s <- readRef rS
    let o' = MkOptoM s oUpdate
    (, o') <$> readRef rX

iterateStochM
    :: MonadSample r m
    => a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateStochM = iterateStochUntilM (\_ _ -> pure False)

sgdM
    :: (ScalingInPlace m v c a, MonadSample r m)
    => c
    -> (r -> a -> m a)          -- ^ gradient
    -> OptoM m v a
sgdM lr gr = fromStatelessM $ \r -> fmap (-lr,) . gr r

sgd
    :: (ScalingInPlace m v c a, MonadSample r m)
    => c
    -> (r -> a -> a)
    -> OptoM m v a
sgd lr gr = sgdM lr (\x -> pure . gr x)

data Adam c = Adam
    { adamStep    :: !c
    , adamDecay1  :: !c
    , adamDecay2  :: !c
    , adamEpsilon :: !c
    }
  deriving (Show, Eq)

instance Fractional a => Default (Adam a) where
    def = Adam { adamStep    = 0.001
               , adamDecay1  = 0.9
               , adamDecay2  = 0.999
               , adamEpsilon = 1e-8
               }

adamM
    :: forall m r v a c.
     ( RealFloat c
     , Floating a
     , ScalingInPlace m v c a
     , MonadSample r m
     , PrimMonad m
     )
    => Adam c
    -> (r -> a -> m a)          -- ^ gradient
    -> OptoM m v a
adamM Adam{..} gr =
    MkOptoM { oInit   = (scaleOne @_ @a, addZero @a, addZero @a)
            , oUpdate = \rS x -> sample >>= \r -> do
                g <- gr r x
                (t0, m0, v0) <- readMutVar rS
                let t1 = t0 + 1
                    m1 = adamDecay1 .* m0 .+. (1 - adamDecay1) .* g
                    v1 = adamDecay2 .* v0 .+. (1 - adamDecay2) .* g
                    mHat = recip (1 - adamDecay1 ** t1) .* m1
                    vHat = recip (1 - adamDecay2 ** t1) .* v1
                writeMutVar rS (t1, m1, v1)
                return ( -adamStep
                       , mHat / (sqrt vHat + realToFrac adamEpsilon)
                       )
            }

adam
    :: forall m r v a c.
     ( RealFloat c
     , Floating a
     , ScalingInPlace m v c a
     , MonadSample r m
     , PrimMonad m
     )
    => Adam c
    -> (r -> a -> a)          -- ^ gradient
    -> OptoM m v a
adam a gr = adamM a (\x -> pure . gr x)


data AdaMax c = AdaMax
    { adaMaxStep    :: !c
    , adaMaxDecay1  :: !c
    , adaMaxDecay2  :: !c
    , adaMaxEpsilon :: !c
    }
  deriving (Show, Eq)

instance Fractional a => Default (AdaMax a) where
    def = AdaMax { adaMaxStep    = 0.002
                 , adaMaxDecay1  = 0.9
                 , adaMaxDecay2  = 0.999
                 , adaMaxEpsilon = 1e-8
                 }

adaMaxM
    :: forall m r v a c.
     ( RealFloat c
     , Floating a
     , Metric c a
     , ScalingInPlace m v c a
     , MonadSample r m
     , PrimMonad m
     )
    => AdaMax c
    -> (r -> a -> m a)          -- ^ gradient
    -> OptoM m v a
adaMaxM AdaMax{..} gr =
    MkOptoM { oInit   = (scaleOne @_ @a, addZero @a, 0 :: c)
            , oUpdate = \rS x -> sample >>= \r -> do
                g <- gr r x
                (t0, m0, u0) <- readMutVar rS
                let t1 = t0 + 1
                    m1 = adaMaxDecay1 .* m0 .+. (1 - adaMaxDecay1) .* g
                    u1 = max (adaMaxDecay2 * u0) (norm_inf g)
                writeMutVar rS (t1, m1, u1)
                return ( -adaMaxStep / ((1 - adaMaxDecay1 ** t1) * u1)
                       , m1
                       )
            }

adaMax
    :: forall m r v a c.
     ( RealFloat c
     , Floating a
     , Metric c a
     , ScalingInPlace m v c a
     , MonadSample r m
     , PrimMonad m
     )
    => AdaMax c
    -> (r -> a -> a)          -- ^ gradient
    -> OptoM m v a
adaMax a gr = adaMaxM a (\x -> pure . gr x)
