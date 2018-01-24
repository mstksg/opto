{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Opto (
    Ref(..)
  , Additive(..), Scaling(..)
  , AdditiveInPlace(..), ScalingInPlace(..)
  , Step, OptoM(..), Opto
  , fromCopying, fromPure
  , scanOptoM, scanOpto
  , scanOptoUntilM, scanOptoUntil
  , iterateOptoM, iterateOpto
  , sgdOptimizerM, sgdOptimizer
  , gdOptimizerM, gdOptimizer
  , Adam(..), adamOptimizerM, adamOptimizer
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Kind
import           Data.Primitive.MutVar
import           Numeric.Opto.Ref
import           Numeric.Opto.Step
import           Unsafe.Coerce

type Step a = a

data OptoM :: (Type -> Type) -> Type -> Type -> Type -> Type where
    MkOptoM :: (Ref m s sVar, ScalingInPlace m v c a)
            => { oInit   :: !s
               , oUpdate :: !(sVar -> r -> a -> m (c, Step a))
               }
            -> OptoM m v r a

type Opto s = OptoM (ST s)

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (r -> a -> s -> m (c, Step a, s))
    -> OptoM m v r a
fromCopying s0 update =
    MkOptoM { oInit   = s0
            , oUpdate = \rS r x -> do
                (c, g, s) <- update r x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromPure
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (r -> a -> s -> (c, Step a, s))
    -> OptoM m v r a
fromPure s0 update = fromCopying s0 (\r x -> pure . update r x)

scanOptoUntilM
    :: forall m v r a t. (Monad m, Foldable t)
    => t r
    -> (Step a -> a -> m Bool)       -- ^ step, current
    -> a
    -> OptoM m v r a
    -> m (a, OptoM m v r a)
scanOptoUntilM rs stop x0 MkOptoM{..} = do
    rS <- newRef oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT . forM_ rs $ \r -> do
      (y, step) <- lift $ do
        y      <- readRef rX
        (c, g) <- oUpdate rS r =<< readRef rX
        rX .*+= (c, g)
        return (y, c .* g)
      guard . not =<< lift (stop step y)
    s' <- readRef rS
    let o' = MkOptoM s' oUpdate
    (, o') <$> readRef rX

scanOptoUntil
    :: Foldable t
    => t r
    -> (Step a -> a -> Bool)         -- ^ step, current
    -> a
    -> (forall s'. Opto s' v r a)
    -> (a, Opto s v r a)
scanOptoUntil xs stop y0 o0 = runST $ do
    (y', o') <- scanOptoUntilM xs (\s -> pure . stop s) y0 o0
    return (y', unsafeCoerce o')        -- is this safe?  probably.

scanOptoM
    :: (Monad m, Foldable t)
    => t r
    -> a
    -> OptoM m v r a
    -> m (a, OptoM m v r a)
scanOptoM xs = scanOptoUntilM xs (\_ _ -> pure False)

scanOpto
    :: Foldable t
    => t r
    -> a
    -> (forall s'. Opto s' v r a)
    -> (a, Opto s v r a)
scanOpto xs = scanOptoUntil xs (\_ _ -> False)

iterateOptoM
    :: Monad m
    => (Step a -> a -> m Bool)   -- ^ step, current
    -> a
    -> OptoM m v () a
    -> m (a, OptoM m v () a)
iterateOptoM = scanOptoUntilM (repeat ())

iterateOpto
    :: (Step a -> a -> Bool)   -- ^ step, current
    -> a
    -> (forall s'. Opto s' v () a)
    -> (a, Opto s v () a)
iterateOpto = scanOptoUntil (repeat ())

sgdOptimizerM
    :: (Ref m a v, ScalingInPlace m v c a, Applicative m)
    => c
    -> (r -> a -> m a)          -- ^ gradient
    -> OptoM m v r a
sgdOptimizerM lr gr =
    MkOptoM { oInit   = EmptyRef
            , oUpdate = \EmptyRef r x -> ((-lr),) <$> gr r x
            }

sgdOptimizer
    :: (Ref m a v, ScalingInPlace m v c a, Applicative m)
    => c
    -> (r -> a -> a)
    -> OptoM m v r a
sgdOptimizer lr gr = sgdOptimizerM lr (\x -> pure . gr x)

gdOptimizerM
    :: (ScalingInPlace m v c a, Applicative m)
    => c
    -> (a -> m a)           -- ^ gradient
    -> OptoM m v () a
gdOptimizerM lr upd = sgdOptimizerM lr (const upd)

gdOptimizer
    :: (ScalingInPlace m v c a, Applicative m)
    => c
    -> (a -> a)             -- ^ gradient
    -> OptoM m v () a
gdOptimizer r gr = sgdOptimizer r (const gr)

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

adamOptimizerM
    :: forall m v r a c. (RealFloat c, Floating a, ScalingInPlace m v c a, PrimMonad m)
    => Adam c
    -> (r -> a -> m a)          -- ^ gradient
    -> OptoM m v r a
adamOptimizerM Adam{..} gr = 
    MkOptoM { oInit   = (scaleOne @_ @a, addZero, addZero)
            , oUpdate = \rS r x -> do
                g <- gr r x
                (t, m0, v0) <- readMutVar rS
                let m1 = adamDecay1 .* m0 .+. (1 - adamDecay1) .* g
                    v1 = adamDecay2 .* v0 .+. (1 - adamDecay2) .* g
                    mHat = recip (1 - adamDecay1 ** t) .* m1
                    vHat = recip (1 - adamDecay2 ** t) .* v1
                writeMutVar rS (t + 1, m1, v1)
                return ( -adamStep
                       , mHat / (sqrt vHat + realToFrac adamEpsilon)
                       )
            }

adamOptimizer
    :: (RealFloat c, Floating a, ScalingInPlace m v c a, PrimMonad m)
    => Adam c
    -> (r -> a -> a)          -- ^ gradient
    -> OptoM m v r a
adamOptimizer a gr = adamOptimizerM a (\x -> pure . gr x)
