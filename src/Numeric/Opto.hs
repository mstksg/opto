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

module Numeric.Opto (
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Data.Default
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Kind
import           Data.Primitive
import           Data.Primitive.MutVar
import           Data.Proxy
import           Unsafe.Coerce

class Ref m a v | v -> a where
    newRef     :: a -> m v
    readRef    :: v -> m a
    writeRef   :: v -> a -> m ()
    modifyRef  :: v -> (a -> a) -> m ()
    modifyRef' :: v -> (a -> a) -> m ()

instance (PrimMonad m, PrimState m ~ s) => Ref m a (MutVar s a) where
    newRef     = newMutVar
    readRef    = readMutVar
    writeRef   = writeMutVar
    modifyRef  = modifyMutVar
    modifyRef' = modifyMutVar'

instance Applicative m => Ref m () () where
    newRef     = \_ -> pure ()
    readRef    = \_ -> pure ()
    writeRef   = \_ _ -> pure ()
    modifyRef  = \_ _ -> pure ()
    modifyRef' = \_ _ -> pure ()

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: (Ref m s sVar, Ref m b bVar)
            => { oSVar   :: Proxy sVar
               , oInit   :: !s
               , oGrad   :: !(a -> b -> m b)
               , oUpdate :: !(sVar -> bVar -> b -> m ())
               }
            -> OptoM m a b

type Opto s = OptoM (ST s)

fromCopying
    :: PrimMonad m
    => s
    -> (a -> b -> m b)
    -> (b -> b -> s -> m (b, s))  -- ^ gradient, value, state
    -> OptoM m a b
fromCopying s0 grad update =
    MkOptoM { oSVar   = Proxy
            , oInit   = s0
            , oGrad   = grad
            , oUpdate = \rS rY g -> do
                y <- readMutVar rY
                s <- readMutVar rS
                (y', s') <- update g y s
                writeMutVar rS s'
                writeMutVar rY y'
            }

fromPure
    :: PrimMonad m
    => s
    -> (a -> b -> b)
    -> (b -> b -> s -> (b, s))  -- ^ gradient, value, state
    -> OptoM m a b
fromPure s0 grad update = fromCopying s0 (\x   -> return . grad x)
                                         (\g y -> return . update g y)

scanOptoM
    :: (Monad m, Foldable t)
    => t a
    -> b
    -> OptoM m a b
    -> m (b, OptoM m a b)
scanOptoM xs = scanOptoUntilM xs (\_ _ -> pure False)

scanOpto
    :: Foldable t
    => t a
    -> b
    -> (forall s. Opto s a b)
    -> (b, Opto s a b)
scanOpto xs = scanOptoUntil xs (\_ _ -> False)

scanOptoUntilM
    :: (Monad m, Foldable t)
    => t a
    -> (b -> b -> m Bool)       -- ^ grad, current
    -> b
    -> OptoM m a b
    -> m (b, OptoM m a b)
scanOptoUntilM xs stop y0 MkOptoM{..} = do
    rS <- newRef oInit
    rY <- newRef y0
    runMaybeT . forM_ xs $ \x -> do
      y <- lift $ readRef rY
      g <- lift $ oGrad x =<< readRef rY
      guard . not =<< lift (stop g y)
      lift $ oUpdate rS rY g
    s' <- readRef rS
    let o' = MkOptoM Proxy s' oGrad oUpdate
    (, o') <$> readRef rY

scanOptoUntil
    :: Foldable t
    => t a
    -> (b -> b -> Bool)         -- ^ grad, current
    -> b
    -> (forall s. Opto s a b)
    -> (b, Opto s a b)
scanOptoUntil xs stop y0 o0 = runST $ do
    (y', o') <- scanOptoUntilM xs (\g -> pure . stop g) y0 o0
    return (y', unsafeCoerce o')        -- is this safe?  probably.

iterateOptoM
    :: Monad m
    => (b -> b -> m Bool)   -- ^ grad, current
    -> b
    -> OptoM m () b
    -> m (b, OptoM m () b)
iterateOptoM = scanOptoUntilM (repeat ())

iterateOpto
    :: (b -> b -> Bool)   -- ^ grad, current
    -> b
    -> (forall s. Opto s () b)
    -> (b, Opto s () b)
iterateOpto = scanOptoUntil (repeat ())

sgdOptimizerM
    :: (Fractional b, Ref m b v, Applicative m)
    => Double
    -> (a -> b -> m b)          -- ^ gradient
    -> (v -> b -> m ())         -- ^ adding action
    -> OptoM m a b
sgdOptimizerM r gr upd =
    MkOptoM { oSVar   = Proxy @()
            , oInit   = ()
            , oGrad   = gr
            , oUpdate = \_ rY g -> upd rY (realToFrac (- r) * g)
            }

sgdOptimizer
    :: (Fractional b, PrimMonad m)
    => Double
    -> (a -> b -> b)
    -> OptoM m a b
sgdOptimizer r gr = sgdOptimizerM r (\x -> return . gr x) $ \rY u ->
    modifyMutVar' rY (+ u)

gdOptimizerM
    :: (Fractional b, Ref m b v, Applicative m)
    => Double
    -> (b -> b)             -- ^ gradient
    -> (v -> b -> m ())     -- ^ adding action
    -> OptoM m () b
gdOptimizerM r gr upd = sgdOptimizerM r (const (pure . gr)) upd

gdOptimizer
    :: (Fractional b, PrimMonad m)
    => Double
    -> (b -> b)             -- ^ gradient
    -> OptoM m () b
gdOptimizer r gr = sgdOptimizer r (const gr)

data Adam = Adam
    { adamStep    :: Double
    , adamDecay1  :: Double
    , adamDecay2  :: Double
    , adamEpsilon :: Double
    }
  deriving (Show, Eq)

instance Default Adam where
    def = Adam { adamStep    = 0.001
               , adamDecay1  = 0.9
               , adamDecay2  = 0.999
               , adamEpsilon = 1e-8
               }

adamOptimizerM
    :: forall a b v m. (Floating b, Ref m b v, PrimMonad m)
    => Adam
    -> (a -> b -> m b)          -- ^ gradient
    -> (v -> b -> m ())         -- ^ adding
    -> OptoM m a b
adamOptimizerM Adam{..} gr upd = 
    MkOptoM { oSVar   = Proxy @(MutVar (PrimState m) (Double, b, b))
            , oInit   = (1,0,0)
            , oGrad   = gr
            , oUpdate = \rS rB g -> do
                (t, m0, v0) <- readRef rS
                let m1 = realToFrac adamDecay1 * m0
                       + realToFrac (1 - adamDecay1) * g
                    v1 = realToFrac adamDecay2 * m0
                       + realToFrac (1 - adamDecay2) * g
                    mHat = m1 / (1 - realToFrac (adamDecay1 ** t))
                    vHat = v1 / (1 - realToFrac (adamDecay2 ** t))
                upd rB $ realToFrac (- adamStep)
                         * mHat
                         / (sqrt vHat + realToFrac adamEpsilon)
            }

adamOptimizer
    :: forall a b v m. (Floating b, PrimMonad m)
    => Adam
    -> (a -> b -> b)          -- ^ gradient
    -> OptoM m a b
adamOptimizer a gr = adamOptimizerM a (\x -> return . gr x) $ \rY u ->
    modifyMutVar' rY (+ u)
