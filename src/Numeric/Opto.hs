{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
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

-- data Ref m v a = Ref { rNew    :: !(a -> m v)
--                      , rRead   :: !(v -> m a)
--                      , rModify :: !(v -> (a -> a) -> m ())
--                      }

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: (Ref m s sVar, Ref m b bVar)
            => { oSVar   :: Proxy sVar
               , oInit   :: !s
               , oGrad   :: !(a -> b -> m b)
               , oUpdate :: !(sVar -> bVar -> b -> m ())
               }
            -> OptoM m a b

type Opto s = OptoM (ST s)

fromPure
    :: PrimMonad m
    => t
    -> (a -> b -> b)
    -> (b -> b -> t -> (b, t))  -- ^ gradient, value, state
    -> OptoM m a b
fromPure s0 grad update =
    MkOptoM { oSVar   = Proxy
            , oInit   = s0
            , oGrad   = \x       -> return . grad x
            , oUpdate = \rS rY g -> do
                (y', s') <- update g <$> readMutVar rY <*> readMutVar rS
                writeMutVar rS s'
                writeMutVar rY y'
            }

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
    -> (a -> b -> m b)
    -> (v -> b -> m ())        -- ^ adding action
    -> OptoM m a b
sgdOptimizerM r gr upd =
    MkOptoM { oSVar   = Proxy @()
            , oInit   = ()
            , oGrad   = gr
            , oUpdate = \_ rY g -> upd rY (realToFrac r * g)
            }

sgdOptimizer
    :: (Fractional b, PrimMonad m)
    => Double
    -> (a -> b -> b)
    -> OptoM m a b
sgdOptimizer r gr = sgdOptimizerM r (\x -> return . gr x) $ \rY u ->
    modifyMutVar' rY (+ u)
