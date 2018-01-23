{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Numeric.Opto (
  ) where

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Foldable
import           Data.Kind
import           Data.Primitive
import           Data.Primitive.MutVar
import           Unsafe.Coerce

data Ref m v a = Ref { rNew    :: !(a -> m v)
                     , rRead   :: !(v -> m a)
                     , rModify :: !(v -> (a -> a) -> m ())
                     }

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: { oInit   :: !s
               , oRefS   :: !(Ref m sVar s)
               , oRefB   :: !(Ref m bVar b)
               , oGrad   :: !(a -> b -> m b)
               , oUpdate :: !(sVar -> bVar -> b -> m ())
               }
            -> OptoM m a b

type Opto s = OptoM (ST s)

mutVarRef :: PrimMonad m => Ref m (MutVar (PrimState m) a) a
mutVarRef = Ref newMutVar readMutVar modifyMutVar'

fromPure
    :: PrimMonad m
    => t
    -> (a -> b -> b)
    -> (b -> b -> t -> (b, t))  -- ^ gradient, value, state
    -> OptoM m a b
fromPure s0 grad update =
    MkOptoM { oInit   = s0
            , oRefS   = mutVarRef
            , oRefB   = mutVarRef
            , oGrad   = \x       -> return . grad x
            , oUpdate = \rS rY g -> do
                (y', s') <- update g <$> readMutVar rY <*> readMutVar rS
                writeMutVar rS s'
                writeMutVar rY y'
            }

scanOptoM
    :: (PrimMonad m, Foldable t)
    => t a
    -> b
    -> OptoM m a b
    -> m (b, OptoM m a b)
scanOptoM xs y0 MkOptoM{..} = do
    rS <- rNew oRefS oInit
    rY <- rNew oRefB y0
    forM_ xs $ \x -> do
      g <- oGrad x =<< rRead oRefB rY
      oUpdate rS rY g
    s' <- rRead oRefS rS
    let o' = MkOptoM s' oRefS oRefB oGrad oUpdate
    (, o') <$> rRead oRefB rY

scanOpto
    :: Foldable t
    => t a
    -> b
    -> (forall s. Opto s a b)
    -> (b, Opto s a b)
scanOpto xs y0 o0 = runST $ do
    (y', o') <- scanOptoM xs y0 o0
    return (y', unsafeCoerce o')        -- is this safe?  probably.

iterateOptoM
    :: b
    -> OptoM m () b
    -> m b
iterateOptoM = undefined

sgdOptimizerM
    :: (Fractional b, PrimMonad m)
    => Double
    -> (a -> b -> m b)
    -> (MutVar (PrimState m) b -> b -> m ())        -- ^ adding action
    -> OptoM m a b
sgdOptimizerM r gr upd =
    MkOptoM { oInit   = ()
            , oRefS   = mutVarRef
            , oRefB   = mutVarRef
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

