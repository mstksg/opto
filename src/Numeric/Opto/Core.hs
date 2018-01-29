{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Numeric.Opto.Core (
    Diff, Grad, OptoM(..), Opto
  , fromCopying, fromPure, fromStateless, fromStatelessM
  , GradSample, sampling, batchSampling
  , batching
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Sample
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Type.ZipProd
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

type Diff   a = a
type Grad m a = a -> m (Diff a)

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: ScalingInPlace m v c a
            => { oInit   :: !( RefInits m ss sVars )
               , oUpdate :: !( Grad m a
                            -> RefVars m ss sVars
                            -> a
                            -> m (c, Diff a)
                             )
               }
            -> OptoM m v a

type Opto s = OptoM (ST s)

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (Grad m a -> a -> s -> m (c, Diff a, s))
    -> OptoM m v a
fromCopying s0 update =
    MkOptoM { oInit   = onlyZP (RI s0)
            , oUpdate = \gr (headZP->RV rS) x -> do
                (c, g, s) <- update gr x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromPure
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (Grad m a -> a -> s -> (c, Diff a, s))
    -> OptoM m v a
fromPure s0 update =
    MkOptoM { oInit   = onlyZP (RI s0)
            , oUpdate = \gr (headZP->RV rS) x -> atomicModifyMutVar' rS $ \s ->
                  let (c, g, s') = update gr x s
                  in  (s', (c, g))
            }

fromStatelessM
    :: ScalingInPlace m v c a
    => (Grad m a -> a -> m (c, Diff a))
    -> OptoM m v a
fromStatelessM update =
    MkOptoM { oInit   = ZPØ
            , oUpdate = \gr _ -> update gr
            }

fromStateless
    :: ScalingInPlace m v c a
    => (Grad m a -> a -> (c, Diff a))
    -> OptoM m v a
fromStateless update = fromStatelessM (\gr -> pure . update gr)

type GradSample m r a = r -> Grad m a

sampling
    :: MonadSample r m
    => GradSample m r a
    -> Grad m a
sampling f x = do
    r <- sample
    f r x

batchSampling
    :: (MonadSample r m, Additive r)
    => Int
    -> GradSample m r a
    -> Grad m a
batchSampling n f x = do
    r <- sumAdditive <$> sampleN n
    f r x

batching
    :: Int
    -> OptoM m v a
    -> OptoM m v a
batching n MkOptoM{..} =
    MkOptoM { oInit   = oInit
            , oUpdate = \gr -> oUpdate $ \x ->
                sumAdditive <$> replicateM n (gr x)
            }
