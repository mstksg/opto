{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Numeric.Opto.Core (
    Diff, Grad, OptoM(..), Opto
  , fromCopying, fromStateless
  , sampling
  , pureGrad, pureSampling
  , ConduitSample(..), conduitSample
  ) where

-- import           Control.Monad.Sample
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Type.ZipProd
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

type Diff   a = a
type Grad m a = a -> m (Diff a)

newtype ConduitSample i o m a = ConduitSample
    { runConduitSample :: ConduitT i o m (Maybe a) }
  deriving (Functor)
  deriving (Applicative, Monad, Alternative, MonadPlus) via (MaybeT (ConduitT i o m))

instance PrimMonad m => PrimMonad (ConduitSample i o m) where
    type PrimState (ConduitSample i o m) = PrimState m
    primitive = lift . primitive

instance MonadTrans (ConduitSample i o) where
    lift = ConduitSample . lift . fmap Just

conduitSample :: ConduitT i o m a -> ConduitSample i o m a
conduitSample = ConduitSample . fmap Just

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: ScalingInPlace m v c a
            => { oInit   :: !( RefVals m ss sVars )
               , oUpdate :: !( RefVars m ss sVars
                            -> a
                            -> m (c, Diff a)
                             )
               }
            -> OptoM m v a

type Opto s = OptoM (ST s)

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (a -> s -> m (c, Diff a, s))
    -> OptoM m v a
fromCopying s0 update =
    MkOptoM { oInit   = onlyZP (RVl s0)
            , oUpdate = \(headZP->RVr rS) x -> do
                (c, g, s) <- update x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromStateless
    :: ScalingInPlace m v c a
    => (a -> m (c, Diff a))
    -> OptoM m v a
fromStateless update =
    MkOptoM { oInit   = ZPØ
            , oUpdate = \_ -> update
            }

sampling
    :: Monad m
    => (r -> Grad m a)
    -> Grad (ConduitSample r o m) a
sampling f x = do
    r <- ConduitSample await
    lift $ f r x

pureGrad
    :: Applicative m
    => (a -> Diff a)
    -> Grad m a
pureGrad f = pure . f

pureSampling
    :: Monad m
    => (r -> a -> Diff a)
    -> Grad (ConduitSample r o m) a
pureSampling f = sampling (pureGrad . f)
