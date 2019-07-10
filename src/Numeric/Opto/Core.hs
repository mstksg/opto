{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Numeric.Opto.Core (
    Diff, Grad, Opto(..)
  , mapSample
  , fromCopying, fromStateless
  , pureGrad
  , nonSampling, pureNonSampling
  ) where

import           Control.Monad.Primitive
import           Data.Kind
import           Data.Primitive.MutVar
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

type Diff     a = a
type Grad m r a = r -> a -> m (Diff a)

data Opto :: (Type -> Type) -> Type -> Type -> Type -> Type where
    MkOpto :: forall s u m v r a c. (ScalingInPlace m v c a, Ref m s u)
           => { oInit   :: !s
              , oUpdate :: !( u
                           -> r
                           -> a
                           -> m (c, Diff a)
                            )
              }
           -> Opto m v r a

mapSample
    :: (r -> s)
    -> Opto m v s a
    -> Opto m v r a
mapSample f MkOpto{..} = MkOpto
    { oInit   = oInit
    , oUpdate = \u r -> oUpdate u (f r)
    }

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (r -> a -> s -> m (c, Diff a, s))
    -> Opto m v r a
fromCopying s0 update = MkOpto
    { oInit    = s0
    , oUpdate = \rS r x -> do
        (c, g, s) <- update r x =<< readMutVar rS
        writeMutVar rS s
        return (c, g)
    }

fromStateless
    :: (ScalingInPlace m v c a)
    => (r -> a -> m (c, Diff a))
    -> Opto m v r a
fromStateless update = MkOpto
    { oInit   = ()
    , oUpdate = \(~()) -> update
    }

pureGrad
    :: Applicative m
    => (r -> a -> Diff a)
    -> Grad m r a
pureGrad f r = pure . f r

nonSampling
    :: (a -> m (Diff a))
    -> Grad m r a
nonSampling f _ = f

pureNonSampling
    :: Applicative m
    => (a -> Diff a)
    -> Grad m r a
pureNonSampling f _ = pure . f
