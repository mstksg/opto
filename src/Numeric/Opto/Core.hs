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

-- hoistOpto
--     :: forall m n v r a. (forall c. ScalingInPlace m v c a => ScalingInPlace n v c a)
--     => (forall s u c. (Ref m s u, Ref n s u) => m (c, a) -> n (c, a))
--     -> Opto m v r a
--     -> Opto n v r a
-- hoistOpto f (MkOpto (i :: s) (p :: u -> r -> a -> m (c, Diff a)))
--     = MkOpto @s @u @n @v @r @a @c i (\u r -> f @s @u @c . p u r)

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (r -> a -> s -> m (c, Diff a, s))
    -> Opto m v r a
fromCopying s0 update =
    MkOpto { oInit    = s0
            , oUpdate = \rS r x -> do
                (c, g, s) <- update r x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromStateless
    :: (ScalingInPlace m v c a)
    => (r -> a -> m (c, Diff a))
    -> Opto m v r a
fromStateless update =
    MkOpto { oInit   = ()
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
