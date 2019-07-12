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

-- |
-- Module      : Numeric.Opto.Core
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Core functionality for optimizers.
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

-- | Useful type synonym to indicate /differences/ in @a@ and rates of
-- change in type signatures.
type Diff     a = a

-- | Gradient function to compute a direction of steepest /ascent/ in @a@,
-- with respect to an @r@ sample.
type Grad m r a = r -> a -> m (Diff a)

-- | An @'Opto' m r a@ represents a (potentially stateful) in-place
-- optimizer for values of type @a@ that can be run in a monad @m@.  Each
-- optimization step requires an additional external "sample" @r@.
--
-- Usually these should be defined to be polymorphic on @m@, so that it can
-- be run in many different contexts in "Numeric.Opto.Run".
--
-- An @'Opto' m v () a@ is a "non-sampling" optimizer, where each
-- optimization step doesn't require any external input.
data Opto :: (Type -> Type) -> Type -> Type -> Type where
    MkOpto :: forall s m r a c. (LinearInPlace m c a, Mutable m s)
           => { oInit   :: !s
              , oUpdate :: !( Ref m s
                           -> r
                           -> a
                           -> m (c, Diff a)
                            )
              }
           -> Opto m r a

-- | (Contravariantly) map over the type of the external sample input of an
-- 'Opto'.
mapSample
    :: (r -> s)
    -> Opto m s a
    -> Opto m r a
mapSample f MkOpto{..} = MkOpto
    { oInit   = oInit
    , oUpdate = \u r -> oUpdate u (f r)
    }

-- | Create an 'Opto' based on a (monadic) state-updating function, given
-- an initial state and the state updating function.  The function takes
-- the external @r@ input, the current value @a@, the current state @s@,
-- and returns a step to move @a@ in, a factor to scale that step via, and
-- an updated state.
--
-- The state is updated in a "copying" manner (by generating new values
-- purely), without any in-place mutation.
fromCopying
    :: (PrimMonad m, LinearInPlace m c a, Mutable m s)
    => s                                    -- ^ Initial state
    -> (r -> a -> s -> m (c, Diff a, s))    -- ^ State-updating function
    -> Opto m r a
fromCopying s0 update = MkOpto
    { oInit    = s0
    , oUpdate = \rS r x -> do
        (c, g, s) <- update r x =<< freezeRef rS
        copyRef rS s
        return (c, g)
    }

-- | Create a statless 'Opto' based on a (monadic) optimizing function.
-- The function takes the external @r@ input and the current value @a@ and
-- returns a step to move @a@ in and a factor to scale that step via.
fromStateless
    :: (LinearInPlace m c a)
    => (r -> a -> m (c, Diff a))
    -> Opto m r a
fromStateless update = MkOpto
    { oInit   = ()
    , oUpdate = \(~()) -> update
    }

-- | Create a bona-fide 'Grad' from a pure (non-monadic) sampling gradient function.
pureGrad
    :: Applicative m
    => (r -> a -> Diff a)
    -> Grad m r a
pureGrad f r = pure . f r

-- | Create a 'Grad' from a monadic non-sampling gradient function, which
-- ignores the external sample input @r@.
nonSampling
    :: (a -> m (Diff a))
    -> Grad m r a
nonSampling f _ = f

-- | Create a 'Grad' from a pure (non-monadic) non-sampling gradient
-- function, which ignores the external sample input @r@.
pureNonSampling
    :: Applicative m
    => (a -> Diff a)
    -> Grad m r a
pureNonSampling f _ = pure . f
