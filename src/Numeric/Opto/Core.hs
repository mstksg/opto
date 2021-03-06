{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
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
  -- * Optimizer type
    Diff, Grad, Opto(..)
  , mapSample
  , fromCopying, fromStateless
  -- * Making Grads
  , pureGrad
  , nonSampling, pureNonSampling
  ) where

import           Data.Kind
import           Data.Mutable
import           Numeric.Opto.Update

-- | Useful type synonym to indicate /differences/ in @a@ and rates of
-- change in type signatures.
type Diff     a = a

-- | Gradient function to compute a direction of steepest /ascent/ in @a@,
-- with respect to an @r@ sample.
type Grad q r a = forall m. (PrimMonad m, PrimState m ~ q) => r -> a -> m (Diff a)

-- | An @'Opto' m r a@ represents a (potentially stateful) in-place
-- optimizer for values of type @a@ that can be run in a monad @m@.  Each
-- optimization step requires an additional external "sample" @r@.
--
-- Usually these should be defined to be polymorphic on @m@, so that it can
-- be run in many different contexts in "Numeric.Opto.Run".
--
-- An @'Opto' m v () a@ is a "non-sampling" optimizer, where each
-- optimization step doesn't require any external input.
data Opto :: Type -> Type -> Type -> Type where
    MkOpto :: forall s q r a c. (LinearInPlace q c a, Mutable q s)
           => { oInit   :: !s
              , oUpdate :: !( forall m. (PrimMonad m, PrimState m ~ q)
                           => Ref q s
                           -> r
                           -> a
                           -> m (c, Diff a)
                            )
              }
           -> Opto q r a

-- -- | Map over the inner monad of an 'Opto' by providing a natural
-- -- transformation, and also a method to "convert" the references.
-- mapOpto = undefined
--     -- :: forall m n r a c q. (LinearInPlace q c a, PrimState m ~ q, PrimState n ~ q)
--     -- => (forall x. m x -> n x)
--     -- -> Opto q r a
--     -- -> Opto q r a
-- -- mapOpto f (MkOpto o (u :: Ref q s -> r -> b -> m (d, b))) = undefined
--     -- reMutable @m @n @s f $ case linearWit @a @c @d of
--     --   Refl -> MkOpto @s @n @r @a @d o $ \r i x -> f (u (g r) i x)

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
    :: (LinearInPlace q c a, Mutable q s)
    => s                                    -- ^ Initial state
    -> (forall m. (PrimMonad m, PrimState m ~ q) => r -> a -> s -> m (c, Diff a, s))    -- ^ State-updating function
    -> Opto q r a
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
    :: LinearInPlace q c a
    => (forall m. (PrimMonad m, PrimState m ~ q) => r -> a -> m (c, Diff a))
    -> Opto q r a
fromStateless update = MkOpto
    { oInit   = ()
    , oUpdate = \_ -> update
    }

-- | Create a bona-fide 'Grad' from a pure (non-monadic) sampling gradient function.
pureGrad
    :: (r -> a -> Diff a)
    -> Grad q r a
pureGrad f r = pure . f r

-- | Create a 'Grad' from a monadic non-sampling gradient function, which
-- ignores the external sample input @r@.
nonSampling
    :: (forall m. (PrimMonad m, PrimState m ~ q) => a -> m (Diff a))
    -> Grad q r a
nonSampling f _ = f

-- | Create a 'Grad' from a pure (non-monadic) non-sampling gradient
-- function, which ignores the external sample input @r@.
pureNonSampling
    :: (a -> Diff a)
    -> Grad q r a
pureNonSampling f _ = pure . f
