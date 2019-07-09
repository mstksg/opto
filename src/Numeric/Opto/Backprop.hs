{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- |
-- Module      : Numeric.Opto.Backprop
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Generate gradients usable with "Numeric.Opto" using the /backprop/
-- library.
module Numeric.Opto.Backprop (
    bpGrad
  , bpGradSample
  ) where

import           Numeric.Backprop
import           Numeric.Opto.Core

-- | Turn a simple @a -> b@ function into a @'Grad' m a@.
bpGrad
    :: (Monad m, Backprop a, Backprop b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> Grad m r a
bpGrad f = pureNonSampling $ gradBP f

-- | Turn a @a -> b@ function parameterized on @r@ into a @'Grad' m a@.
bpGradSample
    :: (Backprop a, Backprop b, Applicative m)
    => (forall s. Reifies s W => r -> BVar s a -> BVar s b)
    -> Grad m r a
bpGradSample f = pureGrad $ \r -> gradBP (f r)
