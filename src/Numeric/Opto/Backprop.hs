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

import           Control.Monad.Sample
import           Numeric.Backprop
import           Numeric.Opto.Core

-- | Turn a simple @a -> b@ function into a @'Grad' m a@.
bpGrad
    :: (Monad m, Backprop a, Backprop b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> Grad m a
bpGrad f = pureGrad $ gradBP f

-- | Turn a @a -> b@ function parameterized on @r@ into a @'Grad' m a@.
bpGradSample
    :: (MonadSample r m, Backprop a, Backprop b)
    => (forall s. Reifies s W => BVar s r -> BVar s a -> BVar s b)
    -> Grad m a
bpGradSample f = pureSampling $ \r -> gradBP (f (constVar r))
