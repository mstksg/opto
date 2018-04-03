{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Numeric.Opto.Backprop (
    bpGrad
  , bpGradSample
  ) where

import           Control.Monad.Sample
import           Numeric.Backprop
import           Numeric.Opto.Core

bpGrad
    :: (Monad m, Num a, Num b)
    => (forall s. Reifies s W => BVar s a -> BVar s b)
    -> Grad m a
bpGrad f = pureGrad $ gradBP f

bpGradSample
    :: (MonadSample r m, Num a, Num b)
    => (forall s. Reifies s W => BVar s r -> BVar s a -> BVar s b)
    -> Grad m a
bpGradSample f = pureSampling $ \r -> gradBP (f (constVar r))
