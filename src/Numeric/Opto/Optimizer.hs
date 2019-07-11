{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- |
-- Module      : Numeric.Opto.Optimizer
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Defining various numeric optimizers.  Most of these implemtations are
-- taken directly from <http://ruder.io/optimizing-gradient-descent/>
module Numeric.Opto.Optimizer (
    steepestDescent
  , Momentum(..), momentum
  , Nesterov(..), nesterov
  , Adagrad(..), adagrad
  , Adadelta(..), adadelta
  , RMSProp(..), rmsProp
  , Adam(..), adam
  , AdaMax(..), adaMax
  ) where

import           Data.Default
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

-- | Steepest descent, acording to some learning rate.  The simplest
-- optimizer.
steepestDescent
    :: LinearInPlace m v c a
    => c                          -- ^ learning rate
    -> Grad m r a                 -- ^ gradient
    -> Opto m v r a
steepestDescent lr gr = fromStateless $ \r x -> do
    !g <- gr r x
    pure (-lr, g)

-- | Hyperparameter for 'momentum'
newtype Momentum c = Momentum
    { momentumDecay :: c
    }
  deriving (Show, Eq)

instance Fractional c => Default (Momentum c) where
    def = Momentum { momentumDecay = 0.9 }

-- | Steepest descent with momentum. (Qian, 1999)
momentum
    :: forall m v r a c. LinearInPlace m v c a
    => Momentum c        -- ^ configuration
    -> c                 -- ^ learning rate
    -> Grad m r a        -- ^ gradient
    -> Opto m v r a
momentum Momentum{..} lr gr = MkOpto
    { oInit   = zeroL :: a
    , oUpdate = \(rV :: v) r x -> do
        !g <- gr r x
        rV  .*= momentumDecay
        rV .*+= (lr, g)
        v  <- freezeRef rV
        pure ( -1, v )
    }

-- | Hyperparameter for 'nesterov'
newtype Nesterov c = Nesterov
    { nesterovDecay :: c
    }
  deriving (Show, Eq)

instance Fractional c => Default (Nesterov c) where
    def = Nesterov { nesterovDecay = 0.9 }

-- | Nesterov accelerated gradient (NAG) (Nesterov, 1983)
nesterov
    :: forall m v r a c. LinearInPlace m v c a
    => Nesterov c       -- ^ configuration
    -> c                -- ^ learning rate
    -> Grad m r a       -- ^ gradient
    -> Opto m v r a
nesterov Nesterov{..} lr gr = MkOpto
    { oInit   = zeroL :: a
    , oUpdate = \(rV :: v) r x -> do
        rV  .*= nesterovDecay
        !v <- freezeRef rV
        !g <- gr r (x .+. ((-1) .* v))
        rV .*+= (lr, g)
        !w <- freezeRef rV
        pure ( -1, w )
    }

-- | Hyperparameters for 'adagrad'
data Adagrad c = Adagrad
    { adagradRate :: c
    , adagradEps  :: c
    }
  deriving (Show, Eq)

instance Fractional c => Default (Adagrad c) where
    def = Adagrad
      { adagradRate = 0.01
      , adagradEps  = 1e-8
      }

-- | Adaptive Gradient (Duchu, Hazan, Singer, 2011).  Note that if the
-- state is not reset periodically, updates tend to zero fairly quickly.
adagrad
    :: forall m v r a c.
     ( LinearInPlace m v c a
     , Floating a
     , Real c
     )
    => Adagrad c
    -> Grad m r a
    -> Opto m v r a
adagrad Adagrad{..} gr = MkOpto
    { oInit   = zeroL :: a
    , oUpdate = \(rBigG :: v) r x -> do
        !g <- gr r x
        rBigG .+.= (g ** 2)
        !bigG <- freezeRef rBigG
        pure ( - adagradRate
             , g / sqrt (bigG + eps)
             )
    }
  where
    eps = realToFrac adagradEps

-- | Hyperparameters for 'adadelta'
data Adadelta c = Adadelta
    { adadeltaDecay :: c
    , adadeltaEps   :: c
    }
  deriving (Show, Eq)

instance Fractional c => Default (Adadelta c) where
    def = Adadelta
      { adadeltaDecay = 0.9
      , adadeltaEps   = 1e-8
      }

-- | The Adadelta extension of Adagrad (Zeiler, 2012) that mitigates the
-- decreasing learning rate.
adadelta
    :: forall m v r a c.
     ( LinearInPlace m v c a
     , Floating a
     , Real c
     )
    => Adadelta c
    -> Grad m r a
    -> Opto m v r a
adadelta Adadelta{..} gr = MkOpto
    { oInit   = (zeroL, zeroL) :: (a, a)
    , oUpdate = \(rDeltHist :: v, rGradHist :: v) r x -> do
        !g        <- gr r x
        !deltHist <- freezeRef rDeltHist

        rGradHist  .*= adadeltaDecay
        rGradHist .*+= (complDecay, g ** 2)
        !gradHist <- freezeRef rGradHist

        let negaDelt = (sqrt (deltHist + eps) / sqrt (gradHist + eps)) * g
        rDeltHist  .*= adadeltaDecay
        rDeltHist .*+= (complDecay, negaDelt ** 2)

        pure ( -1, negaDelt )
    }
  where
    eps = realToFrac adadeltaEps
    complDecay = 1 - adadeltaDecay

-- | Hyperparameters for 'rmsProp'
data RMSProp c = RMSProp
    { rmsPropRate  :: c
    , rmsPropDecay :: c
    , rmsPropEps   :: c
    }
  deriving (Show, Eq)

instance Fractional c => Default (RMSProp c) where
    def = RMSProp
      { rmsPropRate  = 0.001
      , rmsPropDecay = 0.9
      , rmsPropEps   = 1e-8
      }

-- | RMSProp, as described by Geoff Hinton.
rmsProp
    :: forall m v r a c.
     ( LinearInPlace m v c a
     , Floating a
     , Real c
     )
    => RMSProp c
    -> Grad m r a
    -> Opto m v r a
rmsProp RMSProp{..} gr = MkOpto
    { oInit   = zeroL :: a
    , oUpdate = \(rGradHist :: v) r x -> do
        !g <- gr r x
        rGradHist  .*= rmsPropDecay
        rGradHist .*+= (complDecay, g ** 2)
        !gradHist <- freezeRef rGradHist
        pure ( - rmsPropRate
             , g / sqrt (gradHist + eps)
             )
    }
  where
    eps = realToFrac rmsPropEps
    complDecay = 1 - rmsPropDecay

-- | Hyperparameters for 'adam'
data Adam c = Adam
    { adamStep    :: !c
    , adamDecay1  :: !c
    , adamDecay2  :: !c
    , adamEps :: !c
    }
  deriving (Show, Eq)

instance Fractional c => Default (Adam c) where
    def = Adam { adamStep   = 0.001
               , adamDecay1 = 0.9
               , adamDecay2 = 0.999
               , adamEps    = 1e-8
               }

-- | Adaptive Moment Estimation (Kingma, Ba, 2015)
adam
    :: forall m v r a c vc.
     ( RealFloat c
     , Floating a
     , LinearInPlace m v c a
     , Ref m c vc
     )
    => Adam c               -- ^ configuration
    -> Grad m r a           -- ^ gradient
    -> Opto m v r a
adam Adam{..} gr = MkOpto
    { oInit   = (1, zeroL, zeroL) :: (c, a, a)
    , oUpdate = \( rT :: vc
                 , rM :: v
                 , rV :: v
                 ) r x -> do
        !g <- gr r x
        rM .*= adamDecay1
        rV .*= adamDecay2
        rM .*+= (1 - adamDecay1, g)
        rV .*+= (1 - adamDecay2, g * g)
        (!m, !v) <- freezeRef (rM, rV)
        !t       <- updateRef' rT $ \t0 -> let !t1 = t0 + 1
                                           in  (t1, t1)
        let !mHat = recip (1 - adamDecay1 ** t) .* m
            !vHat = recip (1 - adamDecay2 ** t) .* v
        return ( -adamStep
               , mHat / (sqrt vHat + realToFrac adamEps)
               )
    }

-- | Hyperparameters for 'adaMax'
data AdaMax c = AdaMax
    { adaMaxStep    :: !c
    , adaMaxDecay1  :: !c
    , adaMaxDecay2  :: !c
    , adaMaxEps :: !c
    }
  deriving (Show, Eq)

instance Fractional c => Default (AdaMax c) where
    def = AdaMax { adaMaxStep   = 0.002
                 , adaMaxDecay1 = 0.9
                 , adaMaxDecay2 = 0.999
                 , adaMaxEps    = 1e-8
                 }

-- | Adam variation (Kingma and Ba, 2015)
adaMax
    :: forall m v r a c vc.
     ( RealFloat c
     , Metric c a
     , LinearInPlace m v c a
     , Ref m c vc
     )
    => AdaMax c             -- ^ configuration
    -> Grad m r a           -- ^ gradient
    -> Opto m v r a
adaMax AdaMax{..} gr = MkOpto
    { oInit   = (1, zeroL, 0) :: (c, a, c)
    , oUpdate = \( rT :: vc
                 , rM :: v
                 , rU :: vc
                 ) r x -> do
        !g <- gr r x
        rM .*= adaMaxDecay1
        rM .*+= (1 - adaMaxDecay1, g)
        !t <- updateRef' rT $ \t0 ->
            let !t1 = t0 + 1
            in  (t1, t1)
        !m <- freezeRef rM
        !u <- updateRef' rU $ \u0 ->
            let !u1 = max (adaMaxDecay2 * u0) (norm_inf g)
            in  (u1, u1)
        return ( -adaMaxStep / ((1 - adaMaxDecay1 ** t) * u)
               , m
               )
    }

-- TODO: RMSProp, Adadelta, Nadam, AMSGrad
