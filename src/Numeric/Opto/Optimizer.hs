{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- http://ruder.io/optimizing-gradient-descent/

module Numeric.Opto.Optimizer (
    steepestDescent
  , Momentum(..), momentum
  , Nesterov(..), nesterov
  , Adam(..), adam
  , AdaMax(..), adaMax
  ) where

import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Maybe
import           Data.Primitive.MutVar
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

steepestDescent
    :: ScalingInPlace m v c a
    => c                          -- ^ learning rate
    -> Grad m r a                 -- ^ gradient
    -> Opto m v r a
steepestDescent lr gr = fromStateless $ \r -> fmap (-lr,) . gr r

newtype Momentum c = Momentum
    { momentumDecay :: c
    }
  deriving (Show, Eq)

instance Fractional c => Default (Momentum c) where
    def = Momentum { momentumDecay = 0.9 }

momentum
    :: forall m v r a c. (PrimMonad m, ScalingInPlace m v c a)
    => Momentum c        -- ^ configuration
    -> c                 -- ^ learning rate
    -> Grad m r a        -- ^ gradient
    -> Opto m v r a
momentum Momentum{..} lr gr = fromCopying (addZero @a) $ \r x v -> do
    g <- gr r x
    let v' = (momentumDecay .* v) .+. (lr .* g)
    pure (-1, v', v')

newtype Nesterov c = Nesterov
    { nesterovDecay :: c
    }
  deriving (Show, Eq)

instance Fractional c => Default (Nesterov c) where
    def = Nesterov { nesterovDecay = 0.9 }

nesterov
    :: forall m v r a c. (PrimMonad m, ScalingInPlace m v c a)
    => Nesterov c       -- ^ configuration
    -> c                -- ^ learning rate
    -> Grad m r a       -- ^ gradient
    -> Opto m v r a
nesterov Nesterov{..} lr gr = fromCopying (addZero @a) $ \r x v -> do
    let vDecay = nesterovDecay .* v
    g <- gr r (x .+. ((-1) .* vDecay))
    let v' = vDecay .+. (lr .* g)
    pure (-1, v', v')

data Adam c = Adam
    { adamStep    :: !c
    , adamDecay1  :: !c
    , adamDecay2  :: !c
    , adamEpsilon :: !c
    }
  deriving (Show, Eq)

instance Fractional c => Default (Adam c) where
    def = Adam { adamStep    = 0.001
               , adamDecay1  = 0.9
               , adamDecay2  = 0.999
               , adamEpsilon = 1e-8
               }

adam
    :: forall m v r a c.
     ( RealFloat c
     , Floating a
     , ScalingInPlace m v c a
     , PrimMonad m
     )
    => Adam c               -- ^ configuration
    -> Grad m r a           -- ^ gradient
    -> Opto m v r a
adam Adam{..} gr = MkOpto
    { oInit   = (1, addZero, addZero) :: (c, a, a)
    , oUpdate = \( rT :: MutVar (PrimState m) c
                 , rM :: v
                 , rV :: v
                 ) r x -> fmap fromJust . runMaybeT $ do
        lift $ do
          rM .*= adamDecay1
          rV .*= adamDecay2
          g <- gr r x
          rM .*+= (1 - adamDecay1, g)
          rV .*+= (1 - adamDecay2, g * g)
        (m, v) <- lift $ freezeRef (rM, rV)
        t <- lift $ updateRef' rT $ \t0 -> let t1 = t0 + 1
                                           in  (t1, t1)
        let mHat = recip (1 - adamDecay1 ** t) .* m
            vHat = recip (1 - adamDecay2 ** t) .* v
        return ( -adamStep
               , mHat / (sqrt vHat + realToFrac adamEpsilon)
               )
    }

data AdaMax c = AdaMax
    { adaMaxStep    :: !c
    , adaMaxDecay1  :: !c
    , adaMaxDecay2  :: !c
    , adaMaxEpsilon :: !c
    }
  deriving (Show, Eq)

instance Fractional c => Default (AdaMax c) where
    def = AdaMax { adaMaxStep    = 0.002
                 , adaMaxDecay1  = 0.9
                 , adaMaxDecay2  = 0.999
                 , adaMaxEpsilon = 1e-8
                 }

adaMax
    :: forall m v r a c.
     ( RealFloat c
     , Metric c a
     , ScalingInPlace m v c a
     , PrimMonad m
     )
    => AdaMax c             -- ^ configuration
    -> Grad m r a           -- ^ gradient
    -> Opto m v r a
adaMax AdaMax{..} gr = MkOpto
    { oInit   = (1, addZero, 0) :: (c, a, c)
    , oUpdate = \( rT :: MutVar (PrimState m) c
                 , rM :: v
                 , rU :: MutVar (PrimState m) c
                 ) r x -> fmap fromJust . runMaybeT $ do
        lift $ do
          rM .*= adaMaxDecay1
          g <- gr r x
          rM .*+= (1 - adaMaxDecay1, g)
          t <- updateRef' rT $ \t0 ->
              let t1 = t0 + 1
              in  (t1, t1)
          m <- freezeRef rM
          u <- updateRef' rU $ \u0 ->
              let u1 = max (adaMaxDecay2 * u0) (norm_inf g)
              in  (u1, u1)
          return ( -adaMaxStep / ((1 - adaMaxDecay1 ** t) * u)
                 , m
                 )
    }
