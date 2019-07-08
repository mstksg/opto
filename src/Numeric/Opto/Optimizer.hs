{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- http://ruder.io/optimizing-gradient-descent/

module Numeric.Opto.Optimizer (
    -- steepestDescent
  -- , Momentum(..), momentum
  -- , Nesterov(..), nesterov
  -- , Adam(..), adam
  -- , AdaMax(..), adaMax
  ) where

import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Primitive.MutVar
import           Data.Type.ZipProd
import           Data.Vinyl.Core
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

-- steepestDescent
--     :: forall m v a c. (ScalingInPlace m v c a, Applicative m)
--     => c                        -- ^ learning rate
--     -> Grad m a                 -- ^ gradient
--     -> OptoM m v a
-- steepestDescent lr gr = fromStateless $ fmap (-lr,) . gr

-- newtype Momentum c = Momentum
--     { momentumDecay :: c
--     }
--   deriving (Show, Eq)

-- instance Fractional c => Default (Momentum c) where
--     def = Momentum { momentumDecay = 0.9 }

-- momentum
--     :: forall m v a c. (PrimMonad m, ScalingInPlace m v c a)
--     => Momentum c       -- ^ configuration
--     -> c                -- ^ learning rate
--     -> Grad m a         -- ^ gradient
--     -> OptoM m v a
-- momentum Momentum{..} lr gr = fromCopying (addZero @a) $ \x v -> do
--     g <- gr x
--     let v' = (momentumDecay .* v) .+. (lr .* g)
--     pure (-1, v', v')

-- newtype Nesterov c = Nesterov
--     { nesterovDecay :: c
--     }
--   deriving (Show, Eq)

-- instance Fractional c => Default (Nesterov c) where
--     def = Nesterov { nesterovDecay = 0.9 }

-- nesterov
--     :: forall m v a c. (PrimMonad m, ScalingInPlace m v c a)
--     => Nesterov c       -- ^ configuration
--     -> c                -- ^ learning rate
--     -> Grad m a         -- ^ gradient
--     -> OptoM m v a
-- nesterov Nesterov{..} lr gr = fromCopying (addZero @a) $ \x v -> do
--     let vDecay = nesterovDecay .* v
--     g <- gr (x .+. ((-1) .* vDecay))
--     let v' = vDecay .+. (lr .* g)
--     pure (-1, v', v')

-- data Adam c = Adam
--     { adamStep    :: !c
--     , adamDecay1  :: !c
--     , adamDecay2  :: !c
--     , adamEpsilon :: !c
--     }
--   deriving (Show, Eq)

-- instance Fractional c => Default (Adam c) where
--     def = Adam { adamStep    = 0.001
--                , adamDecay1  = 0.9
--                , adamDecay2  = 0.999
--                , adamEpsilon = 1e-8
--                }

-- adam
--     :: forall m v a c.
--      ( RealFloat c
--      , Floating a
--      , ScalingInPlace m v c a
--      , PrimMonad m
--      )
--     => Adam c               -- ^ configuration
--     -> Grad m a             -- ^ gradient
--     -> OptoM m v a
-- adam Adam{..} gr = MkOptoM
--     { oInit   = RVl 1 :<< RVl addZero :<< RVl addZero :<< ZPØ
--              :: ZipProd (RefVal m)
--                         '[c                     ,a,a]
--                         '[MutVar (PrimState m) c,v,v]
--     , oUpdate = \rSs x -> fmap fromJust . runMaybeT $ do
--         RVr rT :<< RVr rM :<< RVr rV :<< ZPØ <- return rSs
--         lift $ do
--           rM .*= adamDecay1
--           rV .*= adamDecay2
--           g <- gr x
--           rM .*+= (1 - adamDecay1, g)
--           rV .*+= (1 - adamDecay2, g * g)
--         Identity m :& Identity v :& RNil <- lift $ freezeRefs (tailZP rSs)
--         t <- lift $ updateRef' rT $ \t0 -> let t1 = t0 + 1
--                                            in  (t1, t1)
--         let mHat = recip (1 - adamDecay1 ** t) .* m
--             vHat = recip (1 - adamDecay2 ** t) .* v
--         return ( -adamStep
--                , mHat / (sqrt vHat + realToFrac adamEpsilon)
--                )
--     }

-- data AdaMax c = AdaMax
--     { adaMaxStep    :: !c
--     , adaMaxDecay1  :: !c
--     , adaMaxDecay2  :: !c
--     , adaMaxEpsilon :: !c
--     }
--   deriving (Show, Eq)

-- instance Fractional c => Default (AdaMax c) where
--     def = AdaMax { adaMaxStep    = 0.002
--                  , adaMaxDecay1  = 0.9
--                  , adaMaxDecay2  = 0.999
--                  , adaMaxEpsilon = 1e-8
--                  }

-- adaMax
--     :: forall m v a c.
--      ( RealFloat c
--      , Metric c a
--      , ScalingInPlace m v c a
--      , PrimMonad m
--      )
--     => AdaMax c             -- ^ configuration
--     -> Grad m a             -- ^ gradient
--     -> OptoM m v a
-- adaMax AdaMax{..} gr = MkOptoM
--     { oInit   = RVl 1 :<< RVl addZero :<< RVl 0 :<< ZPØ
--              :: ZipProd (RefVal m)
--                         '[c,a,c]
--                         '[MutVar (PrimState m) c, v, MutVar (PrimState m) c]
--     , oUpdate = \rSs x -> fmap fromJust . runMaybeT $ do
--         RVr rT :<< RVr rM :<< RVr rU :<< ZPØ <- return rSs
--         lift $ do
--           rM .*= adaMaxDecay1
--           g <- gr x
--           rM .*+= (1 - adaMaxDecay1, g)
--           t <- updateRef' rT $ \t0 ->
--               let t1 = t0 + 1
--               in  (t1, t1)
--           m <- freezeRef rM
--           u <- updateRef' rU $ \u0 ->
--               let u1 = max (adaMaxDecay2 * u0) (norm_inf g)
--               in  (u1, u1)
--           return ( -adaMaxStep / ((1 - adaMaxDecay1 ** t) * u)
--                  , m
--                  )
--     }
