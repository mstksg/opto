{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Numeric.Opto.Stochastic (
    Sampling(..)
  , runSampling, sampling
  , StochM, Stoch
  , fromCopying
  , fromPure
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Foldable
import           Numeric.Opto.Core          (OptoM(..), Step)
import           Numeric.Opto.Ref
import           Numeric.Opto.Step
import qualified Numeric.Opto.Core          as OC

newtype Sampling r m a = Sampling { samplingReader :: ReaderT r m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , PrimMonad
             )

-- instance Ref m a v => Ref (Sampling r m) a v where
--     newRef = lift
-- instance {-# OVERLAPS #-} AdditiveInPlace m v a => AdditiveInPlace (Sampling r m) v a
-- instance {-# OVERLAPS #-} ScalingInPlace m v c a => ScalingInPlace (Sampling r m) v c a

runSampling :: Sampling r m a -> r -> m a
runSampling = runReaderT . samplingReader

sampling :: (r -> m a) -> Sampling r m a
sampling = Sampling . ReaderT


type StochM m r = OptoM (Sampling r m)
type Stoch  s r = OptoM (Sampling r (ST s))

fromCopying
    :: (PrimMonad m, ScalingInPlace (Sampling r m) v c a)
    => s
    -> (r -> a -> s -> m (c, Step a, s))
    -> StochM m r v a
fromCopying s0 update = OC.fromCopying s0 $ \x s -> sampling $ \r ->
                          update r x s

fromPure
    :: (PrimMonad m, ScalingInPlace (Sampling r m) v c a)
    => s
    -> (r -> a -> s -> (c, Step a, s))
    -> StochM m r v a
fromPure s0 update = fromCopying s0 (\r x -> pure . update r x)

-- scanOptoUntilM
--     :: forall m v r a t. (Monad m, Foldable t)
--     => t r
--     -> (Step a -> a -> m Bool)       -- ^ step, current
--     -> a
--     -> StochM m r v a
--     -> m (a, StochM m r v a)
-- scanOptoUntilM rs stop x0 MkOptoM{..} = do
--     rS <- runSampling (newRef oInit) undefined
--     rX <- runSampling (newRef @_ @a @v x0) undefined
--     _ <- runMaybeT . forM_ rs $ \r -> do
--       (y, step) <- lift $ do
--         y      <- runSampling (readRef rX) undefined
--         (c, g) <- runSampling (oUpdate rS =<< readRef rX) r
--         rX .*+= (c, g)
--         return (y, c .* g)
--       guard . not =<< lift (stop step y)
--     s <- runSampling (readRef rS) undefined
--     let o' = MkOptoM s oUpdate
--     (, o') <$> runSampling (readRef rX) undefined

-- scanOptoUntil
--     :: Foldable t
--     => t r
--     -> (Step a -> a -> Bool)         -- ^ step, current
--     -> a
--     -> (forall s'. Opto s' v r a)
--     -> (a, Opto s v r a)
-- scanOptoUntil xs stop y0 o0 = runST $ do
--     (y', o') <- scanOptoUntilM xs (\s -> pure . stop s) y0 o0
--     return (y', unsafeCoerce o')        -- is this safe?  probably.

-- scanOptoM
--     :: (Monad m, Foldable t)
--     => t r
--     -> a
--     -> OptoM m v r a
--     -> m (a, OptoM m v r a)
-- scanOptoM xs = scanOptoUntilM xs (\_ _ -> pure False)

-- scanOpto
--     :: Foldable t
--     => t r
--     -> a
--     -> (forall s'. Opto s' v r a)
--     -> (a, Opto s v r a)
-- scanOpto xs = scanOptoUntil xs (\_ _ -> False)

-- iterateOptoM
--     :: Monad m
--     => (Step a -> a -> m Bool)   -- ^ step, current
--     -> a
--     -> OptoM m v () a
--     -> m (a, OptoM m v () a)
-- iterateOptoM = scanOptoUntilM (repeat ())

-- iterateOpto
--     :: (Step a -> a -> Bool)   -- ^ step, current
--     -> a
--     -> (forall s'. Opto s' v () a)
--     -> (a, Opto s v () a)
-- iterateOpto = scanOptoUntil (repeat ())

-- sgdOptimizerM
--     :: (Ref m a v, ScalingInPlace m v c a, Applicative m)
--     => c
--     -> (r -> a -> m a)          -- ^ gradient
--     -> OptoM m v r a
-- sgdOptimizerM lr gr =
--     MkOptoM { oInit   = EmptyRef
--             , oUpdate = \EmptyRef r x -> ((-lr),) <$> gr r x
--             }

-- sgdOptimizer
--     :: (Ref m a v, ScalingInPlace m v c a, Applicative m)
--     => c
--     -> (r -> a -> a)
--     -> OptoM m v r a
-- sgdOptimizer lr gr = sgdOptimizerM lr (\x -> pure . gr x)

-- gdOptimizerM
--     :: (ScalingInPlace m v c a, Applicative m)
--     => c
--     -> (a -> m a)           -- ^ gradient
--     -> OptoM m v () a
-- gdOptimizerM lr upd = sgdOptimizerM lr (const upd)

-- gdOptimizer
--     :: (ScalingInPlace m v c a, Applicative m)
--     => c
--     -> (a -> a)             -- ^ gradient
--     -> OptoM m v () a
-- gdOptimizer r gr = sgdOptimizer r (const gr)

-- data Adam c = Adam
--     { adamStep    :: !c
--     , adamDecay1  :: !c
--     , adamDecay2  :: !c
--     , adamEpsilon :: !c
--     }
--   deriving (Show, Eq)

-- instance Fractional a => Default (Adam a) where
--     def = Adam { adamStep    = 0.001
--                , adamDecay1  = 0.9
--                , adamDecay2  = 0.999
--                , adamEpsilon = 1e-8
--                }

-- adamOptimizerM
--     :: forall m v r a c. (RealFloat c, Floating a, ScalingInPlace m v c a, PrimMonad m)
--     => Adam c
--     -> (r -> a -> m a)          -- ^ gradient
--     -> OptoM m v r a
-- adamOptimizerM Adam{..} gr =
--     MkOptoM { oInit   = (scaleOne @_ @a, addZero, addZero)
--             , oUpdate = \rS r x -> do
--                 g <- gr r x
--                 (t, m0, v0) <- readMutVar rS
--                 let m1 = adamDecay1 .* m0 .+. (1 - adamDecay1) .* g
--                     v1 = adamDecay2 .* v0 .+. (1 - adamDecay2) .* g
--                     mHat = recip (1 - adamDecay1 ** t) .* m1
--                     vHat = recip (1 - adamDecay2 ** t) .* v1
--                 writeMutVar rS (t + 1, m1, v1)
--                 return ( -adamStep
--                        , mHat / (sqrt vHat + realToFrac adamEpsilon)
--                        )
--             }

-- adamOptimizer
--     :: (RealFloat c, Floating a, ScalingInPlace m v c a, PrimMonad m)
--     => Adam c
--     -> (r -> a -> a)          -- ^ gradient
--     -> OptoM m v r a
-- adamOptimizer a gr = adamOptimizerM a (\x -> pure . gr x)
