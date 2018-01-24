{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Opto where

-- module Numeric.Opto (
--     Ref(..)
--   , OptoM(..), Opto
--   , fromCopying, fromPure
--   , scanOptoM, scanOpto
--   , scanOptoUntilM, scanOptoUntil
--   , iterateOptoM, iterateOpto
--   , sgdOptimizerM, sgdOptimizer
--   , gdOptimizerM, gdOptimizer
--   , Adam(..), adamOptimizerM, adamOptimizer
--   ) where

-- import           Data.Foldable
-- import           Data.Primitive
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Proxy
import           Unsafe.Coerce
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as MV
import qualified Data.Vector.Sized         as SV

class Ref m a v | v -> a where
    newRef     :: a -> m v
    readRef    :: v -> m a
    writeRef   :: v -> a -> m ()
    modifyRef  :: v -> (a -> a) -> m ()
    modifyRef' :: v -> (a -> a) -> m ()

class Additive a where
    (.+.)   :: a -> a -> a
    addZero :: a

    default (.+.) :: Num a => a -> a -> a
    (.+.) = (+)
    default addZero :: Num a => a
    addZero = 0

class (Num c, Additive a) => Scaling c a | a -> c where
    (.*)     :: c -> a -> a
    scaleOne :: c

    default (.*) :: (Num a, c ~ a) => c -> a -> a
    (.*) = (*)
    default scaleOne :: Num c => c
    scaleOne = 1

class (Ref m a v, Additive a) => AdditiveInPlace m v a where
    (.+.=) :: v -> a -> m ()
    r .+.= x = modifyRef' r (.+. x)

class (AdditiveInPlace m v a, Scaling c a) => ScalingInPlace m v c a where
    (.*=)  :: v -> c -> m ()
    r .*= c = modifyRef' r (c .*)
    (.*+=) :: v -> (c, a) -> m ()
    r .*+= (c, x) = modifyRef' r ((c .* x) .+.)

instance (PrimMonad m, PrimState m ~ s) => Ref m a (MutVar s a) where
    newRef     = newMutVar
    readRef    = readMutVar
    writeRef   = writeMutVar
    modifyRef  = modifyMutVar
    modifyRef' = modifyMutVar'

instance (PrimMonad m, PrimState m ~ s) => Ref m (V.Vector a) (MV.MVector s a) where
    newRef    = V.thaw
    readRef   = V.freeze
    writeRef  = V.copy
    modifyRef r f = do
      V.copy r . f =<< V.freeze r
    modifyRef' r f = do
      v <- f <$> V.freeze r
      v `seq` V.copy r v

data EmptyRef = EmptyRef

instance Applicative m => Ref m EmptyRef EmptyRef where
    newRef     = \_ -> pure EmptyRef
    readRef    = \_ -> pure EmptyRef
    writeRef   = \_ _ -> pure ()
    modifyRef  = \_ _ -> pure ()
    modifyRef' = \_ _ -> pure ()

instance Additive Double
instance Scaling Double Double
instance Ref m Double v => AdditiveInPlace m v Double
instance Ref m Double v => ScalingInPlace m v Double Double

instance Num a => Additive (V.Vector a) where
    (.+.) = V.zipWith (+)
    addZero = undefined

instance Num a => Scaling a (V.Vector a) where
    c .* xs  = (c *) <$> xs
    scaleOne = 1

instance (PrimMonad m, PrimState m ~ s, Num a) => AdditiveInPlace m (MV.MVector s a) (V.Vector a) where
    r .+.= xs = flip V.imapM_ xs $ \i x ->
      MV.modify r (+ x) i

instance (PrimMonad m, PrimState m ~ s, Num a) => ScalingInPlace m (MV.MVector s a) a (V.Vector a) where
    r .*= c = forM_ [0 .. MV.length r - 1] $ \i ->
      MV.modify r (c *) i

data OptoM :: (Type -> Type) -> Type -> Type -> Type -> Type where
    MkOptoM :: (Ref m s sVar, ScalingInPlace m v c a)
            => { oInit   :: !s
               , oUpdate :: !(sVar -> r -> a -> m (c, a))
               }
            -> OptoM m v r a

type Opto s = OptoM (ST s)

scanOptoUntilM
    :: forall m v r a t. (Monad m, Foldable t)
    => t r
    -> (a -> a -> m Bool)       -- ^ grad, current
    -> a
    -> OptoM m v r a
    -> m (a, OptoM m v r a)
scanOptoUntilM rs stop x0 MkOptoM{..} = do
    rS <- newRef oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT . forM_ rs $ \r -> do
      (y, g) <- lift $ do
        y      <- readRef rX
        (c, g) <- oUpdate rS r =<< readRef rX
        rX .*+= (c, g)
        return (y, g)
      guard . not =<< lift (stop g y)
    s' <- readRef rS
    let o' = MkOptoM s' oUpdate
    (, o') <$> readRef rX

sgdOptimizerM
    :: (Ref m a v, ScalingInPlace m v c a, Applicative m)
    => c
    -> (r -> a -> m a)          -- ^ gradient
    -> OptoM m v r a
sgdOptimizerM lr gr =
    MkOptoM { oInit   = EmptyRef
            , oUpdate = \EmptyRef r x -> ((-lr),) <$> gr r x
            }

sgdOptimizer
    :: (Ref m a v, ScalingInPlace m v c a, Applicative m)
    => c
    -> (r -> a -> a)
    -> OptoM m v r a
sgdOptimizer lr gr = sgdOptimizerM lr (\x -> pure . gr x)

-- fromCopying
--     :: PrimMonad m
--     => s
--     -> (a -> b -> m b)
--     -> (b -> b -> s -> m (b, s))  -- ^ gradient, value, state
--     -> OptoM m a b
-- fromCopying s0 grad update =
--     MkOptoM { oSVar   = Proxy
--             , oInit   = s0
--             , oGrad   = grad
--             , oUpdate = \rS rY g -> do
--                 y <- readMutVar rY
--                 s <- readMutVar rS
--                 (y', s') <- update g y s
--                 writeMutVar rS s'
--                 writeMutVar rY y'
--             }

-- fromPure
--     :: PrimMonad m
--     => s
--     -> (a -> b -> b)
--     -> (b -> b -> s -> (b, s))  -- ^ gradient, value, state
--     -> OptoM m a b
-- fromPure s0 grad update = fromCopying s0 (\x   -> return . grad x)
--                                          (\g y -> return . update g y)

-- scanOptoM
--     :: (Monad m, Foldable t)
--     => t a
--     -> b
--     -> OptoM m a b
--     -> m (b, OptoM m a b)
-- scanOptoM xs = scanOptoUntilM xs (\_ _ -> pure False)

-- scanOpto
--     :: Foldable t
--     => t a
--     -> b
--     -> (forall s'. Opto s' a b)
--     -> (b, Opto s a b)
-- scanOpto xs = scanOptoUntil xs (\_ _ -> False)

-- scanOptoUntilM
--     :: (Monad m, Foldable t)
--     => t a
--     -> (b -> b -> m Bool)       -- ^ grad, current
--     -> b
--     -> OptoM m a b
--     -> m (b, OptoM m a b)
-- scanOptoUntilM xs stop y0 MkOptoM{..} = do
--     rS <- newRef oInit
--     rY <- newRef y0
--     _ <- runMaybeT . forM_ xs $ \x -> do
--       y <- lift $ readRef rY
--       g <- lift $ oGrad x =<< readRef rY
--       guard . not =<< lift (stop g y)
--       lift $ oUpdate rS rY g
--     s' <- readRef rS
--     let o' = MkOptoM Proxy s' oGrad oUpdate
--     (, o') <$> readRef rY

-- scanOptoUntil
--     :: Foldable t
--     => t a
--     -> (b -> b -> Bool)         -- ^ grad, current
--     -> b
--     -> (forall s'. Opto s' a b)
--     -> (b, Opto s a b)
-- scanOptoUntil xs stop y0 o0 = runST $ do
--     (y', o') <- scanOptoUntilM xs (\g -> pure . stop g) y0 o0
--     return (y', unsafeCoerce o')        -- is this safe?  probably.

-- iterateOptoM
--     :: Monad m
--     => (b -> b -> m Bool)   -- ^ grad, current
--     -> b
--     -> OptoM m () b
--     -> m (b, OptoM m () b)
-- iterateOptoM = scanOptoUntilM (repeat ())

-- iterateOpto
--     :: (b -> b -> Bool)   -- ^ grad, current
--     -> b
--     -> (forall s'. Opto s' () b)
--     -> (b, Opto s () b)
-- iterateOpto = scanOptoUntil (repeat ())

-- sgdOptimizerM
--     :: ()
--     => Double
--     -> (a -> b -> m b)          -- ^ gradient
--     -> (v -> b -> m ())         -- ^ adding action
--     -> OptoM m a b
-- sgdOptimizerM r gr upd =
--     MkOptoM { oSVar   = Proxy @()
--             , oInit   = ()
--             , oGrad   = gr
--             , oUpdate = \_ rY g -> upd rY (realToFrac (- r) * g)
--             }


-- sgdOptimizerM
--     :: (Fractional b, Ref m b v, Applicative m)
--     => Double
--     -> (a -> b -> m b)          -- ^ gradient
--     -> (v -> b -> m ())         -- ^ adding action
--     -> OptoM m a b
-- sgdOptimizerM r gr upd =
--     MkOptoM { oSVar   = Proxy @()
--             , oInit   = ()
--             , oGrad   = gr
--             , oUpdate = \_ rY g -> upd rY (realToFrac (- r) * g)
--             }

-- sgdOptimizer
--     :: (Fractional b, PrimMonad m)
--     => Double
--     -> (a -> b -> b)
--     -> OptoM m a b
-- sgdOptimizer r gr = sgdOptimizerM r (\x -> return . gr x) $ \rY u ->
--     modifyMutVar' rY (+ u)

-- gdOptimizerM
--     :: (Fractional b, Ref m b v, Applicative m)
--     => Double
--     -> (b -> b)             -- ^ gradient
--     -> (v -> b -> m ())     -- ^ adding action
--     -> OptoM m () b
-- gdOptimizerM r gr upd = sgdOptimizerM r (const (pure . gr)) upd

-- gdOptimizer
--     :: (Fractional b, PrimMonad m)
--     => Double
--     -> (b -> b)             -- ^ gradient
--     -> OptoM m () b
-- gdOptimizer r gr = sgdOptimizer r (const gr)

-- data Adam = Adam
--     { adamStep    :: Double
--     , adamDecay1  :: Double
--     , adamDecay2  :: Double
--     , adamEpsilon :: Double
--     }
--   deriving (Show, Eq)

-- instance Default Adam where
--     def = Adam { adamStep    = 0.001
--                , adamDecay1  = 0.9
--                , adamDecay2  = 0.999
--                , adamEpsilon = 1e-8
--                }

-- adamOptimizerM
--     :: forall a b v m. (Floating b, Ref m b v, PrimMonad m)
--     => Adam
--     -> (a -> b -> m b)          -- ^ gradient
--     -> (v -> b -> m ())         -- ^ adding
--     -> OptoM m a b
-- adamOptimizerM Adam{..} gr upd = 
--     MkOptoM { oSVar   = Proxy @(MutVar (PrimState m) (Double, b, b))
--             , oInit   = (1,0,0)
--             , oGrad   = gr
--             , oUpdate = \rS rB g -> do
--                 (t, m0, v0) <- readRef rS
--                 let m1 = realToFrac adamDecay1 * m0
--                        + realToFrac (1 - adamDecay1) * g
--                     v1 = realToFrac adamDecay2 * v0
--                        + realToFrac (1 - adamDecay2) * g
--                     mHat = m1 / (1 - realToFrac (adamDecay1 ** t))
--                     vHat = v1 / (1 - realToFrac (adamDecay2 ** t))
--                 writeRef rS (t + 1, m1, v1)
--                 upd rB $ realToFrac (- adamStep)
--                          * mHat
--                          / (sqrt vHat + realToFrac adamEpsilon)
--             }

-- adamOptimizer
--     :: (Floating b, PrimMonad m)
--     => Adam
--     -> (a -> b -> b)          -- ^ gradient
--     -> OptoM m a b
-- adamOptimizer a gr = adamOptimizerM a (\x -> return . gr x) $ \rY u ->
--     modifyMutVar' rY (+ u)
