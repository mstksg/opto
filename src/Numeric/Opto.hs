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

module Numeric.Opto (
    Ref(..)
  , Additive(..), Scaling(..)
  , AdditiveInPlace(..), ScalingInPlace(..)
  , OptoM(..), Opto
  , fromCopying, fromPure
  , scanOptoM, scanOpto
  , scanOptoUntilM, scanOptoUntil
  , iterateOptoM, iterateOpto
  , sgdOptimizerM, sgdOptimizer
  , gdOptimizerM, gdOptimizer
  , Adam(..), adamOptimizerM, adamOptimizer
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Kind
import           Data.Primitive.MutVar
import           Unsafe.Coerce
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as MV

class Ref m a v | v -> a where
    newRef     :: a -> m v
    readRef    :: v -> m a
    writeRef   :: v -> a -> m ()
    modifyRef  :: v -> (a -> a) -> m ()
    modifyRef' :: v -> (a -> a) -> m ()

class Additive a where
    infixl 6 .+.
    (.+.)   :: a -> a -> a
    addZero :: a

    default (.+.) :: Num a => a -> a -> a
    (.+.) = (+)
    default addZero :: Num a => a
    addZero = 0

class (Num c, Additive a) => Scaling c a | a -> c where
    infixl 7 .*
    (.*)     :: c -> a -> a
    scaleOne :: c

    default (.*) :: (Num a, c ~ a) => c -> a -> a
    (.*) = (*)
    default scaleOne :: Num c => c
    scaleOne = 1

-- class Additive a => Multiplicative a where
--     infixl 7 .*.
--     (.*.)     :: a -> a -> a
--     infixl 7 ./.
--     (./.)     :: a -> a -> a
--     multRecip :: a -> a
--     multOne   :: a
--     default (.*.) :: Num a => a -> a -> a
--     (.*.) = (*)
--     default (./.) :: Fractional a => a -> a -> a
--     (./.) = (/)
--     multRecip = (multOne ./.)
--     default multOne :: Num a => a
--     multOne = 1

class (Ref m a v, Additive a) => AdditiveInPlace m v a where
    infix 4 .+.=
    (.+.=) :: v -> a -> m ()
    r .+.= x = modifyRef' r (.+. x)

class (AdditiveInPlace m v a, Scaling c a) => ScalingInPlace m v c a where
    infix 4 .*=
    (.*=)  :: v -> c -> m ()
    r .*= c = modifyRef' r (c .*)
    infix 4 .*+=
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
-- instance Multiplicative Double
instance Ref m Double v => AdditiveInPlace m v Double
instance Ref m Double v => ScalingInPlace m v Double Double

instance Num a => Additive (V.Vector a) where
    (.+.) = V.zipWith (+)
    addZero = undefined

instance Num a => Scaling a (V.Vector a) where
    c .* xs  = (c *) <$> xs
    scaleOne = 1

-- instance Fractional a => Multiplicative (V.Vector a) where
--     (.*.) = V.zipWith (*)
--     (./.) = V.zipWith (/)
--     multRecip = fmap recip
--     multOne = undefined

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

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (r -> a -> s -> m (c, a, s))
    -> OptoM m v r a
fromCopying s0 update =
    MkOptoM { oInit   = s0
            , oUpdate = \rS r x -> do
                (c, g, s) <- update r x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromPure
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (r -> a -> s -> (c, a, s))
    -> OptoM m v r a
fromPure s0 update = fromCopying s0 (\r x -> pure . update r x)

scanOptoUntilM
    :: forall m v r a t. (Monad m, Foldable t)
    => t r
    -> (a -> a -> m Bool)       -- ^ step, current
    -> a
    -> OptoM m v r a
    -> m (a, OptoM m v r a)
scanOptoUntilM rs stop x0 MkOptoM{..} = do
    rS <- newRef oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT . forM_ rs $ \r -> do
      (y, step) <- lift $ do
        y      <- readRef rX
        (c, g) <- oUpdate rS r =<< readRef rX
        rX .*+= (c, g)
        return (y, c .* g)
      guard . not =<< lift (stop step y)
    s' <- readRef rS
    let o' = MkOptoM s' oUpdate
    (, o') <$> readRef rX

scanOptoUntil
    :: Foldable t
    => t r
    -> (a -> a -> Bool)         -- ^ step, current
    -> a
    -> (forall s'. Opto s' v r a)
    -> (a, Opto s v r a)
scanOptoUntil xs stop y0 o0 = runST $ do
    (y', o') <- scanOptoUntilM xs (\s -> pure . stop s) y0 o0
    return (y', unsafeCoerce o')        -- is this safe?  probably.

scanOptoM
    :: (Monad m, Foldable t)
    => t r
    -> a
    -> OptoM m v r a
    -> m (a, OptoM m v r a)
scanOptoM xs = scanOptoUntilM xs (\_ _ -> pure False)

scanOpto
    :: Foldable t
    => t r
    -> a
    -> (forall s'. Opto s' v r a)
    -> (a, Opto s v r a)
scanOpto xs = scanOptoUntil xs (\_ _ -> False)

iterateOptoM
    :: Monad m
    => (a -> a -> m Bool)   -- ^ step, current
    -> a
    -> OptoM m v () a
    -> m (a, OptoM m v () a)
iterateOptoM = scanOptoUntilM (repeat ())

iterateOpto
    :: (a -> a -> Bool)   -- ^ step, current
    -> a
    -> (forall s'. Opto s' v () a)
    -> (a, Opto s v () a)
iterateOpto = scanOptoUntil (repeat ())

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

gdOptimizerM
    :: (ScalingInPlace m v c a, Applicative m)
    => c
    -> (a -> m a)           -- ^ gradient
    -> OptoM m v () a
gdOptimizerM lr upd = sgdOptimizerM lr (const upd)

gdOptimizer
    :: (ScalingInPlace m v c a, Applicative m)
    => c
    -> (a -> a)             -- ^ gradient
    -> OptoM m v () a
gdOptimizer r gr = sgdOptimizer r (const gr)

data Adam c = Adam
    { adamStep    :: !c
    , adamDecay1  :: !c
    , adamDecay2  :: !c
    , adamEpsilon :: !c
    }
  deriving (Show, Eq)

instance Fractional a => Default (Adam a) where
    def = Adam { adamStep    = 0.001
               , adamDecay1  = 0.9
               , adamDecay2  = 0.999
               , adamEpsilon = 1e-8
               }

adamOptimizerM
    :: forall m v r a c. (RealFloat c, Floating a, ScalingInPlace m v c a, PrimMonad m)
    => Adam c
    -> (r -> a -> m a)          -- ^ gradient
    -> OptoM m v r a
adamOptimizerM Adam{..} gr = 
    MkOptoM { oInit   = (scaleOne @_ @a, addZero, addZero)
            , oUpdate = \rS r x -> do
                g <- gr r x
                (t, m0, v0) <- readMutVar rS
                let m1 = adamDecay1 .* m0 .+. (1 - adamDecay1) .* g
                    v1 = adamDecay2 .* v0 .+. (1 - adamDecay2) .* g
                    mHat = recip (1 - adamDecay1 ** t) .* m1
                    vHat = recip (1 - adamDecay2 ** t) .* v1
                writeMutVar rS (t + 1, m1, v1)
                return ( -adamStep
                       , mHat / (sqrt vHat + realToFrac adamEpsilon)
                       )
            }

adamOptimizer
    :: (RealFloat c, Floating a, ScalingInPlace m v c a, PrimMonad m)
    => Adam c
    -> (r -> a -> a)          -- ^ gradient
    -> OptoM m v r a
adamOptimizer a gr = adamOptimizerM a (\x -> pure . gr x)
