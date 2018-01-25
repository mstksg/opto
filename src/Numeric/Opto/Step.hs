{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Opto.Step (
    Additive(..)
  , Scaling(..)
  , Metric(..)
  , AdditiveInPlace(..)
  , ScalingInPlace(..)
  ) where

import           Control.Monad.Primitive
import           Data.Foldable
import           Numeric.Opto.Ref
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV

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

class Scaling c a => Metric c a where
    infixl 7 <.>
    (<.>)    :: a -> a -> c
    norm_inf :: a -> c
    norm_0   :: a -> c
    norm_1   :: a -> c
    norm_2   :: a -> c

    default (<.>) :: (Num a, c ~ a) => a -> a -> c
    (<.>) = (*)
    default norm_inf :: (Num a, c ~ a) => a -> c
    norm_inf = abs
    default norm_0 :: (Num a, c ~ a) => a -> c
    norm_0 = abs
    default norm_1 :: (Num a, c ~ a) => a -> c
    norm_1 = abs
    default norm_2 :: (Num a, c ~ a) => a -> c
    norm_2 = abs

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

instance Additive Double
instance Scaling Double Double
instance Metric Double Double
instance Ref m Double v => AdditiveInPlace m v Double
instance Ref m Double v => ScalingInPlace m v Double Double

instance Num a => Additive (V.Vector a) where
    (.+.) = V.zipWith (+)
    addZero = undefined

instance Num a => Scaling a (V.Vector a) where
    c .* xs  = (c *) <$> xs
    scaleOne = 1

instance (Num a, Ord a) => Metric a (V.Vector a) where
    xs <.> ys = V.sum $ V.zipWith (*) xs ys
    norm_inf  = V.maximum . V.map abs
    norm_0    = fromIntegral . V.length
    norm_1    = V.sum . V.map abs
    norm_2    = V.sum . V.map (^ (2 :: Int))

instance (PrimMonad m, PrimState m ~ s, Num a) => AdditiveInPlace m (MV.MVector s a) (V.Vector a) where
    r .+.= xs = flip V.imapM_ xs $ \i x ->
      MV.modify r (+ x) i

instance (PrimMonad m, PrimState m ~ s, Num a) => ScalingInPlace m (MV.MVector s a) a (V.Vector a) where
    r .*= c = forM_ [0 .. MV.length r - 1] $ \i ->
      MV.modify r (c *) i

