{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Opto.Step (
    Additive(..)
  , Scaling(..)
  , Metric(..)
  , AdditiveInPlace(..)
  , ScalingInPlace(..)
  ) where

-- import           Data.Proxy
-- import           Data.Type.Equality
-- import           GHC.TypeLits.Compare
-- import qualified Data.Vector                    as V
-- import qualified Data.Vector.Mutable            as MV
import           Control.Monad.Primitive
import           Data.Finite
import           Data.Foldable
import           GHC.TypeLits
import           Numeric.Opto.Ref
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable.Sized as SVGM
import qualified Data.Vector.Generic.Sized         as SVG

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

instance (Num a, VG.Vector v a, KnownNat n) => Additive (SVG.Vector v n a) where
    (.+.)   = (+)
    addZero = 0

instance (Num a, VG.Vector v a, KnownNat n) => Scaling a (SVG.Vector v n a) where
    c .* xs  = SVG.map (c *) xs
    scaleOne = 1

instance (Num a, Ord a, VG.Vector v a, KnownNat n) => Metric a (SVG.Vector v n a) where
    xs <.> ys = SVG.sum (xs * ys)
    norm_inf  = SVG.foldl' (\x y -> max (abs x) y) 0
    norm_0    = fromIntegral . SVG.length
    norm_1    = SVG.sum . abs
    norm_2    = SVG.sum . (^ (2 :: Int))

instance (PrimMonad m, PrimState m ~ s, Num a, mv ~ VG.Mutable v, VG.Vector v a, KnownNat n)
      => AdditiveInPlace m (SVG.MVector mv n s a) (SVG.Vector v n a) where
    r .+.= xs = flip SVG.imapM_ xs $ \i x ->
      SVGM.modify r (+ x) i

instance (PrimMonad m, PrimState m ~ s, Num a, mv ~ VG.Mutable v, VG.Vector v a, KnownNat n)
      => ScalingInPlace m (SVG.MVector mv n s a) a (SVG.Vector v n a) where
    r .*= c = forM_ finites $ \i ->
      SVGM.modify r (c *) i
    r .*+= (c, xs) = flip SVG.imapM_ xs $ \i x ->
      SVGM.modify r (+ (c * x)) i

