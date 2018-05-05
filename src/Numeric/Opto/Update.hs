{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Opto.Update (
    Additive(..), sumAdditive, gAdd, gAddZero
  , Scaling(..), gScale
  , Metric(..), gNorm_inf, gNorm_0, gNorm_1, gNorm_2, gQuadrance
  , AdditiveInPlace(..), sumAdditiveInPlace
  , ScalingInPlace(..)
  ) where

import           Control.Monad.Primitive
import           Data.Finite
import           Data.Foldable
import           Data.Function
import           Data.Maybe
import           Data.Semigroup
import           Data.Type.Length
import           GHC.TypeLits
import           Generics.OneLiner
-- import           Numeric.Backprop.Tuple
import           Numeric.Opto.Ref
import           Type.Class.Known
import           Type.Family.List
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable.Sized as SVGM
import qualified Data.Vector.Generic.Sized         as SVG
import qualified Numeric.LinearAlgebra             as UH
import qualified Numeric.LinearAlgebra.Static      as H

class Additive a where
    infixl 6 .+.
    (.+.)   :: a -> a -> a
    addZero :: a

    default (.+.) :: Num a => a -> a -> a
    (.+.) = (+)
    default addZero :: Num a => a
    addZero = 0

sumAdditive :: (Additive a, Foldable t) => t a -> a
sumAdditive = foldl' (.+.) addZero

gAdd :: (ADTRecord a, Constraints a Additive) => a -> a -> a
gAdd = binaryOp @Additive (.+.)

gAddZero :: (ADTRecord a, Constraints a Additive) => a
gAddZero = nullaryOp @Additive addZero

class (Num c, Additive a) => Scaling c a | a -> c where
    infixl 7 .*
    (.*)     :: c -> a -> a
    scaleOne :: c

    default (.*) :: (ADTRecord a, Constraints a (Scaling c)) => c -> a -> a
    (.*) = gScale
    default scaleOne :: Num c => c
    scaleOne = 1

gScale :: forall c a. (ADTRecord a, Constraints a (Scaling c)) => c -> a -> a
gScale c = unaryOp @(Scaling c) (c .*)

class Scaling c a => Metric c a where
    infixl 7 <.>
    -- | Sum of component-wise product
    (<.>)    :: a -> a -> c
    -- | Maximum absolute component
    norm_inf :: a -> c
    -- | Number of non-zero components
    norm_0   :: a -> c
    -- | Sum of absolute components
    norm_1   :: a -> c
    -- | Square root of sum of squared components
    norm_2    :: a -> c
    -- | Sum of squared components
    quadrance :: a -> c

    default (<.>) :: (ADT a, Constraints a (Metric c)) => a -> a -> c
    (<.>) = gDot
    default norm_inf :: (ADT a, Constraints a (Metric c), Ord c) => a -> c
    norm_inf = gNorm_inf
    default norm_0 :: (ADT a, Constraints a (Metric c)) => a -> c
    norm_0 = gNorm_0
    default norm_1 :: (ADT a, Constraints a (Metric c)) => a -> c
    norm_1 = gNorm_1
    default norm_2 :: Floating c => a -> c
    norm_2 = sqrt . quadrance
    default quadrance :: (ADT a, Constraints a (Metric c)) => a -> c
    quadrance = gQuadrance

gDot :: forall c a. (ADT a, Constraints a (Metric c), Num c) => a -> a -> c
gDot x = getSum . mzipWith @(Metric c) (\x' -> Sum . (x' <.>)) x

gNorm_inf :: forall c a. (ADT a, Constraints a (Metric c), Ord c) => a -> c
gNorm_inf = getMax
          . fromMaybe (error "norm_inf: Divergent infinity norm")
          . getOption
          . gfoldMap @(Metric c) (Option . Just . Max . abs . norm_inf)

gNorm_0 :: forall c a. (ADT a, Constraints a (Metric c), Num c) => a -> c
gNorm_0 = getSum . gfoldMap @(Metric c) (Sum . norm_0)

gNorm_1 :: forall c a. (ADT a, Constraints a (Metric c), Num c) => a -> c
gNorm_1 = getSum . gfoldMap @(Metric c) (Sum . norm_1)

gNorm_2 :: forall c a. (ADT a, Constraints a (Metric c), Floating c) => a -> c
gNorm_2 = sqrt . gQuadrance

gQuadrance :: forall c a. (ADT a, Constraints a (Metric c), Num c) => a -> c
gQuadrance = getSum . gfoldMap @(Metric c) (Sum . quadrance)

class (Ref m a v, Additive a) => AdditiveInPlace m v a where
    infix 4 .+.=
    (.+.=) :: v -> a -> m ()
    r .+.= x = modifyRef' r (.+. x)

sumAdditiveInPlace :: (AdditiveInPlace m v a, Foldable t) => v -> t a -> m ()
sumAdditiveInPlace v = mapM_ (v .+.=)

class (AdditiveInPlace m v a, Scaling c a) => ScalingInPlace m v c a where
    infix 4 .*=
    (.*=)  :: v -> c -> m ()
    r .*= c = modifyRef' r (c .*)
    infix 4 .*+=
    (.*+=) :: v -> (c, a) -> m ()
    r .*+= (c, x) = modifyRef' r ((c .* x) .+.)

instance Additive Double
instance Scaling Double Double where
    (.*) = (*)
instance Metric Double Double where
    (<.>)     = (*)
    norm_inf  = abs
    norm_0    = abs . signum
    norm_1    = abs
    norm_2    = abs
    quadrance = (^ (2 :: Int))

instance Ref m Double v => AdditiveInPlace m v Double
instance Ref m Double v => ScalingInPlace m v Double Double

instance (Num a, VG.Vector v a, KnownNat n) => Additive (SVG.Vector v n a) where
    (.+.)   = (+)
    addZero = 0

instance (Num a, VG.Vector v a, KnownNat n) => Scaling a (SVG.Vector v n a) where
    c .* xs  = SVG.map (c *) xs
    scaleOne = 1

instance (Floating a, Ord a, VG.Vector v a, KnownNat n) => Metric a (SVG.Vector v n a) where
    xs <.> ys = SVG.sum (xs * ys)
    norm_inf  = SVG.foldl' (\x y -> max (abs x) y) 0
    norm_0    = fromIntegral . SVG.length
    norm_1    = SVG.sum . abs
    quadrance = SVG.sum . (^ (2 :: Int))

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

instance Additive (H.R n)
instance KnownNat n => Scaling Double (H.R n) where
    c .* xs  = H.konst c * xs
    scaleOne = 1
instance KnownNat n => Metric Double (H.R n) where
    (<.>)    = (H.<.>)
    norm_inf = H.norm_Inf
    norm_0   = H.norm_0
    norm_1   = H.norm_1
    norm_2   = H.norm_2
    quadrance = (**2) . H.norm_2
instance (KnownNat n, Ref m (H.R n) v) => AdditiveInPlace m v (H.R n)
instance (KnownNat n, Ref m (H.R n) v) => ScalingInPlace m v Double (H.R n)

instance (KnownNat n, KnownNat m) => Additive (H.L n m)
instance (KnownNat n, KnownNat m) => Scaling Double (H.L n m) where
    c .* xs  = H.konst c * xs
    scaleOne = 1
instance (KnownNat n, KnownNat m) => Metric Double (H.L n m) where
    (<.>)     = (UH.<.>) `on` UH.flatten . H.extract
    norm_inf  = UH.maxElement . H.extract . abs
    norm_0    = fromIntegral . uncurry (*) . H.size
    norm_1    = UH.sumElements . H.extract
    norm_2    = UH.norm_2 . UH.flatten . H.extract
    quadrance = (**2) . norm_2
instance (KnownNat n, KnownNat k, Ref m (H.L n k) v) => AdditiveInPlace m v (H.L n k)
instance (KnownNat n, KnownNat k, Ref m (H.L n k) v) => ScalingInPlace m v Double (H.L n k)

instance (Additive a, Additive b) => Additive (a, b) where
    (.+.)   = gAdd
    addZero = gAddZero
instance (Additive a, Additive b, Additive c) => Additive (a, b, c) where
    (.+.)   = gAdd
    addZero = gAddZero
instance (Additive a, Additive b, Additive c, Additive d) => Additive (a, b, c, d) where
    (.+.)   = gAdd
    addZero = gAddZero
instance (Additive a, Additive b, Additive c, Additive d, Additive e) => Additive (a, b, c, d, e) where
    (.+.)   = gAdd
    addZero = gAddZero

instance (Scaling c a, Scaling c b) => Scaling c (a, b)
instance (Scaling c a, Scaling c b, Scaling c d) => Scaling c (a, b, d)
instance (Scaling c a, Scaling c b, Scaling c d, Scaling c e) => Scaling c (a, b, d, e)
instance (Scaling c a, Scaling c b, Scaling c d, Scaling c e, Scaling c f) => Scaling c (a, b, d, e, f)

instance (Metric c a, Metric c b, Ord c, Floating c) => Metric c (a, b)
instance (Metric c a, Metric c b, Metric c d, Ord c, Floating c) => Metric c (a, b, d)
instance (Metric c a, Metric c b, Metric c d, Metric c e, Ord c, Floating c) => Metric c (a, b, d, e)
instance (Metric c a, Metric c b, Metric c d, Metric c e, Metric c f, Ord c, Floating c) => Metric c (a, b, d, e, f)

-- TODO: different refs
instance (Ref m (a, b) v, Additive a, Additive b) => AdditiveInPlace m v (a, b)
instance (Ref m (a, b, c) v, Additive a, Additive b, Additive c) => AdditiveInPlace m v (a, b, c)
instance (Ref m (a, b, c, d) v, Additive a, Additive b, Additive c, Additive d) => AdditiveInPlace m v (a, b, c, d)
instance (Ref m (a, b, c, d, e) v, Additive a, Additive b, Additive c, Additive d, Additive e) => AdditiveInPlace m v (a, b, c, d, e)

instance (Ref m (a, b) v, Scaling c a, Scaling c b) => ScalingInPlace m v c (a, b)
instance (Ref m (a, b, d) v, Scaling c a, Scaling c b, Scaling c d) => ScalingInPlace m v c (a, b, d)
instance (Ref m (a, b, d, e) v, Scaling c a, Scaling c b, Scaling c d, Scaling c e) => ScalingInPlace m v c (a, b, d, e)
instance (Ref m (a, b, d, e, f) v, Scaling c a, Scaling c b, Scaling c d, Scaling c e, Scaling c f) => ScalingInPlace m v c (a, b, d, e, f)
