{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module      : Numeric.Opto.Update
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- A unified interface for values in vector spaces that can be added and
-- scaled (purely and also in-place), and measured.
module Numeric.Opto.Update (
    Linear(..), sumLinear, gAdd, gZeroL, gScale
  , Metric(..), gNorm_inf, gNorm_0, gNorm_1, gNorm_2, gQuadrance
  , LinearInPlace(..), sumLinearInPlace
  ) where

import           Control.DeepSeq
import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Complex
import           Data.Data
import           Data.Finite
import           Data.Foldable
import           Data.Function
import           Data.Maybe
import           Data.Semigroup
import           GHC.Generics                      (Generic)
import           GHC.TypeLits
import           Generics.OneLiner
import           Numeric.Opto.Ref
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable.Sized as SVGM
import qualified Data.Vector.Generic.Sized         as SVG
import qualified Numeric.LinearAlgebra             as UH
import qualified Numeric.LinearAlgebra.Static      as H

class Num c => Linear c a | a -> c where
    (.+.) :: a -> a -> a
    zeroL :: a
    (.*)  :: c -> a -> a

    infixl 6 .+.
    infixl 7 .*

    default (.+.) :: (ADTRecord a, Constraints a (Linear c)) => a -> a -> a
    (.+.) = gAdd @c

    default zeroL :: (ADTRecord a, Constraints a (Linear c)) => a
    zeroL = gZeroL @c

    default (.*) :: (ADTRecord a, Constraints a (Linear c)) => c -> a -> a
    (.*)  = gScale

sumLinear :: (Linear c a, Foldable t) => t a -> a
sumLinear = foldl' (.+.) zeroL

gAdd :: forall c a. (ADTRecord a, Constraints a (Linear c)) => a -> a -> a
gAdd = binaryOp @(Linear c) (.+.)

gZeroL :: forall c a. (ADTRecord a, Constraints a (Linear c)) => a
gZeroL = nullaryOp @(Linear c) zeroL

gScale :: forall c a. (ADTRecord a, Constraints a (Linear c)) => c -> a -> a
gScale c = unaryOp @(Linear c) (c .*)

class Linear c a => Metric c a where
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

sumLinearInPlace :: (LinearInPlace m v c a, Foldable t) => v -> t a -> m ()
sumLinearInPlace v = mapM_ (v .+.=)

class (Ref m a v, Linear c a) => LinearInPlace m v c a where
    (.+.=) :: v -> a -> m ()
    (.*=)  :: v -> c -> m ()
    (.*+=) :: v -> (c, a) -> m ()

    r .+.= x      = modifyRef' r (.+. x)
    r  .*= c      = modifyRef' r (c .*)
    r .*+= (c, x) = modifyRef' r ((c .* x) .+.)

    infix 4 .+.=
    infix 4 .*=
    infix 4 .*+=

newtype LinearNum a = LinearNum { getLinearNum :: a }
  deriving ( Show, Eq, Ord
           , Functor, Foldable, Traversable
           , Enum, Bounded
           , Num, Fractional, Floating, Real, Integral, RealFrac, RealFloat
           , Generic, Typeable, Data
           )
instance NFData a => NFData (LinearNum a)

instance Num a => Linear a (LinearNum a) where
    (.+.) = (+)
    zeroL = 0
    (.*)  = coerce ((*) :: a -> a -> a)
instance Num a => Metric a (LinearNum a) where
    (<.>)     = coerce ((*) :: a -> a -> a)
    norm_inf  = coerce (abs :: a -> a)
    norm_0    = coerce (abs . signum :: a -> a)
    norm_1    = coerce (abs :: a -> a)
    norm_2    = coerce (abs :: a -> a)
    quadrance = coerce ((^ (2 :: Int)) :: a -> a)

deriving via (LinearNum Int)         instance Linear Int Int
deriving via (LinearNum Integer)     instance Linear Integer Integer
deriving via (LinearNum Rational)    instance Linear Rational Rational
deriving via (LinearNum Float)       instance Linear Float Float
deriving via (LinearNum Double)      instance Linear Double Double
deriving via (LinearNum (Complex a)) instance RealFloat a => Linear (Complex a) (Complex a)

deriving via (LinearNum Int)         instance Metric Int Int
deriving via (LinearNum Integer)     instance Metric Integer Integer
deriving via (LinearNum Rational)    instance Metric Rational Rational
deriving via (LinearNum Float)       instance Metric Float Float
deriving via (LinearNum Double)      instance Metric Double Double
deriving via (LinearNum (Complex a)) instance RealFloat a => Metric (Complex a) (Complex a)

instance Ref m Int v                        => LinearInPlace m v Int Int
instance Ref m Integer v                    => LinearInPlace m v Integer Integer
instance Ref m Rational v                   => LinearInPlace m v Rational Rational
instance Ref m Float v                      => LinearInPlace m v Float Float
instance Ref m Double v                     => LinearInPlace m v Double Double
instance (Ref m (Complex a) v, RealFloat a) => LinearInPlace m v (Complex a) (Complex a)

instance (Num a, VG.Vector v a, KnownNat n) => Linear a (SVG.Vector v n a) where
    (.+.)    = (+)
    zeroL    = 0
    c .* xs  = SVG.map (c *) xs

instance (Floating a, Ord a, VG.Vector v a, KnownNat n) => Metric a (SVG.Vector v n a) where
    xs <.> ys = SVG.sum (xs * ys)
    norm_inf  = SVG.foldl' (\x y -> max (abs x) y) 0
    norm_0    = fromIntegral . SVG.length
    norm_1    = SVG.sum . abs
    quadrance = SVG.sum . (^ (2 :: Int))

instance (PrimMonad m, PrimState m ~ s, Num a, mv ~ VG.Mutable v, VG.Vector v a, KnownNat n)
      => LinearInPlace m (SVG.MVector mv n s a) a (SVG.Vector v n a) where
    r .+.= xs = flip SVG.imapM_ xs $ \i x ->
      SVGM.modify r (+ x) i
    r .*= c = forM_ finites $ \i ->
      SVGM.modify r (c *) i
    r .*+= (c, xs) = flip SVG.imapM_ xs $ \i x ->
      SVGM.modify r (+ (c * x)) i

instance KnownNat n => Linear Double (H.R n) where
    (.+.)   = (+)
    zeroL   = 0
    c .* xs = H.konst c * xs
instance KnownNat n => Metric Double (H.R n) where
    (<.>)     = (H.<.>)
    norm_inf  = H.norm_Inf
    norm_0    = H.norm_0
    norm_1    = H.norm_1
    norm_2    = H.norm_2
    quadrance = (**2) . H.norm_2
instance (KnownNat n, Ref m (H.R n) v) => LinearInPlace m v Double (H.R n)

instance (KnownNat n, KnownNat m) => Linear Double (H.L n m) where
    (.+.)   = (+)
    zeroL   = 0
    c .* xs = H.konst c * xs
instance (KnownNat n, KnownNat m) => Metric Double (H.L n m) where
    (<.>)     = (UH.<.>) `on` UH.flatten . H.extract
    norm_inf  = UH.maxElement . H.extract . abs
    norm_0    = fromIntegral . uncurry (*) . H.size
    norm_1    = UH.sumElements . H.extract
    norm_2    = UH.norm_2 . UH.flatten . H.extract
    quadrance = (**2) . norm_2
instance (KnownNat n, KnownNat k, Ref m (H.L n k) v) => LinearInPlace m v Double (H.L n k)

instance (Linear c a, Linear c b) => Linear c (a, b) where
instance (Linear c a, Linear c b, Linear c d) => Linear c (a, b, d) where
instance (Linear c a, Linear c b, Linear c d, Linear c e) => Linear c (a, b, d, e) where
instance (Linear c a, Linear c b, Linear c d, Linear c e, Linear c f) => Linear c (a, b, d, e, f) where

instance (Metric c a, Metric c b, Ord c, Floating c) => Metric c (a, b)
instance (Metric c a, Metric c b, Metric c d, Ord c, Floating c) => Metric c (a, b, d)
instance (Metric c a, Metric c b, Metric c d, Metric c e, Ord c, Floating c) => Metric c (a, b, d, e)
instance (Metric c a, Metric c b, Metric c d, Metric c e, Metric c f, Ord c, Floating c) => Metric c (a, b, d, e, f)

instance (Ref m (a, b) v, Linear c a, Linear c b) => LinearInPlace m v c (a, b)
instance (Ref m (a, b, d) v, Linear c a, Linear c b, Linear c d) => LinearInPlace m v c (a, b, d)
instance (Ref m (a, b, d, e) v, Linear c a, Linear c b, Linear c d, Linear c e) => LinearInPlace m v c (a, b, d, e)
instance (Ref m (a, b, d, e, f) v, Linear c a, Linear c b, Linear c d, Linear c e, Linear c f) => LinearInPlace m v c (a, b, d, e, f)
