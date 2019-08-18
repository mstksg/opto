{-# LANGUAGE AllowAmbiguousTypes                      #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DefaultSignatures                        #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveFoldable                           #-}
{-# LANGUAGE DeriveFunctor                            #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE DeriveTraversable                        #-}
{-# LANGUAGE DerivingVia                              #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE FunctionalDependencies                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE StandaloneDeriving                       #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE UndecidableInstances                     #-}
{-# OPTIONS_GHC -Wno-redundant-constraints            #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

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
  , Metric(..), gDot, gNorm_inf, gNorm_0, gNorm_1, gNorm_2, gQuadrance
  , LinearInPlace(..), sumLinearInPlace
  , linearWit
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
import           Data.Vinyl hiding                   ((:~:))
import           GHC.Generics                        (Generic)
import           GHC.TypeLits
import           Generics.OneLiner
import           Numeric.Opto.Ref
import           Unsafe.Coerce
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Generic.Mutable.Sized   as SVGM
import qualified Data.Vector.Generic.Sized           as SVG
import qualified Numeric.LinearAlgebra               as UH
import qualified Numeric.LinearAlgebra.Static        as H
import qualified Numeric.LinearAlgebra.Static.Vector as H

-- | If @a@ is an instance of @'Linear' c@, you can /add/ together values
-- of @a@, and /scale/ them using @c@s.
--
-- For example, if you have a vector of doubles, you can add them together
-- component-wise, and scale them by multiplying every item by the scalar.
--
-- Mathematically, this means that @a@ forms something like a module or
-- vector space over @c@, where @c@ can be any 'Num' instance.
class Num c => Linear c a | a -> c where
    -- | Add together @a@s.  Should be associative.
    --
    -- @
    -- x .+. (y .+. z) == (x .+. y) .+. z
    -- @
    --
    -- If @a@ is an instance of 'Num', this can be just 'Prelude.+'.
    (.+.) :: a -> a -> a

    -- | The "zero" @a@, meant to form an identity with '.+.'.
    --
    -- @
    -- x .+. zeroL == x
    -- zeroL .+. y == y
    -- @
    --
    -- If @a@ is an instance of 'Num', this can be just 0.
    zeroL :: a

    -- | Scale an @a@ by a factor @c@.  Should distribute over '.+.'.
    --
    -- @
    -- a .* (x .+. y) == (a .* x) .+. (a .* y)
    -- a .* (b .* c)  == (a * b) .* c
    -- @
    (.*)  :: c -> a -> a

    infixl 6 .+.
    infixl 7 .*

    default (.+.) :: (ADTRecord a, Constraints a (Linear c)) => a -> a -> a
    (.+.) = gAdd @c

    default zeroL :: (ADTRecord a, Constraints a (Linear c)) => a
    zeroL = gZeroL @c

    default (.*) :: (ADTRecord a, Constraints a (Linear c)) => c -> a -> a
    (.*)  = gScale

-- | Sum over a 'Foldable' container of @'Linear' c a@
sumLinear :: (Linear c a, Foldable t) => t a -> a
sumLinear = foldl' (.+.) zeroL

-- | An implementation of '.+.' that works for records where every field is
-- an instance of @'Linear' c@ (that is, every field is additive and can be
-- scaled by the same @c@).
gAdd :: forall c a. (ADTRecord a, Constraints a (Linear c)) => a -> a -> a
gAdd = binaryOp @(Linear c) (.+.)

-- | An implementation of 'zeroL' that works for records where every field
-- is an instance of @'Linear' c@ (that is, every field is additive and can
-- be scaled by the same @c@).
gZeroL :: forall c a. (ADTRecord a, Constraints a (Linear c)) => a
gZeroL = nullaryOp @(Linear c) zeroL

-- | An implementation of '.*' that works for records where every field
-- is an instance of @'Linear' c@ (that is, every field is additive and can
-- be scaled by the same @c@).
gScale :: forall c a. (ADTRecord a, Constraints a (Linear c)) => c -> a -> a
gScale c = unaryOp @(Linear c) (c .*)

-- | Class for values supporting an inner product and various norms.
class Linear c a => Metric c a where
    infixl 7 <.>
    -- | Sum of component-wise product
    (<.>)    :: a -> a -> c
    -- | Maximum absolute component.  Is undefined if no components exist.
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

-- | An implementation of 'gDot' that works for records where every field
-- is an instance of @'Metric' c@.
gDot :: forall c a. (ADT a, Constraints a (Metric c), Num c) => a -> a -> c
gDot x = getSum . mzipWith @(Metric c) (\x' -> Sum . (x' <.>)) x

-- | An implementation of 'norm_inf' that works for records where every
-- field is an instance of @'Metric' c@.
gNorm_inf :: forall c a. (ADT a, Constraints a (Metric c), Ord c) => a -> c
gNorm_inf = getMax
          . fromMaybe (error "norm_inf: Divergent infinity norm")
          . getOption
          . gfoldMap @(Metric c) (Option . Just . Max . abs . norm_inf)

-- | An implementation of 'norm_0' that works for records where every field
-- is an instance of @'Metric' c@.
gNorm_0 :: forall c a. (ADT a, Constraints a (Metric c), Num c) => a -> c
gNorm_0 = getSum . gfoldMap @(Metric c) (Sum . norm_0)

-- | An implementation of 'norm_1' that works for records where every field
-- is an instance of @'Metric' c@.
gNorm_1 :: forall c a. (ADT a, Constraints a (Metric c), Num c) => a -> c
gNorm_1 = getSum . gfoldMap @(Metric c) (Sum . norm_1)

-- | An implementation of 'norm_2' that works for records where every field
-- is an instance of @'Metric' c@.
gNorm_2 :: forall c a. (ADT a, Constraints a (Metric c), Floating c) => a -> c
gNorm_2 = sqrt . gQuadrance

-- | An implementation of 'quadrance' that works for records where every
-- field is an instance of @'Metric' c@.
gQuadrance :: forall c a. (ADT a, Constraints a (Metric c), Num c) => a -> c
gQuadrance = getSum . gfoldMap @(Metric c) (Sum . quadrance)

-- | Instaces of 'Linear' that support certain in-place mutations.
-- Inspired by the BLAS Level 1 API.  A @'LinearInPlace' m v c a@ means
-- that @v@ is a mutable reference to an @a@ that can be updated as an
-- action in monad @m@.
class (Mutable m a, Linear c a) => LinearInPlace m c a where
    -- | Add a value in-place.
    (.+.=) :: Ref m a -> a -> m ()

    -- | Scale a value in-place.
    (.*=)  :: Ref m a -> c -> m ()

    -- | Add a scaled value in-place.
    (.*+=) :: Ref m a -> (c, a) -> m ()

    r .+.= x      = modifyRef' r (.+. x)
    r  .*= c      = modifyRef' r (c .*)
    r .*+= (c, x) = modifyRef' r ((c .* x) .+.)

    infix 4 .+.=
    infix 4 .*=
    infix 4 .*+=

-- | Given some starting reference @v@, add every item in a foldable
-- container into that reference in-place.
sumLinearInPlace :: (LinearInPlace m c a, Foldable t) => Ref m a -> t a -> m ()
sumLinearInPlace v = mapM_ (v .+.=)

-- | Newtype wrapper that gives "simple" 'Linear', 'Metric', and
-- 'LinearInPlace' instances for instances of 'Num'.
--
-- This can be used with the /-XDerivingVia/ extension:
--
-- @
-- deriving via (LinearNum Double) instance Linear Double Double
-- deriving via (LinearNum Double) instance Metric Double Double
-- instance Ref m Double v => LinearInPlace m v Double Double
-- @
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

instance Mutable m Int                        => LinearInPlace m Int Int
instance Mutable m Integer                    => LinearInPlace m Integer Integer
instance Mutable m Rational                   => LinearInPlace m Rational Rational
instance Mutable m Float                      => LinearInPlace m Float Float
instance Mutable m Double                     => LinearInPlace m Double Double
instance (Mutable m (Complex a), RealFloat a) => LinearInPlace m (Complex a) (Complex a)

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
      => LinearInPlace m a (SVG.Vector v n a) where
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
instance (PrimMonad m, KnownNat n) => LinearInPlace m Double (H.R n) where
    MR v .+.= x = v .+.= H.rVec x
    MR v  .*= c = v  .*= c
    MR v .*+= (c, x) = v .*+= (c, H.rVec x)

instance (KnownNat n, KnownNat m) => Linear Double (H.L n m) where
    (.+.)   = (+)
    zeroL   = 0
    c .* xs = H.konst c * xs
instance (KnownNat n, KnownNat m) => Metric Double (H.L n m) where
    (<.>)     = (UH.<.>) `on` UH.flatten . H.extract
    norm_inf  = UH.maxElement . H.extract . abs
    norm_0    = sum . map norm_0 . H.toRows
    norm_1    = UH.sumElements . H.extract
    norm_2    = UH.norm_2 . UH.flatten . H.extract
    quadrance = (**2) . norm_2
instance (PrimMonad m, KnownNat n, KnownNat k) => LinearInPlace m Double (H.L n k) where
    ML v .+.= x = v .+.= H.lVec x
    ML v  .*= c = v  .*= c
    ML v .*+= (c, x) = v .*+= (c, H.lVec x)

instance (Linear c a, Linear c b) => Linear c (a, b) where
instance (Linear c a, Linear c b, Linear c d) => Linear c (a, b, d) where
instance (Linear c a, Linear c b, Linear c d, Linear c e) => Linear c (a, b, d, e) where
instance (Linear c a, Linear c b, Linear c d, Linear c e, Linear c f) => Linear c (a, b, d, e, f) where

instance (Metric c a, Metric c b, Ord c, Floating c) => Metric c (a, b)
instance (Metric c a, Metric c b, Metric c d, Ord c, Floating c) => Metric c (a, b, d)
instance (Metric c a, Metric c b, Metric c d, Metric c e, Ord c, Floating c) => Metric c (a, b, d, e)
instance (Metric c a, Metric c b, Metric c d, Metric c e, Metric c f, Ord c, Floating c) => Metric c (a, b, d, e, f)

instance (Mutable m (a, b), Linear c a, Linear c b) => LinearInPlace m c (a, b)
instance (Mutable m (a, b, d), Linear c a, Linear c b, Linear c d) => LinearInPlace m c (a, b, d)
instance (Mutable m (a, b, d, e), Linear c a, Linear c b, Linear c d, Linear c e) => LinearInPlace m c (a, b, d, e)
instance (Mutable m (a, b, d, e, f), Linear c a, Linear c b, Linear c d, Linear c e, Linear c f) => LinearInPlace m c (a, b, d, e, f)

instance Linear c (f a) => Linear c (Rec f '[a]) where
    x :& RNil .+. y :& RNil = (x .+. y) :& RNil
    zeroL = zeroL :& RNil
    c .* (x :& RNil) = (c .* x) :& RNil

instance (Linear c (f a), Linear c (Rec f (b ': bs))) => Linear c (Rec f (a ': b ': bs)) where
    x :& xs .+. y :& ys = (x .+. y) :& (xs .+. ys)
    zeroL = zeroL :& zeroL
    c .* (x :& xs) = (c .* x) :& (c .* xs)

instance Metric c (f a) => Metric c (Rec f '[a]) where
    (x :& RNil) <.> (y :& RNil) = x <.> y
    norm_inf (x :& RNil) = norm_inf x
    norm_0 (x :& RNil) = norm_0 x
    norm_1 (x :& RNil) = norm_1 x
    norm_2 (x :& RNil) = norm_2 x
    quadrance (x :& RNil) = quadrance x

instance (Floating c, Ord c, Metric c (f a), Metric c (Rec f (b ': bs))) => Metric c (Rec f (a ': b ': bs)) where
    (<.>) = undefined
    norm_inf (x :& xs) = norm_0 x `max` norm_0 xs
    norm_0 (x :& xs) = norm_0 x + norm_0 xs
    norm_1 (x :& xs) = norm_1 x + norm_1 xs
    norm_2 = sqrt . quadrance
    quadrance (x :& xs) = quadrance x + quadrance xs

instance LinearInPlace m c (f a) => LinearInPlace m c (Rec f '[a]) where
    (RecRef v :& RNil) .+.= (x :& RNil)    = v .+.= x
    (RecRef v :& RNil) .*=  c              = v .*=  c
    (RecRef v :& RNil) .*+= (c, x :& RNil) = v .*+= (c, x)

instance ( LinearInPlace m c (f a)
         , LinearInPlace m c (Rec f (b ': bs))
         )
        => LinearInPlace m c (Rec f (a ': b ': bs)) where
    (RecRef v :& vs) .+.= (x :& xs)    = do v .+.= x; vs .+.= xs
    (RecRef v :& vs) .*=  c            = do v .*=  c; vs .*=  c
    (RecRef v :& vs) .*+= (c, x :& xs) = do v .*+= (c, x); vs .*+= (c, xs)

-- | If @a@ and @b@ are both 'Linear' instances, then if @a@ is equal to
-- @b@, their scalars @c@ and @d@ must also be equal.  This is necessary
-- because GHC isn't happy with the functional dependency for some reason.
linearWit
    :: forall a c d. (Linear c a, Linear d a)
    => (c :~: d)
linearWit = unsafeCoerce Refl
