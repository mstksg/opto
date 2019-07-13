{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      : Numeric.Opto.Ref
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Abstract over different types for mutable references of values.
module Numeric.Opto.Ref (
    Mutable(..)
  , MutRef(..)
  , RefOf(..), ValOf(..)
  , GMutable, gThawRef, gFreezeRef, gCopyRef
  ) where

import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Complex
import           Data.Functor.Identity
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Ratio
import           GHC.Generics
import qualified Data.Vector               as V
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Data.Vector.Mutable       as MV

class Monad m => Mutable m a where
    type Ref m a = (v :: Type) | v -> a
    type Ref m a = MutVar (PrimState m) a

    thawRef   :: a -> m (Ref m a)
    freezeRef :: Ref m a -> m a
    copyRef   :: Ref m a -> a -> m ()

    -- | Apply a pure function on an immutable value onto a value stored in
    -- a mutable reference.
    modifyRef  :: Ref m a -> (a -> a) -> m ()
    modifyRef v f = updateRef v ((,()) . f)
    -- | 'modifyRef', but forces the result before storing it back in the
    -- reference.
    modifyRef' :: Ref m a -> (a -> a) -> m ()
    modifyRef' v f = updateRef' v ((,()) . f)
    -- | Apply a pure function on an immutable value onto a value stored in
    -- a mutable reference, returning a result value from that function.
    updateRef  :: Ref m a -> (a -> (a, b)) -> m b
    updateRef v f = do
        (x, y) <- f <$> freezeRef v
        copyRef v x
        return y
    -- | 'updateRef', but forces the updated value before storing it back in the
    -- reference.
    updateRef' :: Ref m a -> (a -> (a, b)) -> m b
    updateRef' v f = do
        (x, y) <- f <$> freezeRef v
        x `seq` copyRef v x
        return y

    default thawRef :: (Ref m a ~ MutVar (PrimState m) a, PrimMonad m) => a -> m (Ref m a)
    thawRef   = newMutVar
    default freezeRef :: (Ref m a ~ MutVar (PrimState m) a, PrimMonad m) => Ref m a -> m a
    freezeRef = readMutVar
    default copyRef :: (Ref m a ~ MutVar (PrimState m) a, PrimMonad m) => Ref m a -> a -> m ()
    copyRef = writeMutVar

    {-# MINIMAL #-}

instance PrimMonad m => Mutable m Int
instance PrimMonad m => Mutable m Integer
instance PrimMonad m => Mutable m (Ratio a)
instance PrimMonad m => Mutable m Float
instance PrimMonad m => Mutable m Double
instance PrimMonad m => Mutable m (Complex a)

-- | Newtype wrapper that can provide any type with a 'Mutable' instance.
-- Can be useful for avoiding orphan instances.
newtype MutRef a = MutRef { runMutRef :: a }

instance PrimMonad m => Mutable m (MutRef a)

instance PrimMonad m => Mutable m (V.Vector a) where
    type Ref m (V.Vector a) = MV.MVector (PrimState m) a

    thawRef        = V.thaw
    freezeRef      = V.freeze
    copyRef        = V.copy

instance (PrimMonad m, VG.Vector v a) => Mutable m (SVG.Vector v n a) where
    type Ref m (SVG.Vector v n a) = SVG.MVector (VG.Mutable v) n (PrimState m) a
    thawRef        = SVG.thaw
    freezeRef      = SVG.freeze
    copyRef        = SVG.copy

instance Monad m => Mutable m () where
    type Ref m () = ()
    thawRef   _ = pure ()
    freezeRef _ = pure ()
    copyRef _ _ = pure ()

instance (Monad m, Mutable m a, Mutable m b) => Mutable m (a, b) where
    type Ref m (a, b) = (Ref m a, Ref m b)
    thawRef   (!x, !y) = (,) <$> thawRef x   <*> thawRef y
    freezeRef (u , v ) = (,) <$> freezeRef u <*> freezeRef v
    copyRef   (u , v ) (!x, !y) = copyRef u x *> copyRef v y

instance (Monad m, Mutable m a, Mutable m b, Mutable m c) => Mutable m (a, b, c) where
    type Ref m (a, b, c) = (Ref m a, Ref m b, Ref m c)
    thawRef   (!x, !y, !z) = (,,) <$> thawRef x   <*> thawRef y   <*> thawRef z
    freezeRef (u , v , w ) = (,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w
    copyRef   (u , v , w ) (!x, !y, !z) = copyRef u x *> copyRef v y *> copyRef w z

instance (Monad m, Mutable m a, Mutable m b, Mutable m c, Mutable m d) => Mutable m (a, b, c, d) where
    type Ref m (a, b, c, d) = (Ref m a, Ref m b, Ref m c, Ref m d)
    thawRef   (!x, !y, !z, !a) = (,,,) <$> thawRef x   <*> thawRef y   <*> thawRef z   <*> thawRef a
    freezeRef (u , v , w , j ) = (,,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w <*> freezeRef j
    copyRef   (u , v , w , j ) (!x, !y, !z, !a) = copyRef u x *> copyRef v y *> copyRef w z *> copyRef j a


-- class BVGroup s as i o | o -> i, i -> as where
--     -- | Helper method for generically "splitting" 'BVar's out of
--     -- constructors inside a 'BVar'.  See 'splitBV'.
--     gsplitBV :: Rec AddFunc as -> Rec ZeroFunc as -> BVar s (i ()) -> o ()
--     -- | Helper method for generically "joining" 'BVar's inside
--     -- a constructor into a 'BVar'.  See 'joinBV'.
--     gjoinBV  :: Rec AddFunc as -> Rec ZeroFunc as -> o () -> BVar s (i ())

newtype RefOf m a = RefOf { getRefOf :: Ref m a }
newtype ValOf a = ValOf { getValOf :: a }

instance Mutable m a => Mutable m (ValOf a) where
    type Ref m (ValOf a) = RefOf m a

    thawRef   = fmap coerce . thawRef @m @a   . coerce
    freezeRef = fmap coerce . freezeRef @m @a . coerce
    copyRef v x = copyRef @m @a (coerce v) (coerce x)

class Monad m => GMutable m f g | g -> f, m f -> g where
    gThawRef_   :: f x -> m (g x)
    gFreezeRef_ :: g x -> m (f x)
    gCopyRef_   :: g x -> f x -> m ()

instance GMutable m f g => GMutable m (M1 i c f) (M1 i c g) where
    gThawRef_   = fmap M1 . gThawRef_ . unM1
    gFreezeRef_ = fmap M1 . gFreezeRef_ . unM1
    gCopyRef_ (M1 v) (M1 x) = gCopyRef_ v x

instance (Mutable m a) => GMutable m (K1 i a) (K1 i (RefOf m a)) where
    gThawRef_   = fmap (K1 . RefOf) . thawRef @m @a   . unK1
    gFreezeRef_ = fmap K1 . freezeRef @m @a . getRefOf . unK1
    gCopyRef_ (K1 (RefOf v)) (K1 x) = copyRef v x

instance (GMutable m f g, GMutable m f' g') => GMutable m (f :*: f') (g :*: g') where
    gThawRef_   (x :*: y) = (:*:) <$> gThawRef_ x   <*> gThawRef_ y
    gFreezeRef_ (u :*: v) = (:*:) <$> gFreezeRef_ u <*> gFreezeRef_ v
    gCopyRef_ (u :*: v) (x :*: y) = gCopyRef_ u x *> gCopyRef_ v y

gThawRef
    :: forall z m.
     ( Generic (z Identity)
     , Generic (z (RefOf m))
     , GMutable m (Rep (z Identity)) (Rep (z (RefOf m)))
     )
    => z Identity
    -> m (z (RefOf m))
gThawRef = fmap to . gThawRef_ . from

gFreezeRef
    :: forall z m.
     ( Generic (z Identity)
     , Generic (z (RefOf m))
     , GMutable m (Rep (z Identity)) (Rep (z (RefOf m)))
     )
    => z (RefOf m)
    -> m (z Identity)
gFreezeRef = fmap to . gFreezeRef_ . from

gCopyRef
    :: forall z m.
     ( Generic (z Identity)
     , Generic (z (RefOf m))
     , GMutable m (Rep (z Identity)) (Rep (z (RefOf m)))
     )
    => z (RefOf m)
    -> z Identity
    -> m ()
gCopyRef v x = gCopyRef_ (from v) (from x)

