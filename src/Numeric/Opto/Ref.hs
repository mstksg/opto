{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
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
  , GMutable, GRef(..), gThawRef, gFreezeRef, gCopyRef
  ) where

import           Control.Monad.Primitive
import           Data.Complex
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

class Monad m => GMutable m f where
    type GRef_ m f = (u :: Type -> Type) | u -> f

    gThawRef_ :: f a -> m (GRef_ m f a)
    gFreezeRef_ :: GRef_ m f a -> m (f a)
    gCopyRef_ :: GRef_ m f a -> f a -> m ()

instance Mutable m c => GMutable m (K1 i c) where
    type GRef_ m (K1 i c) = K1 i (Ref m c)

    gThawRef_ = fmap K1 . thawRef . unK1
    gFreezeRef_ = fmap K1 . freezeRef . unK1
    gCopyRef_ (K1 v) (K1 x) = copyRef v x

instance (GMutable m f, GMutable m g) => GMutable m (f :*: g) where
    type GRef_ m (f :*: g) = GRef_ m f :*: GRef_ m g

    gThawRef_ (x :*: y) = (:*:) <$> gThawRef_ x <*> gThawRef_ y
    gFreezeRef_ (v :*: u) = (:*:) <$> gFreezeRef_ v <*> gFreezeRef_ u
    gCopyRef_ (v :*: u) (x :*: y) = gCopyRef_ v x *> gCopyRef_ u y

instance GMutable m f => GMutable m (M1 i c f) where
    type GRef_ m (M1 i c f) = M1 i c (GRef_ m f)

    gThawRef_ = fmap M1 . gThawRef_ . unM1
    gFreezeRef_ = fmap M1 . gFreezeRef_ . unM1
    gCopyRef_ (M1 v) (M1 x) = gCopyRef_ v x

instance (GMutable m f, GMutable m g, PrimMonad m) => GMutable m (f :+: g) where
    type GRef_ m (f :+: g) = MutVar (PrimState m) :.: (GRef_ m f :+: GRef_ m g)

    gThawRef_ = \case
      L1 x -> fmap Comp1 . newMutVar . L1 =<< gThawRef_ x
      R1 x -> fmap Comp1 . newMutVar . R1 =<< gThawRef_ x
    gFreezeRef_ (Comp1 r) = readMutVar r >>= \case
      L1 v -> L1 <$> gFreezeRef_ v
      R1 u -> R1 <$> gFreezeRef_ u
    gCopyRef_ (Comp1 r) xy = readMutVar r >>= \case
      L1 v -> case xy of
        L1 x -> gCopyRef_ v x
        R1 y -> writeMutVar r . R1 =<< gThawRef_ y
      R1 u -> case xy of
        L1 x -> writeMutVar r . L1 =<< gThawRef_ x
        R1 y -> gCopyRef_ u y

newtype GRef m a = GRef { unGRef :: GRef_ m (Rep a) () }

gThawRef
    :: (Generic a, GMutable m (Rep a))
    => a
    -> m (GRef m a)
gThawRef = fmap GRef . gThawRef_ . from

gFreezeRef
    :: (Generic a, GMutable m (Rep a))
    => GRef m a
    -> m a
gFreezeRef = fmap to . gFreezeRef_ . unGRef

gCopyRef
    :: (Generic a, GMutable m (Rep a))
    => GRef m a
    -> a
    -> m ()
gCopyRef (GRef v) x = gCopyRef_ v (from x)
