{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
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
    Ref(..)
  , Ref1(..)
  , GRef(..)
  ) where

import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import           Data.Proxy
import           Data.Void
import           GHC.Generics
import qualified Data.Vector               as V
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Data.Vector.Mutable       as MV

-- | Abstraction over types of mutable references for values.  A @'Ref'
-- m a v@ means that a @v@ is a mutable reference to an @a@, and we may
-- update/write/modify @v@ in context @m@.
--
-- This allows us to reat mutable vectors and in-place mutable numbers or
-- records in the same way.
class Monad m => Ref m a v | v -> a where
    -- | Initialize a mutable reference with a given value
    thawRef      :: a -> m v
    -- | Read an immutable value back from a mutable reference
    freezeRef    :: v -> m a
    freezeRef v = updateRef v $ \x -> (x,x)
    -- | Copy an immutable value into a mutable reference
    copyRef   :: v -> a -> m ()
    copyRef v x = modifyRef v (const x)
    -- | Apply a pure function on an immutable value onto a value stored in
    -- a mutable reference.
    modifyRef  :: v -> (a -> a) -> m ()
    modifyRef v f = updateRef v ((,()) . f)
    -- | 'modifyRef', but forces the result before storing it back in the
    -- reference.
    modifyRef' :: v -> (a -> a) -> m ()
    modifyRef' v f = updateRef' v ((,()) . f)
    -- | Apply a pure function on an immutable value onto a value stored in
    -- a mutable reference, returning a result value from that function.
    updateRef  :: v -> (a -> (a, b)) -> m b
    updateRef v f = do
        (x, y) <- f <$> freezeRef v
        copyRef v x
        return y
    -- | 'updateRef', but forces the updated value before storing it back in the
    -- reference.
    updateRef' :: v -> (a -> (a, b)) -> m b
    updateRef' v f = do
        (x, y) <- f <$> freezeRef v
        x `seq` copyRef v x
        return y
    {-# MINIMAL thawRef, (copyRef | updateRef, updateRef') #-}

instance (PrimMonad m, PrimState m ~ s) => Ref m a (MutVar s a) where
    thawRef    = newMutVar
    freezeRef  = readMutVar
    copyRef    = writeMutVar
    modifyRef  = modifyMutVar
    modifyRef' = modifyMutVar'
    updateRef  = atomicModifyMutVar
    updateRef' = atomicModifyMutVar'

instance (PrimMonad m, PrimState m ~ s) => Ref m (V.Vector a) (MV.MVector s a) where
    thawRef        = V.thaw
    freezeRef      = V.freeze
    copyRef        = V.copy
    modifyRef r f  = V.copy r . f =<< V.freeze r
    modifyRef' r f = do
      v <- f <$> V.freeze r
      v `seq` V.copy r v
    updateRef r f = do
      (v, x) <- f <$> V.freeze r
      V.copy r v
      return x
    updateRef' r f = do
      (v, x) <- f <$> V.freeze r
      v `seq` x `seq` V.copy r v
      return x

instance (PrimMonad m, VG.Mutable v ~ mv, PrimState m ~ s, VG.Vector v a)
      => Ref m (SVG.Vector v n a) (SVG.MVector mv n s a) where
    thawRef        = SVG.thaw
    freezeRef      = SVG.freeze
    copyRef        = SVG.copy
    modifyRef r f  = SVG.copy r . f =<< SVG.freeze r
    modifyRef' r f = do
      v <- f <$> SVG.freeze r
      v `seq` SVG.copy r v
    updateRef r f = do
      (v, x) <- f <$> SVG.freeze r
      SVG.copy r v
      return x
    updateRef' r f = do
      (v, x) <- f <$> SVG.freeze r
      v `seq` x `seq` SVG.copy r v
      return x

instance Monad m => Ref m Void Void where
    thawRef   = absurd
    freezeRef = absurd
    copyRef   = absurd

instance Monad m => Ref m () () where
    thawRef   _ = pure ()
    freezeRef _ = pure ()
    copyRef _ _ = pure ()

instance (Monad m, Ref m a u, Ref m b v) => Ref m (a, b) (u, v) where
    thawRef   (!x, !y) = (,) <$> thawRef x   <*> thawRef y
    freezeRef (u , v ) = (,) <$> freezeRef u <*> freezeRef v
    copyRef   (u , v ) (!x, !y) = copyRef u x *> copyRef v y

instance (Monad m, Ref m a u, Ref m b v, Ref m c w) => Ref m (a, b, c) (u, v, w) where
    thawRef   (!x, !y, !z) = (,,) <$> thawRef x   <*> thawRef y   <*> thawRef z
    freezeRef (u , v , w ) = (,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w
    copyRef   (u , v , w ) (!x, !y, !z) = copyRef u x *> copyRef v y *> copyRef w z

instance (Monad m, Ref m a u, Ref m b v, Ref m c w, Ref m d j) => Ref m (a, b, c, d) (u, v, w, j) where
    thawRef   (!x, !y, !z, !a) = (,,,) <$> thawRef x   <*> thawRef y   <*> thawRef z   <*> thawRef a
    freezeRef (u , v , w , j ) = (,,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w <*> freezeRef j
    copyRef   (u , v , w , j ) (!x, !y, !z, !a) = copyRef u x *> copyRef v y *> copyRef w z *> copyRef j a

class Monad m => Ref1 m f u | u -> f where
    thawRef1   :: f a -> m (u a)
    freezeRef1 :: u a -> m (f a)
    copyRef1   :: u a -> f a -> m ()

instance Monad m => Ref1 m V1 V1 where
    thawRef1   = \case {}
    freezeRef1 = \case {}
    copyRef1   = \case {}

instance Ref m a v => Ref1 m (K1 i a) (K1 i v) where
    thawRef1   = fmap K1 . thawRef   . unK1
    freezeRef1 = fmap K1 . freezeRef . unK1
    copyRef1 (K1 v) (K1 x) = copyRef v x

instance Monad m => Ref1 m U1 U1 where
    thawRef1 _   = pure U1
    freezeRef1 _ = pure U1
    copyRef1 _ _ = pure ()

instance Ref1 m f u => Ref1 m (M1 i c f) (M1 i' c' u) where
    thawRef1 _ = undefined
    freezeRef1 _ = undefined
    copyRef1 _ _ = undefined

instance Monad m => Ref1 m Proxy Proxy where
    thawRef1 _   = pure Proxy
    freezeRef1 _ = pure Proxy
    copyRef1 _ _ = pure ()

instance (Ref1 m f u, Ref1 m g w) => Ref1 m (f :*: g) (u :*: w) where
    thawRef1   (u :*: w) = (:*:) <$> thawRef1 u   <*> thawRef1 w
    freezeRef1 (x :*: y) = (:*:) <$> freezeRef1 x <*> freezeRef1 y
    copyRef1 (u :*: w) (x :*: y) = copyRef1 u x *> copyRef1 w y

newtype GRef a v = GRef { getGRef :: v }

instance (Monad m, Generic a, Generic v, Ref1 m (Rep a) (Rep v)) => Ref m a (GRef a v) where
    thawRef   = fmap (GRef . to) . thawRef1 . from
    freezeRef = fmap to . freezeRef1 . from . getGRef
    copyRef (GRef v) x = copyRef1 (from v) (from x)

instance Monad m => Ref m (Proxy a) (Proxy a) where
    thawRef   = thawRef1
    copyRef   = copyRef1
    freezeRef = freezeRef1


-- instance Monad m => Ref1 m (K1 i c) (MVar

    -- -- | Initialize a mutable reference with a given value
    -- thawRef      :: a -> m v
    -- -- | Read an immutable value back from a mutable reference
    -- freezeRef    :: v -> m a
    -- freezeRef v = updateRef v $ \x -> (x,x)
    -- -- | Copy an immutable value into a mutable reference
    -- copyRef   :: v -> a -> m ()
    -- copyRef v x = modifyRef v (const x)
    -- -- | Apply a pure function on an immutable value onto a value stored in
    -- -- a mutable reference.
    -- modifyRef  :: v -> (a -> a) -> m ()
    -- modifyRef v f = updateRef v ((,()) . f)
    -- -- | 'modifyRef', but forces the result before storing it back in the
    -- -- reference.
    -- modifyRef' :: v -> (a -> a) -> m ()
    -- modifyRef' v f = updateRef' v ((,()) . f)
    -- -- | Apply a pure function on an immutable value onto a value stored in
    -- -- a mutable reference, returning a result value from that function.
    -- updateRef  :: v -> (a -> (a, b)) -> m b
    -- updateRef v f = do
    --     (x, y) <- f <$> freezeRef v
    --     copyRef v x
    --     return y
    -- -- | 'updateRef', but forces the updated value before storing it back in the
    -- -- reference.
    -- updateRef' :: v -> (a -> (a, b)) -> m b
    -- updateRef' v f = do
    --     (x, y) <- f <$> freezeRef v
    --     x `seq` copyRef v x
    --     return y
    -- {-# MINIMAL thawRef, (copyRef | updateRef, updateRef') #-}
