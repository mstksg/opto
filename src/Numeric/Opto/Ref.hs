{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DefaultSignatures      #-}
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
  ) where

import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import           Data.Ratio
import           Numeric.Natural
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
class Monad m => Ref m a v | v -> a, m a -> v where
    -- | Initialize a mutable reference with a given value
    thawRef      :: a -> m v
    -- | Read an immutable value back from a mutable reference
    freezeRef    :: v -> m a
    -- | Copy an immutable value into a mutable reference
    copyRef   :: v -> a -> m ()
    -- | Apply a pure function on an immutable value onto a value stored in
    -- a mutable reference.
    modifyRef  :: v -> (a -> a) -> m ()
    modifyRef v f = copyRef v . f =<< freezeRef v
    -- | 'modifyRef', but forces the result before storing it back in the
    -- reference.
    modifyRef' :: v -> (a -> a) -> m ()
    modifyRef' v f = do
      x <- f <$> freezeRef v
      x `seq` copyRef v x
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

    default thawRef :: (PrimMonad m, v ~ MutVar (PrimState m) a) => a -> m v
    thawRef = newMutVar
    default freezeRef :: (PrimMonad m, v ~ MutVar (PrimState m) a) => v -> m a
    freezeRef = readMutVar
    default copyRef :: (PrimMonad m, v ~ MutVar (PrimState m) a) => v -> a -> m ()
    copyRef = writeMutVar

-- instance (PrimMonad m, PrimState m ~ s) => Ref m a (MutVar s a) where
--     thawRef    = newMutVar
--     freezeRef  = readMutVar
--     copyRef    = writeMutVar
--     modifyRef  = modifyMutVar
--     modifyRef' = modifyMutVar'
--     updateRef  = atomicModifyMutVar
--     updateRef' = atomicModifyMutVar'

instance (PrimMonad m, PrimState m ~ s) => Ref m Int (MutVar s Int)
instance (PrimMonad m, PrimState m ~ s) => Ref m Integer (MutVar s Integer)
instance (PrimMonad m, PrimState m ~ s) => Ref m Natural (MutVar s Natural)
instance (PrimMonad m, PrimState m ~ s) => Ref m (Ratio a) (MutVar s (Ratio a))
instance (PrimMonad m, PrimState m ~ s) => Ref m Float (MutVar s Float)
instance (PrimMonad m, PrimState m ~ s) => Ref m Double (MutVar s Double)
instance (PrimMonad m, PrimState m ~ s) => Ref m (Either a b) (MutVar s (Either a b))

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

