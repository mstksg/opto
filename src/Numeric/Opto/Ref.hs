{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Opto.Ref (
    Ref(..)
  , RefVal(..), RefVar(..), RefVals, RefVars
  , thawRefs, freezeRefs, pullRefs
  ) where

import           Control.Monad.Primitive
import           Data.Functor
import           Data.Functor.Identity
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Type.ZipProd
import           Data.Vinyl.Core
import qualified Data.Vector               as V
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Data.Vector.Mutable       as MV

-- | Abstraction over mutable references of types.  A @'Ref' m a v@ means
-- that a @v@ is a mutable reference to an @a@, and we may
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
    modifyRef v f = void $ updateRef v ((,()) . f)
    -- | 'modifyRef', but forces the result before storing it back in the
    -- reference.
    modifyRef' :: v -> (a -> a) -> m ()
    modifyRef' v f = void $ updateRef' v ((,()) . f)
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

-- | @'RefVal' m a v@ is a wrapper over a pure value @a@ that can be
-- stored in a mutable reference @v@.
data RefVal :: (Type -> Type) -> Type -> Type -> Type where
    RVl :: Ref m a v => a -> RefVal m a v

-- | @'RefVar' m a v@ is a wrapper over a mutable reference @v@ that
-- contains a value @a@.
data RefVar :: (Type -> Type) -> Type -> Type -> Type where
    RVr :: Ref m a v => v -> RefVar m a v

-- | Store multiple 'RefVal's
type RefVals m = ZipProd (RefVal m)

-- | Store multiple 'RefVar's
type RefVars m = ZipProd (RefVar  m)

-- | 'thawRef', but over a collection 'RefVals'.
thawRefs :: Applicative m => RefVals m as vs -> m (RefVars m as vs)
thawRefs = traverseZP $ \(RVl i) -> RVr <$> thawRef i

-- | 'freezeRef', but over a collection 'RefVars'.
freezeRefs :: Applicative m => RefVars m as vs -> m (Rec Identity as)
freezeRefs = traverseZP1 $ \(RVr v) -> Identity <$> freezeRef v

-- | A version of 'freezeRefs' that returns 'RefVals'.
pullRefs :: Applicative m => RefVars m as vs -> m (RefVals m as vs)
pullRefs = traverseZP $ \(RVr v) -> RVl <$> freezeRef v

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
