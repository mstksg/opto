{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DerivingVia            #-}
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
  ) where

import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Complex
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Ratio
import           GHC.Generics
import qualified Data.Vector               as V
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Data.Vector.Mutable       as MV

---- | Abstraction over types of mutable references for values.  A @'Ref'
---- m a v@ means that a @v@ is a mutable reference to an @a@, and we may
---- update/write/modify @v@ in context @m@.
----
---- This allows us to reat mutable vectors and in-place mutable numbers or
---- records in the same way.
--class Monad m => Ref m a v | v -> a where
--    -- | Initialize a mutable reference with a given value
--    thawRef      :: a -> m v
--    -- | Read an immutable value back from a mutable reference
--    freezeRef    :: v -> m a
--    freezeRef v = updateRef v $ \x -> (x,x)
--    -- | Copy an immutable value into a mutable reference
--    copyRef   :: v -> a -> m ()
--    copyRef v x = modifyRef v (const x)
--    -- | Apply a pure function on an immutable value onto a value stored in
--    -- a mutable reference.
--    modifyRef  :: v -> (a -> a) -> m ()
--    modifyRef v f = updateRef v ((,()) . f)
--    -- | 'modifyRef', but forces the result before storing it back in the
--    -- reference.
--    modifyRef' :: v -> (a -> a) -> m ()
--    modifyRef' v f = updateRef' v ((,()) . f)
--    -- | Apply a pure function on an immutable value onto a value stored in
--    -- a mutable reference, returning a result value from that function.
--    updateRef  :: v -> (a -> (a, b)) -> m b
--    updateRef v f = do
--        (x, y) <- f <$> freezeRef v
--        copyRef v x
--        return y
--    -- | 'updateRef', but forces the updated value before storing it back in the
--    -- reference.
--    updateRef' :: v -> (a -> (a, b)) -> m b
--    updateRef' v f = do
--        (x, y) <- f <$> freezeRef v
--        x `seq` copyRef v x
--        return y
--    {-# MINIMAL thawRef, (copyRef | updateRef, updateRef') #-}

--instance (PrimMonad m, PrimState m ~ s) => Ref m a (MutVar s a) where
--    thawRef    = newMutVar
--    freezeRef  = readMutVar
--    copyRef    = writeMutVar
--    modifyRef  = modifyMutVar
--    modifyRef' = modifyMutVar'
--    updateRef  = atomicModifyMutVar
--    updateRef' = atomicModifyMutVar'

--instance (PrimMonad m, PrimState m ~ s) => Ref m (V.Vector a) (MV.MVector s a) where
--    thawRef        = V.thaw
--    freezeRef      = V.freeze
--    copyRef        = V.copy
--    modifyRef r f  = V.copy r . f =<< V.freeze r
--    modifyRef' r f = do
--      v <- f <$> V.freeze r
--      v `seq` V.copy r v
--    updateRef r f = do
--      (v, x) <- f <$> V.freeze r
--      V.copy r v
--      return x
--    updateRef' r f = do
--      (v, x) <- f <$> V.freeze r
--      v `seq` x `seq` V.copy r v
--      return x

--instance (PrimMonad m, VG.Mutable v ~ mv, PrimState m ~ s, VG.Vector v a)
--      => Ref m (SVG.Vector v n a) (SVG.MVector mv n s a) where
--    thawRef        = SVG.thaw
--    freezeRef      = SVG.freeze
--    copyRef        = SVG.copy
--    modifyRef r f  = SVG.copy r . f =<< SVG.freeze r
--    modifyRef' r f = do
--      v <- f <$> SVG.freeze r
--      v `seq` SVG.copy r v
--    updateRef r f = do
--      (v, x) <- f <$> SVG.freeze r
--      SVG.copy r v
--      return x
--    updateRef' r f = do
--      (v, x) <- f <$> SVG.freeze r
--      v `seq` x `seq` SVG.copy r v
--      return x

--instance Monad m => Ref m () () where
--    thawRef   _ = pure ()
--    freezeRef _ = pure ()
--    copyRef _ _ = pure ()

--instance (Monad m, Ref m a u, Ref m b v) => Ref m (a, b) (u, v) where
--    thawRef   (!x, !y) = (,) <$> thawRef x   <*> thawRef y
--    freezeRef (u , v ) = (,) <$> freezeRef u <*> freezeRef v
--    copyRef   (u , v ) (!x, !y) = copyRef u x *> copyRef v y

--instance (Monad m, Ref m a u, Ref m b v, Ref m c w) => Ref m (a, b, c) (u, v, w) where
--    thawRef   (!x, !y, !z) = (,,) <$> thawRef x   <*> thawRef y   <*> thawRef z
--    freezeRef (u , v , w ) = (,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w
--    copyRef   (u , v , w ) (!x, !y, !z) = copyRef u x *> copyRef v y *> copyRef w z

--instance (Monad m, Ref m a u, Ref m b v, Ref m c w, Ref m d j) => Ref m (a, b, c, d) (u, v, w, j) where
--    thawRef   (!x, !y, !z, !a) = (,,,) <$> thawRef x   <*> thawRef y   <*> thawRef z   <*> thawRef a
--    freezeRef (u , v , w , j ) = (,,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w <*> freezeRef j
--    copyRef   (u , v , w , j ) (!x, !y, !z, !a) = copyRef u x *> copyRef v y *> copyRef w z *> copyRef j a


class Monad m => Mutable m a where
    type Ref m a = (v :: Type) | v -> a
    type Ref m a = MutVar (PrimState m) a

    thawRef   :: a -> m (Ref m a)
    freezeRef :: Ref m a -> m a
    copyRef   :: Ref m a -> a -> m ()

    default thawRef :: (Ref m a ~ MutVar (PrimState m) a, PrimMonad m) => a -> m (Ref m a)
    thawRef   = newMutVar
    default freezeRef :: (Ref m a ~ MutVar (PrimState m) a, PrimMonad m) => Ref m a -> m a
    freezeRef = readMutVar
    default copyRef :: (Ref m a ~ MutVar (PrimState m) a, PrimMonad m) => Ref m a -> a -> m ()
    copyRef = writeMutVar

instance PrimMonad m => Mutable m Int
instance PrimMonad m => Mutable m Integer
instance PrimMonad m => Mutable m (Ratio a)
instance PrimMonad m => Mutable m Float
instance PrimMonad m => Mutable m Double
instance PrimMonad m => Mutable m (Complex a)

-- class BVGroup s as i o | o -> i, i -> as where
--     -- | Helper method for generically "splitting" 'BVar's out of
--     -- constructors inside a 'BVar'.  See 'splitBV'.
--     gsplitBV :: Rec AddFunc as -> Rec ZeroFunc as -> BVar s (i ()) -> o ()
--     -- | Helper method for generically "joining" 'BVar's inside
--     -- a constructor into a 'BVar'.  See 'joinBV'.
--     gjoinBV  :: Rec AddFunc as -> Rec ZeroFunc as -> o () -> BVar s (i ())

-- newtype RefOf m a = RefOf { getRefOf :: Ref m a }

class Monad m => GMutable m f g | g -> f, m f -> g where
    gThawRef_   :: f x -> m (g x)
    gFreezeRef_ :: g x -> m (f x)
    gCopyRef_   :: g x -> f x -> m ()

instance GMutable m f g => GMutable m (M1 i c f) (M1 i c g) where
    gThawRef_   = fmap M1 . gThawRef_ . unM1
    gFreezeRef_ = fmap M1 . gFreezeRef_ . unM1
    gCopyRef_ (M1 v) (M1 x) = gCopyRef_ v x

instance (Mutable m a, v ~ Ref m a) => GMutable m (K1 i a) (K1 i v) where
    gThawRef_   = fmap K1 . thawRef @m @a   . unK1
    gFreezeRef_ = fmap K1 . freezeRef @m @a . unK1
    gCopyRef_ (K1 v) (K1 x) = copyRef v x

instance (GMutable m f g, GMutable m f' g') => GMutable m (f :*: f') (g :*: g') where
    gThawRef_   (x :*: y) = (:*:) <$> gThawRef_ x   <*> gThawRef_ y
    gFreezeRef_ (u :*: v) = (:*:) <$> gFreezeRef_ u <*> gFreezeRef_ v
    gCopyRef_ (u :*: v) (x :*: y) = gCopyRef_ u x *> gCopyRef_ v y

-- newtype GRef m f a = GRef { getGRef :: f }

-- instance Monad m => Mutable m a where



-- newtype MutVarRef a = MVR { getMVR :: a }

-- newtype MutVarVar a = MVV { getMVR :: MutVar (PrimState m) a }

-- instance PrimMonad m => Mutable m (MutVarRef a) where
--     type Ref m (MutVarRef a) = MutVar (PrimState m) a

--     thawRef   = coerce $ newMutVar @m @a
--     freezeRef = fmap coerce . readMutVar @m @a
--     copyRef   = coerce $ writeMutVar @m @a

-- -- deriving via MutVarRef Int instance PrimMonad m => Mutable m Int



