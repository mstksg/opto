{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Opto.Ref (
    Ref(..)
  , RefInit(..), RefVar(..), RefInits, RefVars
  , initRefs, readRefs, pullRefs
  ) where

import           Control.Monad.Primitive
import           Data.Functor
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Type.Combinator
import           Data.Type.Product
import           Data.Type.ZipProd
import qualified Data.Vector               as V
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Data.Vector.Mutable       as MV

class Monad m => Ref m a v | v -> a where
    newRef     :: a -> m v
    readRef    :: v -> m a
    readRef v = updateRef v $ \x -> (x,x)
    writeRef   :: v -> a -> m ()
    writeRef v x = modifyRef v (const x)
    modifyRef  :: v -> (a -> a) -> m ()
    modifyRef v f = void $ updateRef v ((,()) . f)
    modifyRef' :: v -> (a -> a) -> m ()
    modifyRef' v f = void $ updateRef' v ((,()) . f)
    updateRef  :: v -> (a -> (a, b)) -> m b
    updateRef' :: v -> (a -> (a, b)) -> m b
    {-# MINIMAL newRef, updateRef, updateRef' #-}

data RefInit :: (Type -> Type) -> Type -> Type -> Type where
    RI :: Ref m a v => a -> RefInit m a v

data RefVar :: (Type -> Type) -> Type -> Type -> Type where
    RV :: Ref m a v => v -> RefVar m a v

type RefInits m = ZipProd (RefInit m)
type RefVars  m = ZipProd (RefVar  m)

initRefs :: Applicative m => ZipProd (RefInit m) as vs -> m (ZipProd (RefVar m) as vs)
initRefs = traverseZP $ \(RI i) -> RV <$> newRef i

readRefs :: Applicative m => ZipProd (RefVar m) as vs -> m (Tuple as)
readRefs = traverseZP1 $ \(RV v) -> I <$> readRef v

pullRefs :: Applicative m => ZipProd (RefVar m) as vs -> m (ZipProd (RefInit m) as vs)
pullRefs = traverseZP $ \(RV v) -> RI <$> readRef v


instance (PrimMonad m, PrimState m ~ s) => Ref m a (MutVar s a) where
    newRef     = newMutVar
    readRef    = readMutVar
    writeRef   = writeMutVar
    modifyRef  = modifyMutVar
    modifyRef' = modifyMutVar'
    updateRef  = atomicModifyMutVar
    updateRef' = atomicModifyMutVar'

instance (PrimMonad m, PrimState m ~ s) => Ref m (V.Vector a) (MV.MVector s a) where
    newRef    = V.thaw
    readRef   = V.freeze
    writeRef  = V.copy
    modifyRef r f = V.copy r . f =<< V.freeze r
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

instance (PrimMonad m, mv ~ VG.Mutable v, PrimState m ~ s, VG.Vector v a)
      => Ref m (SVG.Vector v n a) (SVG.MVector mv n s a) where
    newRef    = SVG.thaw
    readRef   = SVG.freeze
    writeRef  = SVG.copy
    modifyRef r f = SVG.copy r . f =<< SVG.freeze r
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
