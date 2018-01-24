{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}

module Numeric.Opto.Ref (
    Ref(..)
  , EmptyRef(..)
  ) where

import           Control.Monad.Primitive
import           Data.Functor
import           Data.Primitive.MutVar
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV

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


data EmptyRef = EmptyRef

instance Monad m => Ref m EmptyRef EmptyRef where
    newRef     _   = pure EmptyRef
    readRef    _   = pure EmptyRef
    writeRef   _ _ = pure ()
    modifyRef  _ _ = pure ()
    modifyRef' _ _ = pure ()
    updateRef  _ f = pure . snd . f $ EmptyRef
    updateRef' _ f = let x = snd (f EmptyRef)
                     in  x `seq` pure x
