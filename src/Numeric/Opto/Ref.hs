{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Numeric.Opto.Ref (
    Ref(..)
  , EmptyRef(..)
  ) where

import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV

class Ref m a v | v -> a where
    newRef     :: a -> m v
    readRef    :: v -> m a
    writeRef   :: v -> a -> m ()
    modifyRef  :: v -> (a -> a) -> m ()
    modifyRef' :: v -> (a -> a) -> m ()

instance (PrimMonad m, PrimState m ~ s) => Ref m a (MutVar s a) where
    newRef     = newMutVar
    readRef    = readMutVar
    writeRef   = writeMutVar
    modifyRef  = modifyMutVar
    modifyRef' = modifyMutVar'

instance (PrimMonad m, PrimState m ~ s) => Ref m (V.Vector a) (MV.MVector s a) where
    newRef    = V.thaw
    readRef   = V.freeze
    writeRef  = V.copy
    modifyRef r f = V.copy r . f =<< V.freeze r
    modifyRef' r f = do
      v <- f <$> V.freeze r
      v `seq` V.copy r v

data EmptyRef = EmptyRef

instance Applicative m => Ref m EmptyRef EmptyRef where
    newRef     _   = pure EmptyRef
    readRef    _   = pure EmptyRef
    writeRef   _ _ = pure ()
    modifyRef  _ _ = pure ()
    modifyRef' _ _ = pure ()

