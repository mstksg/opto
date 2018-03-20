{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Numeric.Opto.Core (
    Diff, Grad, OptoM(..), Opto
  , fromCopying, fromStateless, fromStatelessM
  , reGrad
  , sampling
  ) where

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Sample
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Type.ZipProd
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

type Diff   a = a
type Grad m a = a -> m (Diff a)

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: ScalingInPlace m v c a
            => { oInit   :: !( RefVals m ss sVars )
               , oUpdate :: !( Grad m a
                            -> RefVars m ss sVars
                            -> a
                            -> m (c, Diff a)
                             )
               }
            -> OptoM m v a

type Opto s = OptoM (ST s)

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (Grad m a -> a -> s -> m (c, Diff a, s))
    -> OptoM m v a
fromCopying s0 update =
    MkOptoM { oInit   = onlyZP (RVl s0)
            , oUpdate = \gr (headZP->RVr rS) x -> do
                (c, g, s) <- update gr x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromStatelessM
    :: ScalingInPlace m v c a
    => (Grad m a -> a -> m (c, Diff a))
    -> OptoM m v a
fromStatelessM update =
    MkOptoM { oInit   = ZPØ
            , oUpdate = \gr _ -> update gr
            }

fromStateless
    :: ScalingInPlace m v c a
    => (Grad m a -> a -> (c, Diff a))
    -> OptoM m v a
fromStateless update = fromStatelessM (\gr -> pure . update gr)

reGrad
    :: (Grad m a -> Grad m a)
    -> OptoM m v a
    -> OptoM m v a
reGrad f MkOptoM{..} =
    MkOptoM { oInit   = oInit
            , oUpdate = \gr -> oUpdate (f gr)
            }

sampling
    :: MonadSample r m
    => (r -> Grad m a)
    -> Grad m a
sampling f x = do
    r <- sample
    f r x
