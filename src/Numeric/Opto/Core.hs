{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Numeric.Opto.Core (
    Diff, Grad, OptoM(..), Opto
  , fromCopying, fromStateless
  -- , reGrad
  , sampling
  , pureGrad, sampling'
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
               , oUpdate :: !( RefVars m ss sVars
                            -> a
                            -> m (c, Diff a)
                             )
               }
            -> OptoM m v a

type Opto s = OptoM (ST s)

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (a -> s -> m (c, Diff a, s))
    -> OptoM m v a
fromCopying s0 update =
    MkOptoM { oInit   = onlyZP (RVl s0)
            , oUpdate = \(headZP->RVr rS) x -> do
                (c, g, s) <- update x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromStateless
    :: ScalingInPlace m v c a
    => (a -> m (c, Diff a))
    -> OptoM m v a
fromStateless update =
    MkOptoM { oInit   = ZPØ
            , oUpdate = \_ -> update
            }

-- reGrad
--     :: (Grad m a -> Grad m a)
--     -> OptoM m v a
--     -> OptoM m v a
-- reGrad f MkOptoM{..} =
--     MkOptoM { oInit   = oInit
--             , oUpdate = \gr -> oUpdate (f gr)
--             }

sampling
    :: MonadSample r m
    => (r -> Grad m a)
    -> Grad m a
sampling f x = do
    r <- sample
    f r x

pureGrad
    :: Applicative m
    => (a -> Diff a)
    -> Grad m a
pureGrad f = pure . f

sampling'
    :: MonadSample r m
    => (r -> a -> Diff a)
    -> Grad m a
sampling' f = sampling (pureGrad . f)
