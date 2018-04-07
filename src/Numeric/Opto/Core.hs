{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Numeric.Opto.Core (
    Diff, Grad, OptoM(..), Opto
  , fromStateless
  , sampling
  , pureGrad, pureSampling
  ) where

import           Control.Monad.ST
import           Control.Monad.Sample
import           Data.Kind
import           Numeric.Opto.Update

type Diff   a = a
type Grad m a = a -> m (Diff a)

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: Scaling c a
            => { oInit   :: !s
               , oUpdate :: !( a -> s -> m ((c, Diff a), s))
               }
            -> OptoM m v a

type Opto s = OptoM (ST s)

fromStateless
    :: Functor m
    => Scaling c a
    => (a -> m (c, Diff a))
    -> OptoM m v a
fromStateless update =
    MkOptoM { oInit   = ()
            , oUpdate = \x s -> (,s) <$> update x
            }

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

pureSampling
    :: MonadSample r m
    => (r -> a -> Diff a)
    -> Grad m a
pureSampling f = sampling (pureGrad . f)
