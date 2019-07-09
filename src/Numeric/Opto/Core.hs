{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Numeric.Opto.Core (
    Diff, Grad, Opto(..)
  , fromCopying, fromStateless
  , pureGrad
  , nonSampling, pureNonSampling
  ) where

import           Control.Monad.Primitive
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Type.ZipProd
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

type Diff     a = a
type Grad m r a = r -> a -> m (Diff a)

data Opto :: (Type -> Type) -> Type -> Type -> Type -> Type where
    MkOpto :: ScalingInPlace m v c a
           => { oInit   :: !( RefVals m ss sVars )
              , oUpdate :: !( RefVars m ss sVars
                           -> r
                           -> a
                           -> m (c, Diff a)
                            )
              }
           -> Opto m v r a

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (r -> a -> s -> m (c, Diff a, s))
    -> Opto m v r a
fromCopying s0 update =
    MkOpto { oInit   = onlyZP (RVl s0)
            , oUpdate = \(headZP->RVr rS) r x -> do
                (c, g, s) <- update r x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromStateless
    :: ScalingInPlace m v c a
    => (r -> a -> m (c, Diff a))
    -> Opto m v r a
fromStateless update =
    MkOpto { oInit   = ZPØ
            , oUpdate = \_ -> update
            }

pureGrad
    :: Applicative m
    => (r -> a -> Diff a)
    -> Grad m r a
pureGrad f r = pure . f r

nonSampling
    :: (a -> m (Diff a))
    -> Grad m r a
nonSampling f _ = f

pureNonSampling
    :: Applicative m
    => (a -> Diff a)
    -> Grad m r a
pureNonSampling f _ = pure . f
