{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoStarIsType           #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

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
    MR(..), ML(..)
  ) where

import           Control.Monad.Primitive
import           Data.Complex
import           Data.Mutable
import           GHC.TypeNats
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Generic.Mutable.Sized   as SMVG
import qualified Data.Vector.Generic.Sized           as SVG
import qualified Data.Vector.Storable                as VS
import qualified Numeric.LinearAlgebra               as HU
import qualified Numeric.LinearAlgebra.Devel         as HU
import qualified Numeric.LinearAlgebra.Static        as H
import qualified Numeric.LinearAlgebra.Static.Vector as H

instance (PrimMonad m, VG.Vector v a) => Mutable m (SVG.Vector v n a) where
    type Ref m (SVG.Vector v n a) = SVG.MVector (VG.Mutable v) n (PrimState m) a
    thawRef         = SVG.thaw
    freezeRef       = SVG.freeze
    copyRef         = SVG.copy
    moveRef         = SMVG.move
    cloneRef        = SMVG.clone
    unsafeThawRef   = SVG.unsafeThaw
    unsafeFreezeRef = SVG.unsafeFreeze

instance (PrimMonad m, HU.Element a) => Mutable m (HU.Matrix a) where
    type Ref m (HU.Matrix a) = HU.STMatrix (PrimState m) a
    thawRef x   = stToPrim $ HU.thawMatrix x
    freezeRef v = stToPrim $ HU.freezeMatrix v
    copyRef v x = stToPrim $ HU.setMatrix v 0 0 x
    moveRef         = undefined
    cloneRef        = undefined
    unsafeThawRef   = undefined
    unsafeFreezeRef = undefined

-- | Mutable ref for hmatrix's statically sized vector types, 'H.R' and
-- 'H.C'.
newtype MR s n a = MR { getMR :: SVG.MVector VS.MVector n s a }

instance (PrimMonad m, KnownNat n) => Mutable m (H.R n) where
    type Ref m (H.R n) = MR (PrimState m) n Double

    thawRef = fmap MR . thawRef . H.rVec
    freezeRef = fmap H.vecR . freezeRef . getMR
    copyRef (MR v) x = copyRef v (H.rVec x)
    moveRef         = undefined
    cloneRef        = undefined
    unsafeThawRef   = undefined
    unsafeFreezeRef = undefined

instance (PrimMonad m, KnownNat n) => Mutable m (H.C n) where
    type Ref m (H.C n) = MR (PrimState m) n (Complex Double)

    thawRef = fmap MR . thawRef . H.cVec
    freezeRef = fmap H.vecC . freezeRef . getMR
    copyRef (MR v) x = copyRef v (H.cVec x)
    moveRef         = undefined
    cloneRef        = undefined
    unsafeThawRef   = undefined
    unsafeFreezeRef = undefined

-- | Mutable ref for hmatrix's statically sized matrix types, 'H.L' and
-- 'H.M'.
newtype ML s n k a = ML { getML :: SVG.MVector VS.MVector (n * k) s a }

instance (PrimMonad m, KnownNat n, KnownNat k) => Mutable m (H.L n k) where
    type Ref m (H.L n k) = ML (PrimState m) n k Double

    thawRef = fmap ML . thawRef . H.lVec
    freezeRef = fmap H.vecL . freezeRef . getML
    copyRef (ML v) x = copyRef v (H.lVec x)
    moveRef         = undefined
    cloneRef        = undefined
    unsafeThawRef   = undefined
    unsafeFreezeRef = undefined

instance (PrimMonad m, KnownNat n, KnownNat k) => Mutable m (H.M n k) where
    type Ref m (H.M n k) = ML (PrimState m) n k (Complex Double)

    thawRef = fmap ML . thawRef . H.mVec
    freezeRef = fmap H.vecM . freezeRef . getML
    copyRef (ML v) x = copyRef v (H.mVec x)
    moveRef         = undefined
    cloneRef        = undefined
    unsafeThawRef   = undefined
    unsafeFreezeRef = undefined
