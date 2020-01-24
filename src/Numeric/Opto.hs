-- |
-- Module      : Numeric.Opto
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Main library entrypoint; essentially re-exports all of the submodules to
-- provide full functionality with a single import.
module Numeric.Opto (
  -- * Optimizer
    Opto, mapSample
  -- * Gradients
  , Diff, Grad
  , pureGrad
  , nonSampling, pureNonSampling
  -- * Optimizers
  , steepestDescent
  , Momentum(..), momentum
  , Nesterov(..), nesterov
  , Adagrad(..), adagrad
  , Adadelta(..), adadelta
  , RMSProp(..), rmsProp
  , Adam(..), adam
  , AdaMax(..), adaMax
  -- * Running
  , RunOpts(..), hoistRunOpts
  , ParallelOpts(..), hoistParallelOpts
  -- ** Single-threaded
  , opto
  , opto'
  , optoNonSampling'
  , optoNonSampling
  -- *** Sampling methods
  , optoConduit, optoConduit'
  , optoFold, optoFold'
  -- ** Parallel
  , optoPar
  , optoParChunk
  , optoParNonSampling
  -- *** Sampling Methods
  , optoConduitPar
  , optoConduitParChunk
  -- * Classes
  -- ** Numeric
  , Linear(..), sumLinear, gAdd, gZeroL, gScale
  , Metric(..), gDot, gNorm_inf, gNorm_0, gNorm_1, gNorm_2, gQuadrance
  , LinearInPlace(..), sumLinearInPlace
  -- ** Mutation
  , Mutable(..)
  , MutRef(..)
  , RefFor(..)
  -- *** Instances
  , GMutable, GRef(..), gThawRef, gFreezeRef, gCopyRef
  , RecRef(..)
  , MR(..), ML(..)
  ) where

    -- module Opto

import           Numeric.Opto.Core        as Opto
import           Numeric.Opto.Optimizer   as Opto
import           Numeric.Opto.Ref         as Opto
import           Numeric.Opto.Run         as Opto
import           Numeric.Opto.Run.Conduit as Opto
import           Numeric.Opto.Update      as Opto
