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
    module Opto
  ) where

import           Numeric.Opto.Core        as Opto
import           Numeric.Opto.Optimizer   as Opto
import           Numeric.Opto.Ref         as Opto
import           Numeric.Opto.Run         as Opto
import           Numeric.Opto.Run.Conduit as Opto
import           Numeric.Opto.Update      as Opto
