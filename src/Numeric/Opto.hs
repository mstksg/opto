{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Numeric.Opto (
    module Opto
  ) where

import           Control.Monad.Sample    as Opto
import           Numeric.Opto.Algorithms as Opto
import           Numeric.Opto.Core       as Opto
import           Numeric.Opto.Ref        as Opto
import           Numeric.Opto.Run        as Opto
import           Numeric.Opto.Update     as Opto
