{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Opto.Stochastic (
    MonadSample(..)
  , fromCopying, fromPure
  , fromStateless, fromStatelessM
  , iterateStochUntilM
  , iterateStochM
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Type.ZipProd
import           Numeric.Opto.Core       (OptoM(..), Step)
import           Numeric.Opto.Ref
import           Numeric.Opto.Sample
import           Numeric.Opto.Update
import qualified Numeric.Opto.Core       as OC

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a, MonadSample r m)
    => s
    -> (r -> a -> s -> m (c, Step a, s))
    -> OptoM m v a
fromCopying s0 update = OC.fromCopying s0 $ \x s -> do
    r <- sample
    update r x s

fromPure
    :: (PrimMonad m, ScalingInPlace m v c a, MonadSample r m)
    => s
    -> (r -> a -> s -> (c, Step a, s))
    -> OptoM m v a
fromPure s0 update = fromCopying s0 (\r x -> pure . update r x)

fromStatelessM
    :: (ScalingInPlace m v c a, MonadSample r m)
    => (r -> a -> m (c, Step a))
    -> OptoM m v a
fromStatelessM update =
    MkOptoM { oInit   = ZPØ
            , oUpdate = \_ x -> do
                r <- sample
                update r x
            }

fromStateless
    :: (ScalingInPlace m v c a, MonadSample r m)
    => (r -> a -> (c, Step a))
    -> OptoM m v a
fromStateless update = fromStatelessM (\r -> pure . update r)

iterateStochUntilM
    :: forall m v r a. (MonadSample r m)
    => (Step a -> a -> m Bool)       -- ^ step, current
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateStochUntilM stop x0 MkOptoM{..} = do
    rS <- initRefs oInit
    rX <- newRef @_ @a @v x0
    _ <- many $ do
      y      <- readRef rX
      (c, g) <- oUpdate rS =<< readRef rX
      rX .*+= (c, g)
      guard . not =<< stop (c .* g) y
    s <- pullRefs rS
    let o' = MkOptoM s oUpdate
    (, o') <$> readRef rX

iterateStochM
    :: MonadSample r m
    => a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateStochM = iterateStochUntilM (\_ _ -> pure False)
