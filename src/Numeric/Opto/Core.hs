{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Numeric.Opto.Core (
    Diff, Grad, OptoM(..), Opto
  , fromCopying, fromPure, fromStateless, fromStatelessM
  , iterateOptoM, iterateOpto
  , GradSample
  , iterateSamplingUntil
  , iterateSampling
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Sample
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Type.ZipProd
import           Numeric.Opto.Ref
import           Numeric.Opto.Update
import           Unsafe.Coerce

type Diff   a = a
type Grad m a = a -> m (Diff a)

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: ScalingInPlace m v c a
            => { oInit   :: !( RefInits m ss sVars )
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
    MkOptoM { oInit   = onlyZP (RI s0)
            , oUpdate = \gr (headZP->RV rS) x -> do
                (c, g, s) <- update gr x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromPure
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (Grad m a -> a -> s -> (c, Diff a, s))
    -> OptoM m v a
fromPure s0 update =
    MkOptoM { oInit   = onlyZP (RI s0)
            , oUpdate = \gr (headZP->RV rS) x -> atomicModifyMutVar' rS $ \s ->
                  let (c, g, s') = update gr x s
                  in  (s', (c, g))
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

iterateOptoM
    :: forall m v a. Monad m
    => Grad m a                     -- ^ Gradient
    -> (Diff a -> a -> m Bool)      -- ^ Stopping condition
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateOptoM gr stop x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT . many $ do
      (x, step) <- lift $ do
        x <- readRef rX
        (c, g) <- update rSs x
        rX .*+= (c, g)
        return (x, c .* g)
      guard . not =<< lift (stop step x)
    s <- pullRefs rSs
    let o' = MkOptoM s oUpdate
    (, o') <$> readRef rX
  where
    update = oUpdate gr

iterateOpto
    :: (a -> Diff a)                -- ^ Gradient
    -> (Diff a -> a -> Bool)        -- ^ Stopping condition
    -> a
    -> (forall s'. Opto s' v a)
    -> (a, Opto s v a)
iterateOpto gr stop y0 o0 = runST $ do
    (y', o') <- iterateOptoM (pure . gr) (\st -> pure . stop st) y0 o0
    return (y', unsafeCoerce o')        -- is this safe?  probably.

type GradSample m r a = r -> Grad m a

sampling
    :: MonadSample r m
    => GradSample m r a
    -> Grad m a
sampling f x = do
    r <- sample
    f r x

iterateSamplingUntil
    :: forall m v r a. MonadSample r m
    => GradSample m r a              -- ^ Gradient
    -> (Diff a -> a -> m Bool)       -- ^ Stopping condition
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateSamplingUntil gr stop x0 MkOptoM{..} = do
    rS <- initRefs oInit
    rX <- newRef @_ @a @v x0
    _ <- many $ do
      y      <- readRef rX
      (c, g) <- update rS =<< readRef rX
      rX .*+= (c, g)
      guard . not =<< stop (c .* g) y
    s <- pullRefs rS
    let o' = MkOptoM s oUpdate
    (, o') <$> readRef rX
  where
    update = oUpdate (sampling gr)

iterateSampling
    :: MonadSample r m
    => GradSample m r a
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateSampling gr = iterateSamplingUntil gr (\_ _ -> pure False)
