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
    Step, OptoM(..), Opto
  , fromCopying, fromPure, fromStateless, fromStatelessM
  , iterateOptoM, iterateOpto
  , steepestDescent, steepestDescentM
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Type.ZipProd
import           Numeric.Opto.Ref
import           Numeric.Opto.Step
import           Unsafe.Coerce

type Step a = a

data OptoM :: (Type -> Type) -> Type -> Type -> Type where
    MkOptoM :: ScalingInPlace m v c a
            => { oInit   :: !(RefInits m ss sVars)
               , oUpdate :: !(RefVars m ss sVars -> a -> m (c, Step a))
               }
            -> OptoM m v a

type Opto s = OptoM (ST s)

fromCopying
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (a -> s -> m (c, Step a, s))
    -> OptoM m v a
fromCopying s0 update =
    MkOptoM { oInit   = onlyZP (RI s0)
            , oUpdate = \(headZP->RV rS) x -> do
                (c, g, s) <- update x =<< readMutVar rS
                writeMutVar rS s
                return (c, g)
            }

fromPure
    :: (PrimMonad m, ScalingInPlace m v c a)
    => s
    -> (a -> s -> (c, Step a, s))
    -> OptoM m v a
fromPure s0 update = fromCopying s0 (\x -> pure . update x)

fromStatelessM
    :: ScalingInPlace m v c a
    => (a -> m (c, Step a))
    -> OptoM m v a
fromStatelessM update =
    MkOptoM { oInit   = ZPØ
            , oUpdate = const update
            }

fromStateless
    :: ScalingInPlace m v c a
    => (a -> (c, Step a))
    -> OptoM m v a
fromStateless update = fromStatelessM (pure . update)

iterateOptoM
    :: forall m v a. Monad m
    => (Step a -> a -> m Bool)   -- ^ step, current
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
iterateOptoM stop x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT . many $ do
      (x, step) <- lift $ do
        x <- readRef rX
        (c, g) <- oUpdate rSs x
        rX .*+= (c, g)
        return (x, c .* g)
      guard . not =<< lift (stop step x)
    s <- pullRefs rSs
    let o' = MkOptoM s oUpdate
    (, o') <$> readRef rX

iterateOpto
    :: (Step a -> a -> Bool)   -- ^ step, current
    -> a
    -> (forall s'. Opto s' v a)
    -> (a, Opto s v a)
iterateOpto stop y0 o0 = runST $ do
    (y', o') <- iterateOptoM (\st -> pure . stop st) y0 o0
    return (y', unsafeCoerce o')        -- is this safe?  probably.

steepestDescentM
    :: (ScalingInPlace m v c a, Applicative m)
    => c
    -> (a -> m a)           -- ^ gradient
    -> OptoM m v a
steepestDescentM lr gr = fromStatelessM $ fmap (-lr,) . gr

steepestDescent
    :: (ScalingInPlace m v c a, Applicative m)
    => c
    -> (a -> a)             -- ^ gradient
    -> OptoM m v a
steepestDescent lr gr = steepestDescentM lr (pure . gr)
