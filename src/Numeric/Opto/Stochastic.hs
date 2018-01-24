{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Numeric.Opto.Stochastic (
    Sample(..)
  , Sampling(..), runSampling, sampling
  , StochM, Stoch
  , fromCopying
  , fromPure
  , iterateStochUntilM
  , iterateStochM
  , sgdM, sgd
  , adam, adamM
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Default
import           Data.Primitive.MutVar
import           Numeric.Opto.Core            (OptoM(..), Step)
import           Numeric.Opto.Ref
import           Numeric.Opto.Step
import qualified Numeric.Opto.Core            as OC

class MonadPlus m => Sample m src a | src -> a where
    sample :: src -> m a

newtype Sampling src r m a = Sampling { samplingReader :: ReaderT src m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , PrimMonad
             , Alternative
             )

-- instance {-# OVERLAPPING #-} Ref m a v => Ref (Sampling r m) a v where
-- instance {-# OVERLAPS #-} AdditiveInPlace m v a => AdditiveInPlace (Sampling r m) v a
-- instance {-# OVERLAPS #-} ScalingInPlace m v c a => ScalingInPlace (Sampling r m) v c a

runSampling :: Sampling src r m a -> src -> m a
runSampling = runReaderT . samplingReader

sampling :: (src -> m a) -> Sampling src r m a
sampling = Sampling . ReaderT

withSample :: Sample m src r => (r -> m a) -> Sampling src r m a
withSample f = sampling $ f <=< sample

type StochM m src r = OptoM (Sampling src r m)
type Stoch  s src r = OptoM (Sampling src r (ST s))

fromCopying
    :: (PrimMonad m, ScalingInPlace (Sampling src r m) v c a, Sample m src r)
    => s
    -> (r -> a -> s -> m (c, Step a, s))
    -> StochM m src r v a
fromCopying s0 update = OC.fromCopying s0 $ \x s -> sampling $ \src -> do
    r <- sample src
    update r x s

fromPure
    :: (PrimMonad m, ScalingInPlace (Sampling src r m) v c a, Sample m src r)
    => s
    -> (r -> a -> s -> (c, Step a, s))
    -> StochM m src r v a
fromPure s0 update = fromCopying s0 (\r x -> pure . update r x)

iterateStochUntilM
    :: forall m src v r a. (Monad m, Sample m src r)
    => src
    -> (Step a -> a -> m Bool)       -- ^ step, current
    -> a
    -> StochM m src r v a
    -> m (a, StochM m src r v a)
iterateStochUntilM src stop x0 MkOptoM{..} = flip runSampling src $ do
    rS <- newRef oInit
    rX <- newRef @_ @a @v x0
    _ <- many $ do
      y      <- readRef rX
      (c, g) <- oUpdate rS =<< readRef rX
      rX .*+= (c, g)
      guard . not =<< lift (stop (c .* g) y)
    s <- readRef rS
    let o' = MkOptoM s oUpdate
    (, o') <$> readRef rX

-- scanOptoUntil
--     :: src
--     -> (Step a -> a -> Bool)         -- ^ step, current
--     -> a
--     -> (forall s'. Stoch s' src r v a)
--     -> (a, Stoch s src r v a)
-- scanOptoUntil src stop y0 o0 = runST $ do
--     (y', o') <- scanOptoUntilM xs (\s -> pure . stop s) y0 o0
--     return (y', unsafeCoerce o')        -- is this safe?  probably.

iterateStochM
    :: (Sample m src r)
    => src
    -> a
    -> StochM m src r v a
    -> m (a, StochM m src r v a)
iterateStochM src = iterateStochUntilM src (\_ _ -> pure False)

-- scanOpto
--     :: Foldable t
--     => t r
--     -> a
--     -> (forall s'. Opto s' v r a)
--     -> (a, Opto s v r a)
-- scanOpto xs = scanOptoUntil xs (\_ _ -> False)

-- iterateOptoM
--     :: Monad m
--     => (Step a -> a -> m Bool)   -- ^ step, current
--     -> a
--     -> OptoM m v () a
--     -> m (a, OptoM m v () a)
-- iterateOptoM = scanOptoUntilM (repeat ())

-- iterateOpto
--     :: (Step a -> a -> Bool)   -- ^ step, current
--     -> a
--     -> (forall s'. Opto s' v () a)
--     -> (a, Opto s v () a)
-- iterateOpto = scanOptoUntil (repeat ())

sgdM
    :: (Ref m a v, ScalingInPlace (Sampling src r m) v c a, Sample m src r)
    => c
    -> (r -> a -> m a)          -- ^ gradient
    -> StochM m src r v a
sgdM lr gr =
    MkOptoM { oInit   = EmptyRef
            , oUpdate = \case ~EmptyRef -> \x -> withSample $ \r ->
                                (-lr,) <$> gr r x
            }

sgd
    :: (Ref m a v, ScalingInPlace (Sampling src r m) v c a, Sample m src r)
    => c
    -> (r -> a -> a)
    -> StochM m src r v a
sgd lr gr = sgdM lr (\x -> pure . gr x)

data Adam c = Adam
    { adamStep    :: !c
    , adamDecay1  :: !c
    , adamDecay2  :: !c
    , adamEpsilon :: !c
    }
  deriving (Show, Eq)

instance Fractional a => Default (Adam a) where
    def = Adam { adamStep    = 0.001
               , adamDecay1  = 0.9
               , adamDecay2  = 0.999
               , adamEpsilon = 1e-8
               }

adamM
    :: forall m src r v a c.
     ( RealFloat c
     , Floating a
     , ScalingInPlace (Sampling src r m) v c a
     , PrimMonad m
     , Sample m src r
     )
    => Adam c
    -> (r -> a -> m a)          -- ^ gradient
    -> StochM m src r v a
adamM Adam{..} gr =
    MkOptoM { oInit   = (scaleOne @_ @a, addZero @a, addZero @a)
            , oUpdate = \rS x -> withSample $ \r -> do
                g <- gr r x
                (t, m0, v0) <- readMutVar rS
                let m1 = adamDecay1 .* m0 .+. (1 - adamDecay1) .* g
                    v1 = adamDecay2 .* v0 .+. (1 - adamDecay2) .* g
                    mHat = recip (1 - adamDecay1 ** t) .* m1
                    vHat = recip (1 - adamDecay2 ** t) .* v1
                writeMutVar rS (t + 1, m1, v1)
                return ( -adamStep
                       , mHat / (sqrt vHat + realToFrac adamEpsilon)
                       )
            }

adam
    :: forall m src r v a c.
     ( RealFloat c
     , Floating a
     , ScalingInPlace (Sampling src r m) v c a
     , PrimMonad m
     , Sample m src r
     )
    => Adam c
    -> (r -> a -> a)          -- ^ gradient
    -> StochM m src r v a
adam a gr = adamM a (\x -> pure . gr x)
