{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run (
    RunOpts(.., RO')
  , RunOpto, EvalOpto
  , runOptoMany, evalOptoMany
  , runOpto, evalOpto
  , iterateRunOpto, iterateEvalOpto
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

type RunOpto m v a  = RunOpts m a -> a -> OptoM m v a -> m (a, OptoM m v a)
type EvalOpto m v a = RunOpts m a -> a -> OptoM m v a -> m a

data RunOpts m a = RO { roGrad     :: Grad m a
                      , roStopCond :: Diff a -> a -> m Bool
                      , roLimit    :: Maybe Int
                      , roBatch    :: Maybe Int
                      }

-- | Construct a 'RunOpts' with no stopping condition
pattern RO' :: Applicative m => Grad m a -> Maybe Int -> Maybe Int -> RunOpts m a
pattern RO' gr lim bt <- RO gr _ lim bt where
    RO' gr = RO gr (\_ _ -> pure False)

runOptoMany :: forall m v a. Alternative m => RunOpto m v a
runOptoMany RO{..} x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    optoLoop roLimit roBatch
        (newRef @m @a @v)
        (.*+=)
        readRef
        rX
        (update rSs)
        roStopCond
    o' <- flip MkOptoM oUpdate <$> pullRefs rSs
    (, o') <$> readRef rX
  where
    update = oUpdate roGrad

evalOptoMany :: forall m v a. Alternative m => EvalOpto m v a
evalOptoMany ro x0 = fmap fst . runOptoMany ro x0

runOpto :: forall m v a. Monad m => RunOpto m v a
runOpto RO{..} x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT $ optoLoop roLimit roBatch
            (lift . newRef @m @a @v)
            (\v -> lift . (v .*+=))
            (lift . readRef)
            rX
            (lift . update rSs)
            (\d -> lift . roStopCond d)
    o' <- flip MkOptoM oUpdate <$> pullRefs rSs
    (, o') <$> readRef rX
  where
    update = oUpdate roGrad

evalOpto :: forall m v a. Monad m => EvalOpto m v a
evalOpto ro x0 = fmap fst . runOpto ro x0

iterateRunOpto
    :: Monad m
    => RunOpto m v a
    -> (Int -> m Bool)        -- ^ callback, with iteration # and whether or not to continue.  run first
    -> RunOpto m v a
iterateRunOpto ro f ropts = go 0
  where
    go !n !x !o = do
      c <- f n
      if c
        then pure (x, o)
        else do
          (x', o') <- ro ropts x o
          go (n + 1) x' o'

iterateEvalOpto
    :: Monad m
    => EvalOpto m v a
    -> (Int -> m Bool)        -- ^ callback, with iteration # and whether or not to continue.  run first
    -> EvalOpto m v a
iterateEvalOpto ro f ropts x0 o = go 0 x0
  where
    go !n !x = do
      c <- f n
      if c
        then pure x
        else do
          x' <- ro ropts x o
          go (n + 1) x'


-- type RunOpto m v a  = RunOpts m a -> a -> OptoM m v a -> m (a, OptoM m v a)

optoLoop
    :: forall m v a c. (Alternative m, Monad m, Scaling c a)
    => Maybe Int
    -> Maybe Int
    -> (a -> m v)
    -> (v -> (c, a) -> m ())
    -> (v -> m a)
    -> v
    -> (a -> m (c, a))
    -> (Diff a -> a -> m Bool)
    -> m ()
optoLoop lim batch initXVar updateXVar readXVar xVar updateState stop = repeatLim $ do
    x <- readXVar xVar
    (c, g) <- repeatBatch x
    updateXVar xVar (c, g)
    guard . not =<< stop (c .* g) x
  where
    repeatLim = case lim of
      Nothing -> void . many
      Just l  -> replicateM_ l
    repeatBatch x = case batch of
      Nothing -> updateState x
      Just b  -> do
        v <- initXVar addZero
        -- TODO: should state be reset?
        replicateM_ b $
          updateXVar v =<< updateState x
        (scaleOne @c @a,) <$> readXVar v
