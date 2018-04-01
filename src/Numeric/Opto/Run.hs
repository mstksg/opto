{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run (
    RunOpts(.., RO')
  , runOptoMany, evalOptoMany
  , runOpto, evalOpto
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update

data RunOpts m a = RO { roStopCond :: Diff a -> a -> m Bool
                      , roLimit    :: Maybe Int
                      , roBatch    :: Maybe Int
                      }

-- | Construct a 'RunOpts' with no stopping condition
pattern RO' :: Applicative m => Maybe Int -> Maybe Int -> RunOpts m a
pattern RO' lim bt <- RO _ lim bt where
    RO' = RO (\_ _ -> pure False)

runOptoMany
    :: forall m v a. Alternative m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
runOptoMany RO{..} x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    optoLoop roLimit roBatch
        (newRef @m @a @v)
        (.*+=)
        readRef
        rX
        (oUpdate rSs)
        roStopCond
    o' <- flip MkOptoM oUpdate <$> pullRefs rSs
    (, o') <$> readRef rX

evalOptoMany
    :: forall m v a. Alternative m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m a
evalOptoMany ro x0 = fmap fst . runOptoMany ro x0

runOpto
    :: forall m v a. Monad m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m (a, OptoM m v a)
runOpto RO{..} x0 MkOptoM{..} = do
    rSs <- initRefs oInit
    rX <- newRef @m @a @v x0
    _ <- runMaybeT $ optoLoop roLimit roBatch
            (lift . newRef @m @a @v)
            (\v -> lift . (v .*+=))
            (lift . readRef)
            rX
            (lift . oUpdate rSs)
            (\d -> lift . roStopCond d)
    o' <- flip MkOptoM oUpdate <$> pullRefs rSs
    (, o') <$> readRef rX
  where

evalOpto
    :: forall m v a. Monad m
    => RunOpts m a
    -> a
    -> OptoM m v a
    -> m a
evalOpto ro x0 = fmap fst . runOpto ro x0

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
