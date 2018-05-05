{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Opto.Run (
  -- * Single-threaded
    RunOpts(.., RO')
  , runOptoMany, evalOptoMany
  , runOpto, evalOpto
  -- * Parallel
  , ParallelOpts(..)
  , runOptoManyParallel
  , runOptoParallel
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Maybe
import           Numeric.Opto.Core
import           Numeric.Opto.Ref
import           Numeric.Opto.Update
import           UnliftIO.Async
import           UnliftIO.Concurrent
import           UnliftIO.IORef

data RunOpts m a = RO { roStopCond :: Diff a -> a -> m Bool
                      , roLimit    :: Maybe Int
                      , roBatch    :: Maybe Int
                      }

data ParallelOpts = PO { poThreads   :: Maybe Int
                       , poSplitRuns :: Int         -- ^ how much each thread will process before regrouping
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

mean :: (Foldable t, Fractional a) => t a -> a
mean = go . foldMap (`Sum2` 1)
  where
    go (Sum2 x n) = x / fromInteger n

data Sum2 a b = Sum2 !a !b

instance (Num a, Num b) => Semigroup (Sum2 a b) where
    Sum2 x1 y1 <> Sum2 x2 y2 = Sum2 (x1 + x2) (y1 + y2)

instance (Num a, Num b) => Monoid (Sum2 a b) where
    mappend = (<>)
    mempty = Sum2 0 0

runOptoManyParallel
    :: (MonadUnliftIO m, MonadPlus m, Fractional a)
    => RunOpts m a
    -> ParallelOpts
    -> a
    -> OptoM m v a
    -> m (a, [OptoM m v a])
runOptoManyParallel ro PO{..} x0 o0 = do
    n <- maybe getNumCapabilities pure poThreads
    hitStop <- newIORef Nothing
    let os0 = replicate n o0
    fmap fst . flip execStateT ((x0, os0), 0) . many . StateT $ \((x,os), i) -> do
      maybe (pure ()) (const empty) =<< readIORef hitStop
      m <- case roLimit ro of
        Nothing -> pure poSplitRuns
        Just l  -> do
          guard $ i < l
          pure $ if i + poSplitRuns <= l
                   then poSplitRuns
                   else l - i
      (xs, os') <- fmap unzip . forConcurrently os $ \o ->
        runOptoMany (ro { roLimit    = Just m
                        , roStopCond = \d y -> do
                            s <- roStopCond ro d y
                            when s $ writeIORef hitStop $ Just y
                            return s
                        }
                    ) x o
      newX <- fromMaybe (mean xs) <$> readIORef hitStop
      return ((), ((newX, os'), i + m))

runOptoParallel
    :: (MonadUnliftIO m, Fractional a)
    => RunOpts m a
    -> ParallelOpts
    -> a
    -> OptoM m v a
    -> m (a, [OptoM m v a])
runOptoParallel ro PO{..} x0 o0 = do
    n <- maybe getNumCapabilities pure poThreads
    hitStop <- newIORef Nothing
    let os0 = replicate n o0
    fmap (maybe (x0, os0) fst)
        . runMaybeT
        . flip execStateT ((x0, os0), 0)
        . many
        . StateT $ \((x,os), i) -> do
      maybe (pure ()) (const empty) =<< readIORef hitStop
      m <- case roLimit ro of
        Nothing -> pure poSplitRuns
        Just l  -> do
          guard $ i < l
          pure $ if i + poSplitRuns <= l
                   then poSplitRuns
                   else l - i
      (xs, os') <- lift . fmap unzip . forConcurrently os $ \o ->
        runOpto (ro { roLimit    = Just m
                    , roStopCond = \d y -> do
                        s <- roStopCond ro d y
                        when s $ writeIORef hitStop $ Just y
                        return s
                    }
                ) x o
      newX <- fromMaybe (mean xs) <$> readIORef hitStop
      return ((), ((newX, os'), i + m))

