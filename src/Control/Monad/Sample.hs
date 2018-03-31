{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Sample (
    MonadSample(..), flushSamples
  , SampleRef(..), runSampleRef, foldSampleRef, sampleRef
  , SampleFoldT(..), foldSampleFoldT, sampleFoldT
  , SampleFold, foldSampleFold, sampleFold
  , SampleGen(..), runSampleGen, sampleGen
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Profunctor
import           Numeric.Opto.Ref

-- | 'MonadPlus' to imply that an empty pool is empty forever unless you
-- re-fill it first.
class MonadPlus m => MonadSample r m | m -> r where
    sample  :: m r
    -- | Should never fail
    sampleN :: Int -> m [r]

flushSamples :: MonadSample r m => m [r]
flushSamples = many sample

newtype SampleRef v r m a = SampleRef { sampleRefReader :: MaybeT (ReaderT v m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             )

instance MonadTrans (SampleRef v r) where
    lift = SampleRef . lift . lift

runSampleRef :: SampleRef v r m a -> v -> m (Maybe a)
runSampleRef = runReaderT . runMaybeT . sampleRefReader

foldSampleRef :: (Ref m [r] v, Foldable t) => SampleRef v r m a -> t r -> m (Maybe a, [r])
foldSampleRef sr xs = do
    r <- newRef (toList xs)
    y <- runSampleRef sr r
    (y,) <$> readRef r

sampleRef :: (v -> m (Maybe a)) -> SampleRef v r m a
sampleRef = SampleRef . MaybeT . ReaderT

instance (Monad m, Ref m [r] v) => MonadSample r (SampleRef v r m) where
    sample = sampleRef $ \v ->
      updateRef' v $  \case
        []   -> ([], Nothing)
        x:xs -> (xs, Just x )
    sampleN n = sampleRef $ \v ->
      updateRef' v (second Just . splitAt n)

newtype SampleFoldT r m a = SampleFoldT { sampleFoldState :: MaybeT (StateT [r] m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             )

instance MonadTrans (SampleFoldT r) where
    lift = SampleFoldT . lift . lift

foldSampleFoldT :: Foldable t => SampleFoldT r m a -> t r -> m (Maybe a, [r])
foldSampleFoldT = lmap toList . runStateT . runMaybeT . sampleFoldState

sampleFoldT :: ([r] -> m (Maybe a, [r])) -> SampleFoldT r m a
sampleFoldT = SampleFoldT . MaybeT . StateT

type SampleFold r = SampleFoldT r Identity

foldSampleFold :: Foldable t => SampleFold r a -> t r -> (Maybe a, [r])
foldSampleFold sf = runIdentity . foldSampleFoldT sf

sampleFold :: Monad m => ([r] -> (Maybe a, [r])) -> SampleFoldT r m a
sampleFold = SampleFoldT . MaybeT . StateT . fmap return

instance Monad m => MonadSample r (SampleFoldT r m) where
    sample = sampleFold $ \case []   -> (Nothing, [])
                                x:xs -> (Just x , xs)
    sampleN n = sampleFold $
      first Just . splitAt n

newtype SampleGen r m a = SampleGen { sampleGenReader :: MaybeT (ReaderT (m r, Int) (StateT Int m)) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             )

instance MonadTrans (SampleGen r) where
    lift = SampleGen . lift . lift . lift

runSampleGen
    :: Monad m
    => SampleGen r m a
    -> m r                  -- ^ gen
    -> Int                  -- ^ limit
    -> m (Maybe a)
runSampleGen sg g l = flip evalStateT 0
                    . flip runReaderT (g, l)
                    . runMaybeT
                    . sampleGenReader
                    $ sg

sampleGen :: Monad m => (r -> Int -> m (Maybe a, Int)) -> SampleGen r m a
sampleGen f = SampleGen . MaybeT . ReaderT $ \(g, lim) -> StateT $ \i ->
    if i < lim
      then pure (Nothing, i)
      else do
        r <- g
        f r i

-- | 'sample' and 'sampleN' never fail
instance Monad m => MonadSample r (SampleGen r m) where
    sample  = sampleGen $ \r i -> pure (Just r, i + 1)
    sampleN 0 = pure []
    sampleN n = ((:) <$> sample <*> sampleN (n - 1)) <|> pure []

