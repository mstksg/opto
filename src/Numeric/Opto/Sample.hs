{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Numeric.Opto.Sample (
    MonadSample(..)
  , SampleRefT, runSampleRefT, foldSampleRefT, sampleRefT
  , SampleFoldT, foldSampleFoldT, sampleFoldT
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Profunctor
import           Numeric.Opto.Ref

class MonadPlus m => MonadSample r m | m -> r where
    sample :: m r

newtype SampleRefT v r m a = SampleRefT { sampleRefReader :: ReaderT v (MaybeT m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             )

runSampleRefT :: SampleRefT v r m a -> v -> m (Maybe a)
runSampleRefT = fmap runMaybeT . runReaderT . sampleRefReader

foldSampleRefT :: (Ref m [r] v, Foldable t) => SampleRefT v r m a -> t r -> m (Maybe a, [r])
foldSampleRefT sr xs = do
    r <- newRef (toList xs)
    y <- runSampleRefT sr r
    (y,) <$> readRef r

sampleRefT :: (v -> m (Maybe a)) -> SampleRefT v r m a
sampleRefT = SampleRefT . ReaderT . fmap MaybeT

instance (Monad m, Ref m [r] v) => MonadSample r (SampleRefT v r m) where
    sample = sampleRefT $ \v ->
      updateRef' v $  \case
        []   -> ([], Nothing)
        x:xs -> (xs, Just x )

newtype SampleFoldT r m a = SampleFoldT { sampleFoldState :: MaybeT (StateT [r] m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             )

foldSampleFoldT :: Foldable t => SampleFoldT r m a -> t r -> m (Maybe a, [r])
foldSampleFoldT = lmap toList . runStateT . runMaybeT . sampleFoldState

sampleFoldT :: ([r] -> m (Maybe a, [r])) -> SampleFoldT r m a
sampleFoldT = SampleFoldT . MaybeT . StateT

