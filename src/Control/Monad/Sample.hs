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

module Control.Monad.Sample (
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
import           Data.Bifunctor
import           Data.Foldable
import           Data.Profunctor
import           Numeric.Opto.Ref

class MonadPlus m => MonadSample r m | m -> r where
    sample  :: m r
    sampleN :: Int -> m [r]

newtype SampleRefT v r m a = SampleRefT { sampleRefReader :: MaybeT (ReaderT v m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             )

runSampleRefT :: SampleRefT v r m a -> v -> m (Maybe a)
runSampleRefT = runReaderT . runMaybeT . sampleRefReader

foldSampleRefT :: (Ref m [r] v, Foldable t) => SampleRefT v r m a -> t r -> m (Maybe a, [r])
foldSampleRefT sr xs = do
    r <- newRef (toList xs)
    y <- runSampleRefT sr r
    (y,) <$> readRef r

sampleRefT :: (v -> m (Maybe a)) -> SampleRefT v r m a
sampleRefT = SampleRefT . MaybeT . ReaderT

instance (Monad m, Ref m [r] v) => MonadSample r (SampleRefT v r m) where
    sample = sampleRefT $ \v ->
      updateRef' v $  \case
        []   -> ([], Nothing)
        x:xs -> (xs, Just x )
    sampleN n = sampleRefT $ \v ->
      updateRef' v (second (mfilter (not . null) . Just) . splitAt n)

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

instance Monad m =>  MonadSample r (SampleFoldT r m) where
    sample = sampleFoldT . fmap return $ \case []   -> (Nothing, [])
                                               x:xs -> (Just x , xs)
    sampleN n = sampleFoldT . fmap return $
      first (mfilter (not . null) . Just) . splitAt n

