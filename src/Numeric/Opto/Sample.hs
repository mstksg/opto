{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Numeric.Opto.Sample (
    MonadSample(..)
  , SampleRefT, runSampleRefT, sampleRefT
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
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

sampleRefT :: (v -> m (Maybe a)) -> SampleRefT v r m a
sampleRefT = SampleRefT . ReaderT . fmap MaybeT

instance (Monad m, Ref m [r] v) => MonadSample r (SampleRefT v r m) where
    sample = sampleRefT $ \v ->
      updateRef' v $  \case
        []   -> ([], Nothing)
        x:xs -> (xs, Just x )

