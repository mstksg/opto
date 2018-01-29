{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Sample (
    MonadSample(..)
  , SampleRef, runSampleRef, foldSampleRef, sampleRef
  , SampleFoldT, foldSampleFoldT, sampleFoldT
  , SampleFold, foldSampleFold, sampleFold
  , Batching(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List.Split
import           Data.Profunctor
import           Data.Proxy
import           GHC.TypeLits
import           Numeric.Opto.Ref

class MonadPlus m => MonadSample r m | m -> r where
    sample  :: m r
    sampleN :: Int -> m [r]

newtype SampleRef v r m a = SampleRef { sampleRefReader :: MaybeT (ReaderT v m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             )

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

type SampleFold r = SampleFoldT r Identity

foldSampleFold :: Foldable t => SampleFold r a -> t r -> (Maybe a, [r])
foldSampleFold sf = runIdentity . foldSampleFoldT sf

sampleFold :: Monad m => ([r] -> (Maybe a, [r])) -> SampleFoldT r m a
sampleFold = SampleFoldT . MaybeT . StateT . fmap return

instance Monad m =>  MonadSample r (SampleFoldT r m) where
    sample = sampleFold $ \case []   -> (Nothing, [])
                                x:xs -> (Just x , xs)
    sampleN n = sampleFold $
      first (mfilter (not . null) . Just) . splitAt n

newtype Batching (n :: Nat) m a = Batching { runBatching :: m a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             )

instance (KnownNat n, MonadSample r m) => MonadSample [r] (Batching n m) where
    sample    = Batching $ sampleN (fromIntegral (natVal (Proxy @n)))
    sampleN m = Batching $ chunksOf n <$> sampleN (n * m)
      where
        n = fromIntegral $ natVal (Proxy @n)
