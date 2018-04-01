{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Control.Monad.Sample (
    MonadSample(..), flushSamples
  , SampleRef(.., SampleRef), runSampleRef, foldSampleRef
  , SampleFoldT(.., SampleFoldT), runSampleFoldT, foldSampleFoldT
  , SampleFold, runSampleFold, foldSampleFold, sampleFold
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Conduit
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
    sampleN 0 = pure []
    sampleN n = ((:) <$> sample <*> sampleN (n - 1)) <|> pure []

flushSamples :: MonadSample r m => m [r]
flushSamples = many sample

newtype SampleRef v r m a = SR_ { sampleRefReader :: MaybeT (ReaderT v m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             , MonadIO
             )

instance MonadTrans (SampleRef v r) where
    lift = SR_ . lift . lift

pattern SampleRef :: (v -> m (Maybe a)) -> SampleRef v r m a
pattern SampleRef { runSampleRef } <- (unwrapSR->runSampleRef)
  where
    SampleRef = SR_ . MaybeT . ReaderT

unwrapSR :: SampleRef v r m a -> v -> m (Maybe a)
unwrapSR = runReaderT . runMaybeT . sampleRefReader

foldSampleRef :: (Ref m [r] v, Foldable t) => SampleRef v r m a -> t r -> m (Maybe a, [r])
foldSampleRef sr xs = do
    r <- newRef (toList xs)
    y <- runSampleRef sr r
    (y,) <$> readRef r

instance (Monad m, Ref m [r] v) => MonadSample r (SampleRef v r m) where
    sample = SampleRef $ \v ->
      updateRef' v $ \case
        []   -> ([], Nothing)
        x:xs -> (xs, Just x )
    sampleN n = SampleRef $ \v ->
      updateRef' v (second Just . splitAt n)

newtype SampleFoldT r m a = SFT_ { sampleFoldState :: MaybeT (StateT [r] m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , PrimMonad
             , Alternative
             , MonadPlus
             , MonadIO
             )

instance MonadTrans (SampleFoldT r) where
    lift = SFT_ . lift . lift

pattern SampleFoldT :: ([r] -> m (Maybe a, [r])) -> SampleFoldT r m a
pattern SampleFoldT { runSampleFoldT } <- (runSFT->runSampleFoldT)
  where
    SampleFoldT = SFT_ . MaybeT . StateT

runSFT :: SampleFoldT r m a -> [r] -> m (Maybe a, [r])
runSFT = runStateT . runMaybeT . sampleFoldState

foldSampleFoldT :: Foldable t => SampleFoldT r m a -> t r -> m (Maybe a, [r])
foldSampleFoldT = lmap toList . runSampleFoldT

type SampleFold r = SampleFoldT r Identity

runSampleFold :: SampleFold r a -> [r] -> (Maybe a, [r])
runSampleFold sf = runIdentity . runSampleFoldT sf

foldSampleFold :: Foldable t => SampleFold r a -> t r -> (Maybe a, [r])
foldSampleFold sf = runIdentity . foldSampleFoldT sf

sampleFold :: Monad m => ([r] -> (Maybe a, [r])) -> SampleFoldT r m a
sampleFold = SFT_ . MaybeT . StateT . fmap return

instance Monad m => MonadSample r (SampleFoldT r m) where
    sample = sampleFold $ \case []   -> (Nothing, [])
                                x:xs -> (Just x , xs)
    sampleN n = sampleFold $
      first Just . splitAt n

instance Monad m => MonadSample r (MaybeT (ConduitT r o m)) where
    sample = MaybeT await
