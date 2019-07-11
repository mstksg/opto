{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

import           Control.Concurrent hiding                    (yield)
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens hiding                          ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.Trans.Maybe
import           Data.Bitraversable
import           Data.Conduit
import           Data.Default
import           Data.IDX
import           Data.Primitive.MutVar
import           Data.Time
import           Data.Traversable
import           Data.Tuple
import           GHC.Generics                                 (Generic)
import           GHC.TypeLits
import           Numeric.Backprop hiding                      (auto)
import           Numeric.LinearAlgebra.Static.Backprop hiding ((<>))
import           Numeric.OneLiner
import           Numeric.Opto hiding                          ((<.>))
import           Numeric.Opto.Backprop
import           Options.Applicative
import           System.FilePath hiding                       ((<.>))
import           Text.Printf
import qualified Data.Conduit.Combinators                     as C
import qualified Data.Text                                    as T
import qualified Data.Vector.Generic                          as VG
import qualified Numeric.LinearAlgebra                        as HM
import qualified Numeric.LinearAlgebra.Static                 as H
import qualified System.Random.MWC                            as MWC

data OMode = OSingle
           | OParallel Bool Int

data Opts = Opts
    { oDataDir :: FilePath
    , oReport  :: Int
    , oBatch   :: Int
    , oMode    :: OMode
    }

parseOMode :: Parser OMode
parseOMode = subparser
    ( command "single" (info (pure OSingle) (progDesc "Single-threaded"))
   <> command "parallel" (info (uncurry OParallel <$> parseParallel) (progDesc "Parallel"))
    )
  where
    parseParallel :: Parser (Bool, Int)
    parseParallel = (,)
        <$> switch (long "chunked" <> help "Chunked mode")
        <*> option auto
                ( long "split"
               <> short 's'
               <> help "Number of items per thread"
               <> metavar "INT"
               <> showDefault
               <> value 750
                )

parseOpts :: Parser Opts
parseOpts = Opts
    <$> strArgument ( help "Data directory (containing uncompressed MNIST data set)"
                   <> metavar "DIR"
                    )
    <*> option auto
          ( long "report"
         <> short 'r'
         <> help "Report frequency (in batches)"
         <> metavar "INT"
         <> showDefault
         <> value 2500
          )
    <*> option auto
          ( long "batch"
         <> short 'b'
         <> help "Batching amount"
         <> metavar "INT"
         <> showDefault
         <> value 1
          )
    <*> (parseOMode <|> pure OSingle)

data Net = N { _weights1 :: !(L 250 784)
             , _bias1    :: !(R 250)
             , _weights2 :: !(L 10 250)
             , _bias2    :: !(R 10)
             }
  deriving (Generic)
  deriving (Num, Fractional, Floating) via (GNum Net)
makeLenses ''Net

instance Linear Double Net
instance Ref m Net v => LinearInPlace m v Double Net

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

softMax
    :: Reifies s W
    => BVar s (R 10)
    -> BVar s (R 10)
softMax x = expx / konst (norm_1V expx)
  where
    expx = exp x

crossEntropy
    :: Reifies s W
    => BVar s (R 10)
    -> BVar s (R 10)
    -> BVar s Double
crossEntropy targ res = -(log res <.> targ)

runNet
    :: Reifies s W
    => BVar s Net
    -> BVar s (R 784)
    -> BVar s (R 10)
runNet n x = z
  where
    y = logistic $ (n ^^. weights1) #> x + (n ^^. bias1)
    z = softMax  $ (n ^^. weights2) #> y + (n ^^. bias2)

netErr
    :: Reifies s W
    => BVar s (R 784)
    -> BVar s (R 10)
    -> BVar s Net
    -> BVar s Double
netErr x targ n = crossEntropy targ (runNet n x)

-- *********************************************
-- Plumbing for running the network on real data
-- *********************************************

main :: IO ()
main = MWC.withSystemRandom $ \g -> do
    Opts{..} <- execParser $ info (parseOpts <**> helper)
        ( fullDesc
       <> progDesc "Run optimizer samples on MNIST"
       <> header "opto-neural - opto runner on MNIST data set"
        )

    Just train <- loadMNIST (oDataDir </> "train-images-idx3-ubyte")
                            (oDataDir </> "train-labels-idx1-ubyte")
    Just test  <- loadMNIST (oDataDir </> "t10k-images-idx3-ubyte")
                            (oDataDir </> "t10k-labels-idx1-ubyte")
    putStrLn "Loaded data."
    net0 <- MWC.uniformR (-0.5, 0.5) g
    n    <- getNumCapabilities
    sampleQueue <- atomically $ newTBQueue 25000

    let o :: PrimMonad m => Opto m (MutVar (PrimState m) Net) (R 784, R 10) Net
        o = adam @_ @(MutVar _ Net) def
               (bpGradSample $ \(x, y) -> netErr (constVar x) (constVar y))

        ro = def { roBatch = oBatch }

        report b = do
          yield $ printf "(Batch %d)\n" (b :: Int)
          t0   <- liftIO getCurrentTime
          _    <- liftIO . atomically $ flushTBQueue sampleQueue
          net' <- mapM (liftIO . evaluate . force) =<< await
          chnk <- liftIO . atomically $ flushTBQueue sampleQueue
          t1   <- liftIO getCurrentTime
          case net' of
            Nothing  -> yield "Done!\n"
            Just net -> do
              yield $ printf "Trained on %d points in %s.\n"
                             (length chnk)
                             (show (t1 `diffUTCTime` t0))
              let trainScore = testNet chnk net
                  testScore  = testNet test net
              yield $ printf "Training error:   %.2f%%\n" ((1 - trainScore) * 100)
              yield $ printf "Validation error: %.2f%%\n" ((1 - testScore ) * 100)

        source = forM_ [0..] (\e -> liftIO (printf "[Epoch %d]\n" (e :: Int))
                                 >> C.yieldMany train .| shuffling g
                             )
              .| C.iterM (atomically . writeTBQueue sampleQueue)

        optimizer = case oMode of
          OSingle -> source
                  .| optoConduit ro net0 o
                  .| forever (C.drop (oReport - 1) *> (mapM_ yield =<< await))
          OParallel c s ->
            let po      = def { poSplit = s }
                skipAmt = max 0 $ (oReport `div` (n * s)) - 1
                source'
                  | c         = optoConduitParChunk ro po net0 o source
                  | otherwise = optoConduitPar      ro po net0 o source
            in  source'
                  .| forever (C.drop skipAmt *> (mapM_ yield =<< await))

    runConduit $ optimizer
              .| mapM_ report [0..]
              .| C.map T.pack
              .| C.encodeUtf8
              .| C.stdout

testNet :: [(R 784, R 10)] -> Net -> Double
testNet xs n = sum (map (uncurry test) xs) / fromIntegral (length xs)
  where
    test x (H.extract->t)
        | HM.maxIndex t == HM.maxIndex (H.extract r) = 1
        | otherwise                                  = 0
      where
        r = evalBP (`runNet` constVar x) n

loadMNIST
    :: FilePath
    -> FilePath
    -> IO (Maybe [(R 784, R 10)])
loadMNIST fpI fpL = runMaybeT $ do
    i <- MaybeT          $ decodeIDXFile       fpI
    l <- MaybeT          $ decodeIDXLabelsFile fpL
    d <- MaybeT . return $ labeledIntData l i
    MaybeT . return $ for d (bitraverse mkImage mkLabel . swap)
  where
    mkImage = H.create . VG.convert . VG.map (\i -> fromIntegral i / 255)
    mkLabel n = H.create $ HM.build 10 (\i -> if round i == n then 1 else 0)

instance KnownNat n => MWC.Variate (R n) where
    uniform g = H.randomVector <$> MWC.uniform g <*> pure H.Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (L m n) where
    uniform g = H.uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance MWC.Variate Net where
    uniform g = N <$> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance NFData Net

instance Backprop Net
