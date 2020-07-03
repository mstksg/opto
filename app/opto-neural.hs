{-# LANGUAGE AllowAmbiguousTypes   #-}
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

import           Control.DeepSeq
import           Control.Lens hiding                          ((<.>))
import           Control.Monad.Primitive
import           Control.Monad.Trans.Maybe
import           Data.Bitraversable
import           Data.Default
import           Data.IDX
import           Data.Mutable
import           Data.Traversable
import           Data.Tuple
import           GHC.Generics                                 (Generic)
import           GHC.TypeLits
import           Numeric.Backprop hiding                      (auto)
import           Numeric.LinearAlgebra.Static.Backprop hiding ((<>))
import           Numeric.OneLiner
import           Numeric.Opto hiding                          ((<.>))
import           Numeric.Opto.Backprop
import           Numeric.Opto.Run.Simple
import           Options.Applicative
import           System.FilePath hiding                       ((<.>))
import           Text.Printf
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
instance Mutable q Net
instance LinearInPlace q Double Net

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

    let o :: Opto (PrimState IO) (R 784, R 10) Net
        o = adam def $
              bpGradSample $ \(x, y) -> netErr (constVar x) (constVar y)

        runTest chnk net = printf "Error: %.2f%%" ((1 - score) * 100)
          where
            score = testNet chnk net

        ro = def { roBatch = oBatch
                 }
        so = def { soTestSet   = Just test
                 , soEvaluate  = runTest
                 , soSkipSamps = oReport
                 }

    case oMode of
      OSingle       -> simpleRunner so train SOSingle ro net0 o g
      OParallel c s -> do
        let po = def { poSplit = s }
        if c
          then simpleRunner so train (SOParallel   po) ro net0 o g
          else simpleRunner so train (SOParChunked po) ro net0 o g

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
