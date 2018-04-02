{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

import           Control.DeepSeq
import           Control.Exception
import           Control.Lens hiding                          ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
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
import           Numeric.Backprop
import           Numeric.LinearAlgebra.Static.Backprop hiding ((&))
import           Numeric.OneLiner
import           Numeric.Opto hiding                          ((<.>))
import           System.Environment
import           System.FilePath hiding                       ((<.>))
import           Text.Printf
import qualified Data.Conduit.Combinators                     as C
import qualified Data.Text                                    as T
import qualified Data.Vector.Generic                          as VG
import qualified Numeric.LinearAlgebra                        as HM
import qualified Numeric.LinearAlgebra.Static                 as H
import qualified System.Random.MWC                            as MWC

data Net = N { _weights1 :: !(L 250 784)
             , _bias1    :: !(R 250)
             , _weights2 :: !(L 10 250)
             , _bias2    :: !(R 10)
             }
  deriving (Generic)
makeLenses ''Net

instance Additive Net
instance Scaling Double Net where
    x .* n = realToFrac x * n
instance Ref m Net v => AdditiveInPlace m v Net
instance Ref m Net v => ScalingInPlace m v Double Net

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
    datadir:_ <- getArgs
    Just train <- loadMNIST (datadir </> "train-images-idx3-ubyte")
                            (datadir </> "train-labels-idx1-ubyte")
    Just test  <- loadMNIST (datadir </> "t10k-images-idx3-ubyte")
                            (datadir </> "t10k-labels-idx1-ubyte")
    putStrLn "Loaded data."
    net0 <- MWC.uniformR (-0.5, 0.5) g
    let report n b = do
          yield $ printf "(Batch %d)\n" (b :: Int)
          t0 <- liftIO getCurrentTime
          C.drop (n - 1)
          net' <- mapM (liftIO . evaluate . force) =<< await
          t1 <- liftIO getCurrentTime
          case net' of
            Nothing  -> yield "Done!\n"
            Just net -> do
              chnk <- lift . state $ (,[])
              yield $ printf "Trained on %d points in %s.\n"
                             (length chnk)
                             (show (t1 `diffUTCTime` t0))
              let trainScore = testNet chnk net
                  testScore  = testNet test net
              yield $ printf "Training error:   %.2f%%\n" ((1 - trainScore) * 100)
              yield $ printf "Validation error: %.2f%%\n" ((1 - testScore ) * 100)

    flip evalStateT []
        . runConduit
        $ forM_ [0..] (\e -> liftIO (printf "[Epoch %d]\n" (e :: Int))
                          >> C.yieldMany train .| shuffling g
                      )
       .| C.iterM (modify . (:))      -- add to state stack for train eval
       .| void (runOptoConduit o net0 (adam @_ @(MutVar _ Net) def (pureSampling gr)))
       .| mapM_ (report 2500) [0..]
       .| C.map T.pack
       .| C.encodeUtf8
       .| C.stdout
  where
    gr (x, y) = gradBP (netErr (constVar x) (constVar y))
    o = RO' Nothing Nothing

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

instance Num Net where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum
    fromInteger = gFromInteger

instance Fractional Net where
    (/)          = gDivide
    recip        = gRecip
    fromRational = gFromRational

instance Floating Net where
    pi = gPi
    sqrt = gSqrt
    exp = gExp
    log = gLog
    sin = gSin
    cos = gCos
    asin = gAsin
    acos = gAcos
    atan = gAtan
    sinh = gSinh
    cosh = gCosh
    asinh = gAsinh
    acosh = gAcosh
    atanh = gAtanh

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
