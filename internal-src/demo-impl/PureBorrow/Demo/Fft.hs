{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureBorrow.Demo.Fft (
  defaultMain,
  defaultMainWith,
  CLIOpts (..),
  optionsP,
) where

import Control.Applicative ((<**>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.DivideConquer.Linear (fftDC)
import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Control.Functor.Linear qualified as Control
import Data.Bits (popCount)
import Data.Complex
import Data.FMList qualified as FML
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear.Borrow qualified as LV
import Data.Vector.Unboxed qualified as U
import Options.Applicative qualified as Opts
import Prelude.Linear qualified as PL
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Random
import Text.Read (readEither)
import Prelude

data CLIOpts = CLIOpts
  { threshold :: !Int
  , size :: !Int
  , seed :: !(Maybe Int)
  , output :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Ord)

optionsP :: Opts.ParserInfo CLIOpts
optionsP = Opts.info (p <**> Opts.helper) $ Opts.progDesc "Parallel FFT"
  where
    p = do
      threshold <-
        Opts.option Opts.auto $
          Opts.short 'w'
            <> Opts.long "threshold"
            <> Opts.value 1024
            <> Opts.showDefault
            <> Opts.help "Threshold to calculate sequentially below this length."
      size <-
        Opts.option power2 $
          Opts.short 'n'
            <> Opts.long "size"
            <> Opts.value kN
            <> Opts.showDefault
            <> Opts.help "Sample Size (must be a power of 2)"
      output <-
        Opts.optional $
          Opts.strOption $
            Opts.short 'o'
              <> Opts.metavar "FILE"
              <> Opts.help "Output TSV path"
      seed <-
        Opts.optional $
          Opts.option Opts.auto $
            Opts.short 's'
              <> Opts.long "seed"
              <> Opts.metavar "INT"
              <> Opts.help "Random seed"
      pure CLIOpts {..}

power2 :: Opts.ReadM Int
power2 = Opts.eitherReader \s ->
  case readEither s of
    Right n
      | n > 0 && popCount n == 1 -> Right n
      | otherwise -> Left $ "Must be a positive power of 2, but got: " <> s
    Left err -> Left err

sample :: Int -> (Double -> Double) -> V.Vector Double
sample n f = V.generate n \i -> f (-4 + 8 * fromIntegral i / fromIntegral n)

-- | Convert to the vector of complex numbers, with real part even element and imaginary part odd.
compress :: V.Vector Double -> V.Vector (Complex Double)
compress v =
  V.generate (V.length v `quot` 2) \i ->
    let re = v V.! (2 * i)
        im = v V.! (2 * i + 1)
     in re :+ im

kN :: Int
kN = 2 ^ (20 :: Int)

fun :: Double -> Double
fun x = sin (2 * pi * x) + 2 * sin (pi * x) + 3 * sin (0.5 * pi * x)

defaultMain :: IO ()
defaultMain = do
  opts <- Opts.execParser optionsP
  defaultMainWith opts

defaultMainWith :: CLIOpts -> IO ()
defaultMainWith CLIOpts {..} = do
  numCap <- getNumCapabilities
  !v <- evaluate $ force $ compress $ sample size fun
  g <- maybe newStdGen (pure . mkStdGen) seed
  let !kM = size `quot` 2
      toFreq i = fromIntegral ((i + kM) `rem` size - kM) / 8
      decodeComp !i (!c :: Complex Double)
        | i == (0 :: Int) = (realPart c / fromIntegral size, 0)
        | otherwise =
            let re :+ im = 2 * c / fromIntegral size
             in (re, im)
      retrv = case output of
        Nothing -> evaluate . rnf
        Just fp -> \vs -> do
          createDirectoryIfMissing True $ takeDirectory fp
          writeFile fp
            $ unlines
            $ FML.toList
            $ FML.cons "Frequency\tcos\tsin"
            $ U.foldMap
              ( \(i, c) ->
                  let (co, si) = decodeComp i c
                   in FML.singleton $ show (toFreq i :: Double) <> "\t" <> show co <> "\t" <> show si
              )
            $ U.indexed
            $ V.convert vs
          putStrLn $ "Written to: " <> fp
  retrv $
    postprocess size $
      LV.modifyBoxedVector (Control.void PL.. fftDC g numCap threshold) v

postprocess :: Int -> V.Vector (Complex Double) -> V.Vector (Complex Double)
postprocess kN hs =
  let !kM = kN `quot` 2
   in V.generate kM \k ->
        let !m = (kM - k) `rem` kM
         in 0.5 * ((hs V.! k) + conjugate (hs V.! m))
              - (0 :+ 0.5) * (hs V.! k - conjugate (hs V.! m)) * exp (0 :+ (2 * pi * fromIntegral k / fromIntegral kN))
