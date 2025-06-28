{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception (throwIO)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Csv
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import qualified Data.IntMap.Monoidal.Strict as MIM
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import qualified Options.Applicative as Opts
import Text.Read (readEither)

data CLIOptions = CLIOptions
  { inputFile :: FilePath
  , outputFile :: FilePath
  }
  deriving (Show, Eq, Ord)

cliOptionsP :: Opts.ParserInfo CLIOptions
cliOptionsP =
  Opts.info (p <**> Opts.helper) $
    Opts.fullDesc
      <> Opts.progDesc "Convert a CSV file for qsort benchmark"
  where
    p :: Opts.Parser CLIOptions
    p =
      CLIOptions
        <$> Opts.strOption
          ( Opts.long "input"
              <> Opts.short 'i'
              <> Opts.metavar "INPUT_FILE"
              <> Opts.help "Input CSV file"
          )
        <*> Opts.strOption
          ( Opts.long "output"
              <> Opts.short 'o'
              <> Opts.metavar "OUTPUT_FILE"
              <> Opts.help "Output CSV file"
          )

data RawRow = RawRow
  { size :: !Int
  , name :: !T.Text
  , mean :: !Int
  , stddev :: !Int
  , alloc :: !Int
  , copied :: !Int
  , peak :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

instance FromNamedRecord RawRow where
  parseNamedRecord r = do
    fullName <- r .: "Name"
    let ~(sz : name : _) = drop 2 $ T.splitOn "." fullName
    size <- either fail pure $ readEither $ T.unpack sz
    mean <- r .: "Mean (ps)"
    stddev <- r .: "2*Stdev (ps)" <&> (`quot` 2)
    alloc <- r .: "Allocated"
    copied <- r .: "Copied"
    peak <- r .: "Peak Memory"
    pure RawRow {..}

type SizeDataMap = MonoidalIntMap SizeData

fromRawRow :: RawRow -> SizeDataMap
fromRawRow RawRow {..} = fromMaybe mempty do
  dat <- case name of
    "intro" -> pure mempty {introMean = Sum (fromIntegral mean * 1e-9), introStddev = Sum (fromIntegral stddev * 1e-9), introAlloc = Sum (fromIntegral alloc * 1e-6), introCopied = Sum (fromIntegral copied * 1e-6), introPeak = Sum (fromIntegral peak * 1e-6)}
    "sequential" -> pure mempty {sequentialMean = Sum (fromIntegral mean * 1e-9), sequentialStddev = Sum (fromIntegral stddev * 1e-9), sequentialAlloc = Sum (fromIntegral alloc * 1e-6), sequentialCopied = Sum (fromIntegral copied * 1e-6), sequentialPeak = Sum (fromIntegral peak * 1e-6)}
    "parallel (budget = 4)" -> pure mempty {parallel4Mean = Sum (fromIntegral mean * 1e-9), parallel4Stddev = Sum (fromIntegral stddev * 1e-9), parallel4Alloc = Sum (fromIntegral alloc * 1e-6), parallel4Copied = Sum (fromIntegral copied * 1e-6), parallel4Peak = Sum (fromIntegral peak * 1e-6)}
    "parallel (budget = 8)" -> pure mempty {parallel8Mean = Sum (fromIntegral mean * 1e-9), parallel8Stddev = Sum (fromIntegral stddev * 1e-9), parallel8Alloc = Sum (fromIntegral alloc * 1e-6), parallel8Copied = Sum (fromIntegral copied * 1e-6), parallel8Peak = Sum (fromIntegral peak * 1e-6)}
    "parallel (budget = 16)" -> pure mempty {parallel16Mean = Sum (fromIntegral mean * 1e-9), parallel16Stddev = Sum (fromIntegral stddev * 1e-9), parallel16Alloc = Sum (fromIntegral alloc * 1e-6), parallel16Copied = Sum (fromIntegral copied * 1e-6), parallel16Peak = Sum (fromIntegral peak * 1e-6)}
    "parallel (budget = 32)" -> pure mempty {parallel32Mean = Sum (fromIntegral mean * 1e-9), parallel32Stddev = Sum (fromIntegral stddev * 1e-9), parallel32Alloc = Sum (fromIntegral alloc * 1e-6), parallel32Copied = Sum (fromIntegral copied * 1e-6), parallel32Peak = Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 2)" -> pure mempty {workSteal2Mean = Sum (fromIntegral mean * 1e-9), workSteal2Stddev = Sum (fromIntegral stddev * 1e-9), workSteal2Alloc = Sum (fromIntegral alloc * 1e-6), workSteal2Copied = Sum (fromIntegral copied * 1e-6), workSteal2Peak = Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 4)" -> pure mempty {workSteal4Mean = Sum (fromIntegral mean * 1e-9), workSteal4Stddev = Sum (fromIntegral stddev * 1e-9), workSteal4Alloc = Sum (fromIntegral alloc * 1e-6), workSteal4Copied = Sum (fromIntegral copied * 1e-6), workSteal4Peak = Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 6)" -> pure mempty {workSteal6Mean = Sum (fromIntegral mean * 1e-9), workSteal6Stddev = Sum (fromIntegral stddev * 1e-9), workSteal6Alloc = Sum (fromIntegral alloc * 1e-6), workSteal6Copied = Sum (fromIntegral copied * 1e-6), workSteal6Peak = Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 8)" -> pure mempty {workSteal8Mean = Sum (fromIntegral mean * 1e-9), workSteal8Stddev = Sum (fromIntegral stddev * 1e-9), workSteal8Alloc = Sum (fromIntegral alloc * 1e-6), workSteal8Copied = Sum (fromIntegral copied * 1e-6), workSteal8Peak = Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 10)" -> pure mempty {workSteal10Mean = Sum (fromIntegral mean * 1e-9), workSteal10Stddev = Sum (fromIntegral stddev * 1e-9), workSteal10Alloc = Sum (fromIntegral alloc * 1e-6), workSteal10Copied = Sum (fromIntegral copied * 1e-6), workSteal10Peak = Sum (fromIntegral peak * 1e-6)}
    _ -> Nothing
  pure (MIM.singleton size dat)

data SizeData = SizeData
  { introMean :: !(Sum Double)
  , introStddev :: !(Sum Double)
  , introAlloc :: !(Sum Double)
  , introCopied :: !(Sum Double)
  , introPeak :: !(Sum Double)
  , sequentialMean :: !(Sum Double)
  , sequentialStddev :: !(Sum Double)
  , sequentialAlloc :: !(Sum Double)
  , sequentialCopied :: !(Sum Double)
  , sequentialPeak :: !(Sum Double)
  , parallel4Mean :: !(Sum Double)
  , parallel4Stddev :: !(Sum Double)
  , parallel4Alloc :: !(Sum Double)
  , parallel4Copied :: !(Sum Double)
  , parallel4Peak :: !(Sum Double)
  , parallel8Mean :: !(Sum Double)
  , parallel8Stddev :: !(Sum Double)
  , parallel8Alloc :: !(Sum Double)
  , parallel8Copied :: !(Sum Double)
  , parallel8Peak :: !(Sum Double)
  , parallel16Mean :: !(Sum Double)
  , parallel16Stddev :: !(Sum Double)
  , parallel16Alloc :: !(Sum Double)
  , parallel16Copied :: !(Sum Double)
  , parallel16Peak :: !(Sum Double)
  , parallel32Mean :: !(Sum Double)
  , parallel32Stddev :: !(Sum Double)
  , parallel32Alloc :: !(Sum Double)
  , parallel32Copied :: !(Sum Double)
  , parallel32Peak :: !(Sum Double)
  , workSteal2Mean :: !(Sum Double)
  , workSteal2Stddev :: !(Sum Double)
  , workSteal2Alloc :: !(Sum Double)
  , workSteal2Copied :: !(Sum Double)
  , workSteal2Peak :: !(Sum Double)
  , workSteal4Mean :: !(Sum Double)
  , workSteal4Stddev :: !(Sum Double)
  , workSteal4Alloc :: !(Sum Double)
  , workSteal4Copied :: !(Sum Double)
  , workSteal4Peak :: !(Sum Double)
  , workSteal6Mean :: !(Sum Double)
  , workSteal6Stddev :: !(Sum Double)
  , workSteal6Alloc :: !(Sum Double)
  , workSteal6Copied :: !(Sum Double)
  , workSteal6Peak :: !(Sum Double)
  , workSteal8Mean :: !(Sum Double)
  , workSteal8Stddev :: !(Sum Double)
  , workSteal8Alloc :: !(Sum Double)
  , workSteal8Copied :: !(Sum Double)
  , workSteal8Peak :: !(Sum Double)
  , workSteal10Mean :: !(Sum Double)
  , workSteal10Stddev :: !(Sum Double)
  , workSteal10Alloc :: !(Sum Double)
  , workSteal10Copied :: !(Sum Double)
  , workSteal10Peak :: !(Sum Double)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToNamedRecord, DefaultOrdered)
  deriving (Semigroup, Monoid) via Generically SizeData

newtype ODP = ODP (Int, SizeData)

instance DefaultOrdered ODP where
  headerOrder _ = "size" `V.cons` headerOrder (undefined :: SizeData)

instance ToNamedRecord ODP where
  toNamedRecord (ODP (sz, r)) = HM.insert "size" (BS8.pack $ show sz) $ toNamedRecord r

instance (ToField a) => ToField (Sum a) where
  toField (Sum x) = toField x

main :: IO ()
main = do
  CLIOptions {..} <- Opts.execParser cliOptionsP
  (_, rawRows) <- either (throwIO . userError) pure . decodeByName =<< LBS.readFile inputFile
  let sd = MIM.toList $ foldMap fromRawRow rawRows
  LBS.writeFile outputFile $ encodeDefaultOrderedByName $ coerce @_ @[ODP] sd
