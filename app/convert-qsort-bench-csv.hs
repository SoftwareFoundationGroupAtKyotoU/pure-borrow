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
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Csv
import Data.Functor
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.IntMap.Monoidal.Strict qualified as MIM
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics
import Options.Applicative qualified as Opts
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
    "intro" -> pure mempty {introMean = Just $ Sum (fromIntegral mean * 1e-9), introStddev = Just $ Sum (fromIntegral stddev * 1e-9), introAlloc = Just $ Sum (fromIntegral alloc * 1e-6), introCopied = Just $ Sum (fromIntegral copied * 1e-6), introPeak = Just $ Sum (fromIntegral peak * 1e-6)}
    "sequential" -> pure mempty {sequentialMean = Just $ Sum (fromIntegral mean * 1e-9), sequentialStddev = Just $ Sum (fromIntegral stddev * 1e-9), sequentialAlloc = Just $ Sum (fromIntegral alloc * 1e-6), sequentialCopied = Just $ Sum (fromIntegral copied * 1e-6), sequentialPeak = Just $ Sum (fromIntegral peak * 1e-6)}
    "parallel (budget = 4)" -> pure mempty {parallel4Mean = Just $ Sum (fromIntegral mean * 1e-9), parallel4Stddev = Just $ Sum (fromIntegral stddev * 1e-9), parallel4Alloc = Just $ Sum (fromIntegral alloc * 1e-6), parallel4Copied = Just $ Sum (fromIntegral copied * 1e-6), parallel4Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    "parallel (budget = 8)" -> pure mempty {parallel8Mean = Just $ Sum (fromIntegral mean * 1e-9), parallel8Stddev = Just $ Sum (fromIntegral stddev * 1e-9), parallel8Alloc = Just $ Sum (fromIntegral alloc * 1e-6), parallel8Copied = Just $ Sum (fromIntegral copied * 1e-6), parallel8Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    "parallel (budget = 16)" -> pure mempty {parallel16Mean = Just $ Sum (fromIntegral mean * 1e-9), parallel16Stddev = Just $ Sum (fromIntegral stddev * 1e-9), parallel16Alloc = Just $ Sum (fromIntegral alloc * 1e-6), parallel16Copied = Just $ Sum (fromIntegral copied * 1e-6), parallel16Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    "parallel (budget = 32)" -> pure mempty {parallel32Mean = Just $ Sum (fromIntegral mean * 1e-9), parallel32Stddev = Just $ Sum (fromIntegral stddev * 1e-9), parallel32Alloc = Just $ Sum (fromIntegral alloc * 1e-6), parallel32Copied = Just $ Sum (fromIntegral copied * 1e-6), parallel32Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 2)" -> pure mempty {workSteal2Mean = Just $ Sum (fromIntegral mean * 1e-9), workSteal2Stddev = Just $ Sum (fromIntegral stddev * 1e-9), workSteal2Alloc = Just $ Sum (fromIntegral alloc * 1e-6), workSteal2Copied = Just $ Sum (fromIntegral copied * 1e-6), workSteal2Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 4)" -> pure mempty {workSteal4Mean = Just $ Sum (fromIntegral mean * 1e-9), workSteal4Stddev = Just $ Sum (fromIntegral stddev * 1e-9), workSteal4Alloc = Just $ Sum (fromIntegral alloc * 1e-6), workSteal4Copied = Just $ Sum (fromIntegral copied * 1e-6), workSteal4Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 6)" -> pure mempty {workSteal6Mean = Just $ Sum (fromIntegral mean * 1e-9), workSteal6Stddev = Just $ Sum (fromIntegral stddev * 1e-9), workSteal6Alloc = Just $ Sum (fromIntegral alloc * 1e-6), workSteal6Copied = Just $ Sum (fromIntegral copied * 1e-6), workSteal6Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 8)" -> pure mempty {workSteal8Mean = Just $ Sum (fromIntegral mean * 1e-9), workSteal8Stddev = Just $ Sum (fromIntegral stddev * 1e-9), workSteal8Alloc = Just $ Sum (fromIntegral alloc * 1e-6), workSteal8Copied = Just $ Sum (fromIntegral copied * 1e-6), workSteal8Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    "worksteal (workers = 10)" -> pure mempty {workSteal10Mean = Just $ Sum (fromIntegral mean * 1e-9), workSteal10Stddev = Just $ Sum (fromIntegral stddev * 1e-9), workSteal10Alloc = Just $ Sum (fromIntegral alloc * 1e-6), workSteal10Copied = Just $ Sum (fromIntegral copied * 1e-6), workSteal10Peak = Just $ Sum (fromIntegral peak * 1e-6)}
    _ -> Nothing
  pure (MIM.singleton size dat)

data SizeData = SizeData
  { introMean :: !(Maybe (Sum Double))
  , introStddev :: !(Maybe (Sum Double))
  , introAlloc :: !(Maybe (Sum Double))
  , introCopied :: !(Maybe (Sum Double))
  , introPeak :: !(Maybe (Sum Double))
  , sequentialMean :: !(Maybe (Sum Double))
  , sequentialStddev :: !(Maybe (Sum Double))
  , sequentialAlloc :: !(Maybe (Sum Double))
  , sequentialCopied :: !(Maybe (Sum Double))
  , sequentialPeak :: !(Maybe (Sum Double))
  , parallel4Mean :: !(Maybe (Sum Double))
  , parallel4Stddev :: !(Maybe (Sum Double))
  , parallel4Alloc :: !(Maybe (Sum Double))
  , parallel4Copied :: !(Maybe (Sum Double))
  , parallel4Peak :: !(Maybe (Sum Double))
  , parallel8Mean :: !(Maybe (Sum Double))
  , parallel8Stddev :: !(Maybe (Sum Double))
  , parallel8Alloc :: !(Maybe (Sum Double))
  , parallel8Copied :: !(Maybe (Sum Double))
  , parallel8Peak :: !(Maybe (Sum Double))
  , parallel16Mean :: !(Maybe (Sum Double))
  , parallel16Stddev :: !(Maybe (Sum Double))
  , parallel16Alloc :: !(Maybe (Sum Double))
  , parallel16Copied :: !(Maybe (Sum Double))
  , parallel16Peak :: !(Maybe (Sum Double))
  , parallel32Mean :: !(Maybe (Sum Double))
  , parallel32Stddev :: !(Maybe (Sum Double))
  , parallel32Alloc :: !(Maybe (Sum Double))
  , parallel32Copied :: !(Maybe (Sum Double))
  , parallel32Peak :: !(Maybe (Sum Double))
  , workSteal2Mean :: !(Maybe (Sum Double))
  , workSteal2Stddev :: !(Maybe (Sum Double))
  , workSteal2Alloc :: !(Maybe (Sum Double))
  , workSteal2Copied :: !(Maybe (Sum Double))
  , workSteal2Peak :: !(Maybe (Sum Double))
  , workSteal4Mean :: !(Maybe (Sum Double))
  , workSteal4Stddev :: !(Maybe (Sum Double))
  , workSteal4Alloc :: !(Maybe (Sum Double))
  , workSteal4Copied :: !(Maybe (Sum Double))
  , workSteal4Peak :: !(Maybe (Sum Double))
  , workSteal6Mean :: !(Maybe (Sum Double))
  , workSteal6Stddev :: !(Maybe (Sum Double))
  , workSteal6Alloc :: !(Maybe (Sum Double))
  , workSteal6Copied :: !(Maybe (Sum Double))
  , workSteal6Peak :: !(Maybe (Sum Double))
  , workSteal8Mean :: !(Maybe (Sum Double))
  , workSteal8Stddev :: !(Maybe (Sum Double))
  , workSteal8Alloc :: !(Maybe (Sum Double))
  , workSteal8Copied :: !(Maybe (Sum Double))
  , workSteal8Peak :: !(Maybe (Sum Double))
  , workSteal10Mean :: !(Maybe (Sum Double))
  , workSteal10Stddev :: !(Maybe (Sum Double))
  , workSteal10Alloc :: !(Maybe (Sum Double))
  , workSteal10Copied :: !(Maybe (Sum Double))
  , workSteal10Peak :: !(Maybe (Sum Double))
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
