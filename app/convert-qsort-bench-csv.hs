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
    "intro" -> pure mempty {introMean = Sum mean, introStddev = Sum stddev, introAlloc = Sum alloc, introCopied = Sum copied, introPeak = Sum peak}
    "sequential" -> pure mempty {sequentialMean = Sum mean, sequentialStddev = Sum stddev, sequentialAlloc = Sum alloc, sequentialCopied = Sum copied, sequentialPeak = Sum peak}
    "parallel (budget = 4)" -> pure mempty {parallel4Mean = Sum mean, parallel4Stddev = Sum stddev, parallel4Alloc = Sum alloc, parallel4Copied = Sum copied, parallel4Peak = Sum peak}
    "parallel (budget = 8)" -> pure mempty {parallel8Mean = Sum mean, parallel8Stddev = Sum stddev, parallel8Alloc = Sum alloc, parallel8Copied = Sum copied, parallel8Peak = Sum peak}
    "parallel (budget = 16)" -> pure mempty {parallel16Mean = Sum mean, parallel16Stddev = Sum stddev, parallel16Alloc = Sum alloc, parallel16Copied = Sum copied, parallel16Peak = Sum peak}
    "parallel (budget = 32)" -> pure mempty {parallel32Mean = Sum mean, parallel32Stddev = Sum stddev, parallel32Alloc = Sum alloc, parallel32Copied = Sum copied, parallel32Peak = Sum peak}
    "worksteal (workers = 2)" -> pure mempty {workSteal2Mean = Sum mean, workSteal2Stddev = Sum stddev, workSteal2Alloc = Sum alloc, workSteal2Copied = Sum copied, workSteal2Peak = Sum peak}
    "worksteal (workers = 4)" -> pure mempty {workSteal4Mean = Sum mean, workSteal4Stddev = Sum stddev, workSteal4Alloc = Sum alloc, workSteal4Copied = Sum copied, workSteal4Peak = Sum peak}
    "worksteal (workers = 6)" -> pure mempty {workSteal6Mean = Sum mean, workSteal6Stddev = Sum stddev, workSteal6Alloc = Sum alloc, workSteal6Copied = Sum copied, workSteal6Peak = Sum peak}
    "worksteal (workers = 8)" -> pure mempty {workSteal8Mean = Sum mean, workSteal8Stddev = Sum stddev, workSteal8Alloc = Sum alloc, workSteal8Copied = Sum copied, workSteal8Peak = Sum peak}
    "worksteal (workers = 10)" -> pure mempty {workSteal10Mean = Sum mean, workSteal10Stddev = Sum stddev, workSteal10Alloc = Sum alloc, workSteal10Copied = Sum copied, workSteal10Peak = Sum peak}
    _ -> Nothing
  pure (MIM.singleton size dat)

data SizeData = SizeData
  { introMean :: !(Sum Int)
  , introStddev :: !(Sum Int)
  , introAlloc :: !(Sum Int)
  , introCopied :: !(Sum Int)
  , introPeak :: !(Sum Int)
  , sequentialMean :: !(Sum Int)
  , sequentialStddev :: !(Sum Int)
  , sequentialAlloc :: !(Sum Int)
  , sequentialCopied :: !(Sum Int)
  , sequentialPeak :: !(Sum Int)
  , parallel4Mean :: !(Sum Int)
  , parallel4Stddev :: !(Sum Int)
  , parallel4Alloc :: !(Sum Int)
  , parallel4Copied :: !(Sum Int)
  , parallel4Peak :: !(Sum Int)
  , parallel8Mean :: !(Sum Int)
  , parallel8Stddev :: !(Sum Int)
  , parallel8Alloc :: !(Sum Int)
  , parallel8Copied :: !(Sum Int)
  , parallel8Peak :: !(Sum Int)
  , parallel16Mean :: !(Sum Int)
  , parallel16Stddev :: !(Sum Int)
  , parallel16Alloc :: !(Sum Int)
  , parallel16Copied :: !(Sum Int)
  , parallel16Peak :: !(Sum Int)
  , parallel32Mean :: !(Sum Int)
  , parallel32Stddev :: !(Sum Int)
  , parallel32Alloc :: !(Sum Int)
  , parallel32Copied :: !(Sum Int)
  , parallel32Peak :: !(Sum Int)
  , workSteal2Mean :: !(Sum Int)
  , workSteal2Stddev :: !(Sum Int)
  , workSteal2Alloc :: !(Sum Int)
  , workSteal2Copied :: !(Sum Int)
  , workSteal2Peak :: !(Sum Int)
  , workSteal4Mean :: !(Sum Int)
  , workSteal4Stddev :: !(Sum Int)
  , workSteal4Alloc :: !(Sum Int)
  , workSteal4Copied :: !(Sum Int)
  , workSteal4Peak :: !(Sum Int)
  , workSteal6Mean :: !(Sum Int)
  , workSteal6Stddev :: !(Sum Int)
  , workSteal6Alloc :: !(Sum Int)
  , workSteal6Copied :: !(Sum Int)
  , workSteal6Peak :: !(Sum Int)
  , workSteal8Mean :: !(Sum Int)
  , workSteal8Stddev :: !(Sum Int)
  , workSteal8Alloc :: !(Sum Int)
  , workSteal8Copied :: !(Sum Int)
  , workSteal8Peak :: !(Sum Int)
  , workSteal10Mean :: !(Sum Int)
  , workSteal10Stddev :: !(Sum Int)
  , workSteal10Alloc :: !(Sum Int)
  , workSteal10Copied :: !(Sum Int)
  , workSteal10Peak :: !(Sum Int)
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
