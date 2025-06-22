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
    "parallel (budget = 12)" -> pure mempty {parallel12Mean = Sum mean, parallel12Stddev = Sum stddev, parallel12Alloc = Sum alloc, parallel12Copied = Sum copied, parallel12Peak = Sum peak}
    "parallel (budget = 16)" -> pure mempty {parallel16Mean = Sum mean, parallel16Stddev = Sum stddev, parallel16Alloc = Sum alloc, parallel16Copied = Sum copied, parallel16Peak = Sum peak}
    "parallel (budget = 20)" -> pure mempty {parallel20Mean = Sum mean, parallel20Stddev = Sum stddev, parallel20Alloc = Sum alloc, parallel20Copied = Sum copied, parallel20Peak = Sum peak}
    "parallel (budget = 24)" -> pure mempty {parallel24Mean = Sum mean, parallel24Stddev = Sum stddev, parallel24Alloc = Sum alloc, parallel24Copied = Sum copied, parallel24Peak = Sum peak}
    "parallel (budget = 28)" -> pure mempty {parallel28Mean = Sum mean, parallel28Stddev = Sum stddev, parallel28Alloc = Sum alloc, parallel28Copied = Sum copied, parallel28Peak = Sum peak}
    "parallel (budget = 32)" -> pure mempty {parallel32Mean = Sum mean, parallel32Stddev = Sum stddev, parallel32Alloc = Sum alloc, parallel32Copied = Sum copied, parallel32Peak = Sum peak}
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
  , parallel12Mean :: !(Sum Int)
  , parallel12Stddev :: !(Sum Int)
  , parallel12Alloc :: !(Sum Int)
  , parallel12Copied :: !(Sum Int)
  , parallel12Peak :: !(Sum Int)
  , parallel16Mean :: !(Sum Int)
  , parallel16Stddev :: !(Sum Int)
  , parallel16Alloc :: !(Sum Int)
  , parallel16Copied :: !(Sum Int)
  , parallel16Peak :: !(Sum Int)
  , parallel20Mean :: !(Sum Int)
  , parallel20Stddev :: !(Sum Int)
  , parallel20Alloc :: !(Sum Int)
  , parallel20Copied :: !(Sum Int)
  , parallel20Peak :: !(Sum Int)
  , parallel24Mean :: !(Sum Int)
  , parallel24Stddev :: !(Sum Int)
  , parallel24Alloc :: !(Sum Int)
  , parallel24Copied :: !(Sum Int)
  , parallel24Peak :: !(Sum Int)
  , parallel28Mean :: !(Sum Int)
  , parallel28Stddev :: !(Sum Int)
  , parallel28Alloc :: !(Sum Int)
  , parallel28Copied :: !(Sum Int)
  , parallel28Peak :: !(Sum Int)
  , parallel32Mean :: !(Sum Int)
  , parallel32Stddev :: !(Sum Int)
  , parallel32Alloc :: !(Sum Int)
  , parallel32Copied :: !(Sum Int)
  , parallel32Peak :: !(Sum Int)
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
