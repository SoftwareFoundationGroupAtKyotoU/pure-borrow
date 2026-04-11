{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Exception (throwIO, try)
import Control.Monad (forM_)
import Control.Monad.Trans.Writer.CPS (execWriter, tell)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as LBS
import Data.Csv (FromNamedRecord (..), decodeByName, (.:))
import Data.FileEmbed (embedFile)
import Data.Foldable (fold)
import Data.Foldable1 (fold1)
import Data.Functor
import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.IntMap.Monoidal.Strict qualified as MIM
import Data.IntSet qualified as IS
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Monoid (First (..))
import Data.Semigroup qualified as Semi
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import GHC.IO.Handle (hClose, hFlush)
import Options.Applicative qualified as Opts
import PureBorrow.Demo.QSort qualified as QS
import PureBorrow.Internal.Bench.QSort (BenchOpts)
import PureBorrow.Internal.Bench.QSort qualified as Bench
import System.Directory (canonicalizePath, findExecutable)
import System.Environment (withArgs)
import System.Exit (ExitCode)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcess)
import Text.Read (readEither)

data Cmd = Bench BenchOpts | QuickBench | QSortDemo QS.CLIOpts
  deriving (Show, Eq, Ord, Generic)

optionsP :: Int -> Opts.ParserInfo Cmd
optionsP numCapa =
  Opts.info (p <**> Opts.helper) $
    Opts.fullDesc
      <> Opts.progDesc "Artifact runner for qsort benchmarks and demos"
  where
    p = cmds <|> Bench <$> Bench.rawOptsP
    cmds =
      Opts.hsubparser $
        fold1 $
          Opts.command "bench" (Bench <$> Bench.optionsP)
            :| [ Opts.command "demo" $ QSortDemo <$> QS.optionsP numCapa
               , Opts.command "quick" $
                   Opts.info (pure QuickBench) $
                     Opts.progDesc $
                       "Run quick benchmarks with numcpu = 4 for sizes 0 and " <> show Bench.kMAX_SIZE
               ]

main :: IO ()
main = do
  numCap <- getNumCapabilities
  Opts.customExecParser (Opts.prefs Opts.subparserInline) (optionsP numCap) >>= \case
    Bench benchOpts -> runBench benchOpts
    QuickBench -> runBench Bench.BenchOpts {numThreads = 4, sampleSize = 2}
    QSortDemo cliOpts -> QS.defaultMainWith cliOpts

runBench :: BenchOpts -> IO ()
runBench benchOpts = do
  let rawDest = "qsort-raw.csv"
  void $ try @ExitCode $ withArgs ["--csv", rawDest, "-j1", "--time-mode=wall", "-t", "10s"] do
    setNumCapabilities benchOpts.numThreads
    Bench.defaultMainWith benchOpts

  putStrLn "Processing results..."
  (_, rawRows) <- either (throwIO . userError) pure . decodeByName =<< LBS.readFile rawDest
  let sd = foldMap fromRawRow rawRows
      builder = buildOutput sd
  csvDest <- canonicalizePath "qsort.csv"
  BB.writeFile csvDest builder

  mgp <- findExecutable "gnuplot"
  forM_ mgp \gnuplot -> withSystemTempFile "plot.gp" \tmp h -> do
    putStrLn $ "Gnuplot found: " <> gnuplot
    pngDest <- canonicalizePath "qsort.png"
    BS.hPut h gnuplotScript
    hFlush h
    hClose h
    !_ <- readProcess gnuplot ["-e", "input='" <> csvDest <> "'; output='" <> pngDest <> "'", tmp] ""
    putStrLn $ "Plot generated: " <> pngDest

gnuplotScript :: BS.ByteString
gnuplotScript = $(embedFile "scripts/genplot.gnuplot")

buildOutput :: Statistics -> BB.Builder
buildOutput sd = execWriter do
  let (hdrs, targets) = toHeaders sd

  putLine $
    fold $
      List.intersperse "," $
        map (BB.byteString . TE.encodeUtf8) hdrs
  forM_ (MIM.toAscList sd) \(size, ps) -> do
    let row =
          BB.intDec size
            : concatMap
              ( \t ->
                  maybe (replicate 5 mempty) (\p -> map BB.doubleDec [p.mean, p.stddev, p.alloc, p.copied, p.peak]) $
                    lookupStat t ps
              )
              targets
    putLine $ fold $ List.intersperse "," row
  where
    crlf = tell "\r\n"
    putLine = (>> crlf) . tell

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

data Performance = Performance
  { mean :: !Double
  , stddev :: !Double
  , alloc :: !Double
  , copied :: !Double
  , peak :: !Double
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (Semigroup) via Semi.First Performance

toPerformance :: RawRow -> Performance
toPerformance RawRow {..} =
  Performance
    { mean = fromIntegral mean * 1e-9
    , stddev = fromIntegral stddev * 1e-9
    , alloc = fromIntegral alloc * 1e-6
    , copied = fromIntegral copied * 1e-6
    , peak = fromIntegral peak * 1e-6
    }

data PerformanceSet = PerformanceSet
  { intro :: !(First Performance)
  , sequential :: !(First Performance)
  , parallel :: !(MonoidalIntMap Performance)
  , worksteal :: !(MonoidalIntMap Performance)
  , others :: !(MonoidalMap T.Text Performance)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via Generically PerformanceSet

type Statistics = MonoidalIntMap PerformanceSet

data Target = Intro | Sequential | Parallel Int | Worksteal Int | Other T.Text
  deriving (Show, Eq, Ord, Generic)

lookupStat :: Target -> PerformanceSet -> Maybe Performance
lookupStat t ps =
  case t of
    Intro -> getFirst ps.intro
    Sequential -> getFirst ps.sequential
    Parallel n -> MIM.lookup n ps.parallel
    Worksteal n -> MIM.lookup n ps.worksteal
    Other name -> MonoidalMap.lookup name ps.others

toHeaders :: Statistics -> ([T.Text], [Target])
toHeaders stats =
  (headers, targets)
  where
    headers = "size" : [cat <> metric | cat <- categories, metric <- metrics]
    targets = Intro : Sequential : [Parallel n | n <- IS.toList parallels] ++ [Worksteal n | n <- IS.toList worksteals] ++ [Other name | name <- Set.toList miscs]
    (parallels, worksteals, miscs) =
      foldMap
        ( \ps ->
            ( MIM.keysSet ps.parallel
            , MIM.keysSet ps.worksteal
            , MonoidalMap.keysSet ps.others
            )
        )
        stats
    categories =
      "intro"
        : "sequential"
        : [T.pack $ "parallel" <> show n | n <- IS.toList parallels]
        ++ [T.pack $ "worksteal" <> show n | n <- IS.toList worksteals]
        ++ [name | name <- Set.toList miscs]

    metrics = ["Mean", "Stddev", "Alloc", "Copied", "Peak"]

fromRawRow :: RawRow -> Statistics
fromRawRow r@RawRow {..} = MIM.singleton size $
  case name of
    "intro" -> mempty {intro = First (Just $ toPerformance r)}
    "sequential" -> mempty {sequential = First (Just $ perf)}
    inp
      | Just rest <- T.stripPrefix "parallel (budget =" inp
      , [(n, _)] <- reads (T.unpack rest) ->
          mempty {parallel = MIM.singleton n perf}
      | Just rest <- T.stripPrefix "worksteal (workers =" inp
      , [(n, _)] <- reads (T.unpack rest) ->
          mempty {worksteal = MIM.singleton n perf}
    _ -> mempty {others = MonoidalMap.singleton name perf}
  where
    perf = toPerformance r
