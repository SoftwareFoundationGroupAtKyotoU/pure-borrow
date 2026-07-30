module Main (main) where

import PureBorrow.Internal.Bench.MultiStoreScan qualified as MultiStoreScan

main :: IO ()
main = MultiStoreScan.defaultMain
