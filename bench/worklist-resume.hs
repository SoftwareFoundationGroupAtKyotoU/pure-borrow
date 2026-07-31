module Main (main) where

import PureBorrow.Internal.Bench.Worklist.Resume qualified as Worklist

main :: IO ()
main = Worklist.defaultMain
