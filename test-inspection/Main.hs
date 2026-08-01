module Main (main) where

import PureBorrow.Inspection.Fft qualified as Fft
import PureBorrow.Inspection.GenericGrowableUnrestricted qualified as GenericGrowableUnrestricted
import PureBorrow.Inspection.QSort qualified as QSort
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "optimized Core"
      [ Fft.tests
      , GenericGrowableUnrestricted.tests
      , QSort.tests
      ]
