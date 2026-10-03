module Main (main) where

import PureBorrow.Inspection.Barriers qualified as Barriers
import PureBorrow.Inspection.Fft qualified as Fft
import PureBorrow.Inspection.GenericGrowableUnrestricted qualified as GenericGrowableUnrestricted
import PureBorrow.Inspection.GrowableAccess qualified as GrowableAccess
import PureBorrow.Inspection.MultiStoreScan qualified as MultiStoreScan
import PureBorrow.Inspection.QSort qualified as QSort
import PureBorrow.Inspection.Sublifetime qualified as Sublifetime
import PureBorrow.Inspection.Worklist.Resume qualified as WorklistResume
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "optimized Core"
      [ Barriers.tests
      , Fft.tests
      , GenericGrowableUnrestricted.tests
      , GrowableAccess.tests
      , MultiStoreScan.tests
      , QSort.tests
      , Sublifetime.tests
      , Sublifetime.barrierTests
      , WorklistResume.tests
      ]
