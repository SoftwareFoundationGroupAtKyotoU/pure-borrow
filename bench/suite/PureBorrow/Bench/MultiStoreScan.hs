{- | Exposes the benchmarks of "PureBorrow.Internal.Bench.MultiStoreScan" to
@tasty-discover@. The kernels themselves stay in the @bench-suites@ internal
library because @pure-borrow-test@ and @pure-borrow-inspection@ exercise them
too.
-}
module PureBorrow.Bench.MultiStoreScan (test_multiStoreScan) where

import PureBorrow.Internal.Bench.MultiStoreScan qualified as MultiStoreScan
import Test.Tasty.Bench (Benchmark)

test_multiStoreScan :: [Benchmark]
test_multiStoreScan = MultiStoreScan.benches
