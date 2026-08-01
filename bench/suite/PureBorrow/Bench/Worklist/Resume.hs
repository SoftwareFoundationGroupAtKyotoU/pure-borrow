{- | Exposes the benchmarks of "PureBorrow.Internal.Bench.Worklist.Resume" to
@tasty-discover@. The kernels themselves stay in the @bench-suites@ internal
library because @pure-borrow-test@ and @pure-borrow-inspection@ exercise them
too.
-}
module PureBorrow.Bench.Worklist.Resume (test_worklistResume) where

import PureBorrow.Internal.Bench.Worklist.Resume qualified as Resume
import Test.Tasty.Bench (Benchmark)

test_worklistResume :: [Benchmark]
test_worklistResume = Resume.benches
