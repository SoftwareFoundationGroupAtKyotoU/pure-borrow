{- | Exposes the benchmarks of "PureBorrow.Internal.Bench.Unboxed" to
@tasty-discover@. The kernels themselves stay in the @bench-suites@ internal
library because @pure-borrow-test@ exercises them too.
-}
module PureBorrow.Bench.Unboxed (test_unboxed) where

import PureBorrow.Internal.Bench.Unboxed qualified as Unboxed
import Test.Tasty.Bench (Benchmark)

test_unboxed :: [Benchmark]
test_unboxed = Unboxed.benches
