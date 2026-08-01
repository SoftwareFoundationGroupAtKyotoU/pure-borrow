module PureBorrow.Bench.Ingredients (benchReporter) where

import Test.Tasty.Bench (consoleBenchReporter, csvReporter, svgReporter)
import Test.Tasty.Ingredients (Ingredient, composeReporters)

{- | The reporting half of 'Test.Tasty.Bench.benchIngredients', as a single
composed ingredient.

Composition matters: 'Test.Tasty.defaultMainWithIngredients' runs the first
ingredient that accepts the given options, and 'consoleBenchReporter' accepts
every invocation. Listing the reporters separately would therefore make
@--csv@ and @--svg@ unreachable.

This is a separate ingredient rather than the whole 'benchIngredients' list so
that it can be named by @tasty-discover@'s @--ingredient@ flag; see
@bench/suite/Main.hs@. Discovery skips this module: @tasty-discover@ imports
only the modules that export a discovered binding.
-}
benchReporter :: Ingredient
benchReporter =
  consoleBenchReporter `composeReporters` (csvReporter `composeReporters` svgReporter)
