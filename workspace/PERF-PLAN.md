# Pure-Borrow Performance and API Improvement Plan

## Summary

Implement the measured, pure-borrow-local improvements from
`FEEDBACK-FROM-TAMAGOH.md`:

- eliminate non-inline default instance workers on performance-sensitive types;
- add the inference-friendly `subShare` API;
- document the proven hot-loop, `Ref.update`, structured-move, and `Ur`-read
  patterns; and
- validate against pure-borrow's tests and the downstream tamagoh benchmark.

## Implementation

1. Give `BO`, `After`, and `Par` explicit, inlinable implementations of their
   performance-sensitive `Applicative`/`Monad` methods instead of accepting
   generated defaults.
2. Export `subShare :: (alpha >= beta) => Share alpha a -> Share beta a` from
   both the complete `BO` API and the package prelude.
3. Add API documentation for share-once read loops, single-pass `Ref.update`,
   avoiding structured `move` in hot loops, and inlining loops over `Ur` reads.
4. Keep pre-shared loop combinators and CPS/unboxed reads as measured-first
   follow-ups. `UrT` belongs to `linear-base` and is an upstream follow-up.

## Verification

- Format touched Haskell with Fourmolu.
- Run the library build, test suite, and doctests.
- Inspect optimized interfaces/Core to ensure default workers are replaced by
  stable inline unfoldings.
- Build and run tamagoh at `aec0b0f` against the modified pure-borrow in an
  isolated clone, without changing tamagoh's working tree. Treat this as a
  downstream integration/performance smoke test; a controlled before/after
  comparison is separate benchmark work.

## Results

Implemented:

- `BO` and `After` now define direct state-threading `liftA2` and `(>>)`
  methods with stable inline unfoldings. `Par` likewise defines its
  performance-sensitive applicative methods explicitly.
- The safe public API now exports `subShare`, an inference-friendly lifetime
  shortening operation for shared borrows.
- The umbrella tutorial and focused API Haddocks document share-once read
  loops, `Ref.update`, structured-move avoidance, and inlining loops over `Ur`
  reads.
- Tests cover `subShare` at compile time and the explicit linear and non-linear
  applicative/monadic methods at runtime.

Validated locally with the repository-pinned GHC 9.12.4:

- `cabal build all` succeeded.
- `cabal test all --test-show-details=direct` succeeded: all 21 main-suite
  tests passed, and all 20 doctest examples passed.
- Optimized interface inspection showed stable user inline unfoldings for the
  new `BO` and `After` `liftA2`/`(>>)` methods instead of generated vanilla
  default workers.
- An optimized isolated build of tamagoh `aec0b0f` succeeded against this
  working tree. `tamagoh-bench-math -j1` passed all 12 benchmark cases; its six
  tamagoh cases measured 206, 156, 168, 147, 195, and 143 ms. These numbers
  are a smoke-test snapshot, not a controlled speedup claim, because no
  same-session baseline was collected.

The GHC-version matrix remains CI's responsibility through `ci/configs/`.
Pre-shared combinators, CPS/unboxed reads, and an upstream `linear-base` `UrT`
remain explicitly deferred until downstream measurements justify them.
