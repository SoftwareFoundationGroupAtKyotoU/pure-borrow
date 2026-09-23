# Implementing linear primitives

Read this only when writing the unsafe core of a linear API: a mutable container, a capability token, a parallel combinator, or an FFI wrapper.
Application code should never need it.

A linear API makes destructive operations look pure: `set :: Int -> a -> Array a %1 -> Array a` really writes into memory.
That is sound only while every resource has a unique owner and the optimiser evaluates every such expression exactly once, in the order the data dependencies imply.
GHC does not know the expression is effectful, so the implementation must stop it from duplicating, sharing, merging, or moving the effect.

## Rules

1. **Make uniqueness a property of the API.**
   A linear arrow does not stop callers from passing an unrestricted value, so hide the resource's constructor and hand resources out only through a linear continuation (`(Array a %1 -> b) %1 -> b`), beside an existing linear resource, or in exchange for a consumed linearity token.
   A function such as `new :: Int -> Array a` would let `let a = new 3 in (write a 0 x, a)` typecheck.
2. **Use `unsafePerformIO`, which runs `noDuplicate#`, for a destructive operation exposed as a pure function.**
   `unsafeDupablePerformIO` and bare `runRW#` (which is what `unsafeDupablePerformIO` is built on) allow two threads to enter the same thunk and run the write twice.
   Use the dupable forms only with an argument that the thunk can never be entered twice; linear-base's arrays and pure-borrow's reference internals do this deliberately.
   Never fake purity with `unsafeThaw`/`unsafeFreeze` tricks or `unsafeIOToST`.
3. **Protect every binding whose body reaches an effect from inlining and worker/wrapper**: mark it `OPAQUE`, or `NOINLINE` with its right-hand side wrapped in `GHC.Exts.noinline`.
   Plain `NOINLINE` still allows worker/wrapper: GHC can then rebuild a field-less token in the caller and merge two allocations into one, or common-subexpression-eliminate two calls that look identical.
   This applies to allocators, token producers and duplicators (`linearly`, `dup2` of a token), and `consume` methods that traverse elements; `INLINE` on such a binding is a bug.
4. **Make every effect mention an argument of its function** (the token, the size, the input), or compile the module with `-fno-full-laziness -fno-cse`.
   Otherwise full laziness floats an effect such as `unsafePerformIO (newIORef 0)` out of the lambda into a single top-level value that every call shares, even under `OPAQUE`.
5. **Parallel combinators are effectful too.**
   A linear `par :: a %1 -> b %1 -> (a, b)` evaluates pure-but-destructive work on other threads, so its runner needs the same protection (`noDuplicate#`, not the dupable runner of the `parallel` package).
6. **Always hand the resource back.**
   Every operation except the final consumer returns the resource, and reads return unrestricted results in `Ur` only when the element really is unrestricted.
   If a container owns its elements linearly, `set` must return the old element rather than drop it.
7. **Ban instances that conjure or alias unrestricted values.**
   A mutable type must not be `Movable` or have a Prelude `Monoid` (`mempty` would create an unrestricted value); ban them with `Unsatisfiable`.
8. **Every `Unsafe.Linear.toLinear`, `unsafeCoerce`, or `Unsafe.coerce` is a proof obligation.**
   State the invariant it relies on next to the use.
9. **Exceptions are outside the model.**
   Document what leaks when an exception interrupts a linear computation, and route system resources through `RIO` or pools.

## Testing

- Test the intended behaviour at the optimisation level you ship (`-O2`), since the hazards above only appear after inlining, worker/wrapper, CSE, and floating; a program whose result differs between `-O0` and `-O2` is broken.
  A useful probe: allocate twice from duplicated tokens and check that the two resources are distinct.
- Keep "must not typecheck" tests for misuse (leaking a resource into `Ur`, using it twice).
  With `-fdefer-type-errors`, most type errors, including multiplicity mismatches between arrow types (`Expected: Int %1 -> Int`, `Actual: Int -> Int`), are deferred and can be observed at run time.
  Usage errors (`Couldn't match type 'Many' with 'One' arising from multiplicity of 'x'`) are reported at compile time even then, so check those with a separate compile-fail test.
- Distinguish "must not typecheck" tests (rejection is the specification) from "should hold but does not yet" tests (mark those as expected failures) and never convert one into the other.
