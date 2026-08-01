# AGENTS.md

This file is the source of truth for coding-agent guidance in this repository.
Keep agent-agnostic project instructions here, and keep tool-specific files as thin adapters.
`CLAUDE.md` is intentionally a symlink to this file so Claude Code and Codex read the same repository policy.

## Overview

**pure-borrow** realizes **Rust-style borrowing in Linear Haskell, purely** — compile-time ownership and memory safety with no runtime overhead, plus safe deterministic parallelism.
It is the artifact of the paper *Pure Borrow: Linear Haskell Meets Rust-Style Borrowing* (Y. Matsushita & H. Ishii, PLDI 2026; [arXiv:2604.15290](https://arxiv.org/abs/2604.15290)).
The package is already released on Hackage (version `0.0.0.0`); current work is **incremental improvement** (notably performance) of a published, paper-backed library — so preserve the public API and the soundness invariants unless a change is deliberate.

`Control.Monad.Borrow.Pure` is the umbrella module and carries the full Haddock tutorial; read it before designing changes to the core.

## Language & toolchain

- **GHC 9.10.2+** required (Linear Types); **9.12.4+ recommended** — pinned locally to `ghc-9.12.4` via `cabal.project.local`.
  `tested-with: 9.10.3 || 9.12.4 || 9.14.1`.
- `default-language: GHC2021`, with `LinearTypes` enabled by default everywhere.
- **Interpreter caveat:** GHC < 9.12.3 segfaults evaluating some linear programs in GHCi / the HLS eval plugin (a compiler bug).
  Compiled code is fine on 9.10.2+.
  Use 9.12.3+ if you need the REPL.
  The `pure-borrow-doctests` suite is disabled for GHC < 9.12.3.
- Cabal **nix-style builds only**; `cabal.project` (+ `cabal.project.local`) is the source of truth.
  Do not use stack or invoke `ghc` directly.
  cabal-install ≥ 3.14.2 recommended.

## Build / test / bench / run

`cabal.project` sets the `+examples` flag and `cabal.project.local` enables tests+benchmarks, so locally every component below is buildable.

```bash
cabal build all                       # build the library + all enabled components
cabal build pure-borrow               # just the library

cabal test                            # run all test suites
cabal test pure-borrow-test           # main tasty suite only
cabal test pure-borrow-doctests       # doctests (needs GHC >= 9.12.3)

cabal bench qsort-bench               # parallel quicksort benchmark (tasty-bench)
cabal bench fft-bench                 # parallel FFT benchmark (tasty-bench)

cabal run qsort -- --help             # quicksort demo executable (needs +examples)
cabal run fft   -- --help             # FFT demo executable (needs +examples)
```

Prefer HLS (via the haskell skill) for iterating; it is far faster than a full `cabal build` for typecheck / hover / go-to-definition / find-references / rename.

### Tests — tasty + tasty-discover (property tests via `falsify`)

`test/Main.hs` is only the `tasty-discover` driver (`-optF --tree-display`); real tests live in `test/**/*Spec.hs`, in modules mirroring the source tree.
Filter with tasty's `-p`:

```bash
cabal test pure-borrow-test --test-options='-p "Lifetime"'   # run a subset by pattern
```

`test/Control/Monad/Borrow/Pure/Lifetime/TypingCases.hs` holds type-level (compile-time) constraint checks, not runtime assertions.

Two kinds of failing test look superficially alike here, and they encode opposite intentions.
Never convert one into the other.

**"This must not typecheck."** Rejection is the specification, and a case that started compiling would be a soundness bug.
Define the intentionally ill-typed cases in a separate `TypingCases` module compiled with `-fdefer-type-errors -Wno-deferred-type-errors`.
In the corresponding `*Spec.hs`, force each case to WHNF with `evaluate`, catch the exception with `try`, and inspect its diagnostic text.
Do not use `expectFail`, `expectFailBecause`, or another expected-failure wrapper: the test itself must pass only when it observes the intended deferred type error.

**"This ought to hold, but we have not achieved it."** The property is *desirable*, and the failure records a known limitation rather than an invariant.
Write the test as the property you want and wrap it in `expectFailBecause "<why it does not hold yet>"`.
The suite then stays green while the limitation stands, and turns red the day the property starts holding — which is exactly the notification you want.
`test_should_pass` in `test/Control/Monad/Borrow/Pure/LifetimeSpec.hs` is the reference case: transitivity and monotonicity of the outlives relation *should* hold, and the layered `INCOHERENT` instances simply do not derive them today.
Asserting a deferred type error there would claim the opposite — that we intend those properties to be underivable.

Exception verified with GHC 9.12.4: linear multiplicity errors such as `Couldn't match type 'Many' with 'One'` are rejected while compiling the module even with `-fdefer-type-errors -Wno-deferred-type-errors`; they do not reach the runtime deferred-error path above.
Put those cases in `test/typing-fail/` and validate their compile failure using the Cabal-selected compiler.
Keep errors that GHC does defer in `TypingCases`.

### Benchmarks & profiling

Benchmarks are `tasty-bench` executables; pass options through cabal, e.g. `cabal bench qsort-bench --benchmark-options='--csv bench-results/qsort.csv -j1 --time-mode=wall +RTS -N -s'`.
For profiled runs use the dedicated project file: `cabal --project-file=cabal-bench.project ...` (enables `-fprof-late`/`-fprof-auto` profiling).
CSV/plots land in `bench-results/`.

#### Never call `unsafePerformIO` in a `direct` baseline

A `direct…` kernel exists to show what the plain `vector` API costs, so it must be written the way a user of `vector` would actually write it.
Use `modify` (`Data.Vector.modify`, `Data.Vector.Unboxed.modify`, `Data.Vector.Generic.modify`), which takes a `forall s. MVector s a -> ST s ()` and handles the thaw/freeze internally.
Reach for `STRef` when a kernel needs auxiliary mutable state inside that `ST` action.

Never hand-roll the same thing as `unsafePerformIO (thaw >>= … >>= unsafeFreeze)`.
It is unsound — `unsafePerformIO` gives no guarantee the action runs once, or at all, or is not floated out of a loop — and it also makes the comparison dishonest, because the measured pure-borrow variant is held to a safety standard the baseline is not.
`unsafeFreeze` on a buffer that was reachable from `IO` is exactly the aliasing hazard this library exists to rule out.
The same applies to `unsafeThaw`, `unsafeDupablePerformIO` and `runST . unsafeIOToST`.

This prohibition is about *baselines*, not about the library: trusted `unsafe*` primitives inside `src/` remain a proof obligation as described above, and `unsafePerformIO` is still fine in a test that deliberately observes an effect (for example the `IORef`-based copy/move trackers in the vector specs).

## Architecture

Everything lives under `src/`.
The public entry point is `Control.Monad.Borrow.Pure`.

### Module-suffix convention (strict — respect the boundary)

- **No suffix = safe public API.** User/application code should import only these (`Control.Monad.Borrow.Pure`, `Data.Vector.Mutable.Linear.Borrow`, `Data.Ref.Linear.Borrow`, `Control.Concurrent.DivideConquer.Linear`, `…/Lifetime.hs`, `…/Lifetime/Token.hs`, …).
- **`.Internal` = real definitions, `{-# OPTIONS_HADDOCK hide #-}`.** Exposed (so other modules can import) but omitted from docs; contains the actual newtypes and the `Unsafe*` constructors.
- **`.Unsafe` = trusted escape hatches** (`unsafeSystemIOToBO`, `unsafeCastBO`, `Alias(..)`, `LinearOnly(..)`, …).
  Every use is a proof obligation that lifetime/linearity invariants hold.
  Only the data-structure and scheduler modules should import these.
- **`Utils` / `Utils/**` = truly private** (`other-modules`, not exposed).
- **`Experimental.*`** modules are exposed but unstable (`Borrows`, `Loop`, `Reborrowable`, and `Data.Record.Linear.Borrow.Experimental.*` record-splitting).

### Core: the `BO` monad — `src/Control/Monad/Borrow/Pure/BO/Internal.hs`

`BO α a` is morally a **linear `ST` monad** whose phantom state token is indexed by a *lifetime* `α` (kind `Lifetime`) instead of `ST`'s `s`.
Run with `runBO`/`runBO_`/`srunBO`, which require a `Linearly` witness.
Purity of parallelism (`parBO`) is provided by trusted `unsafe*` primitives that fork real IO and `evaluate` into `MVar`s — observationally pure thanks to the phantom-state + linearity discipline.

Borrow types are all one zero-cost representation, `Alias ak α a`:
- `Mut α a` — mutable/exclusive borrow (affine + `LinearOnly`).
- `Share α a` — shared/read-only borrow (`Dupable`+`Movable`, freely copied/dropped).
- `Lend α a` — the capability to `reclaim` the original once the lifetime ends.
- Intro/elim: `borrow`, `share`, `reborrow`, `reclaim`/`reclaim'`, plus `split*` machinery (generic, via `Generics.Linear`) that turns a borrow of a structure into a structure of borrows.

### Lifetimes — `src/Control/Monad/Borrow/Pure/Lifetime/`

- `Lifetime/Internal.hs` — the type-level algebra: `Lifetime = Al Nat | (:/\) | Static`, a free bounded lower-semilattice; `/\` is meet.
  The outlives relation `(<=)`/`(>=)` is a layered class hierarchy with explicit GADT witnesses and `INCOHERENT` instances that hand-implement transitivity/associativity of subtyping (no typechecker plugin).
- `Lifetime/Token/Internal.hs` — zero-cost value-level tokens (`Now`, `EndToken`/`End`, `newLifetime`), the `After α a` finalizer monad, and the linearity witnesses (`Linearly`, `linearly`, `LinearOnly`).
  Several `NOINLINE`/`noinline` annotations here deliberately defeat CSE / full-laziness that would otherwise duplicate linear tokens — **do not "clean these up".**

The same rule applies wherever a binding's own body calls `unsafePerformIO`: mark it `NOINLINE`, and mark any class method that reaches one — `Consumable`'s `consume` for the vector owners is the recurring case.
Inlining hands GHC a licence the linear types do not: it can duplicate the call across use sites or float it out of a scope, and each surviving copy runs the effect again.
This bites even when the action only *reads*, as an element-consuming traversal does, because running it twice consumes every element twice.
`INLINE` on such a binding is a bug, not a tuning choice.
An ordinary `IO` worker that does not itself call `unsafePerformIO` may stay `INLINE`; it is the `unsafePerformIO` occurrence that must be kept unique.

### Parallel divide-and-conquer — `src/Control/Concurrent/DivideConquer/Linear.hs`

A borrow-safe **work-stealing/work-sharing** divide-and-conquer skeleton.
The user supplies a `DivideConquer` record (`initialise`/`divide`/`conquer`); `divide` splits a `Mut` borrow into a traversable of disjoint sub-borrows in the parent lifetime.
Substrate:
- `src/Control/Concurrent/Queue/ChaseLev.hs` — lock-free Chase-Lev deque.
- `…/DivideConquer/Utils/QueuePool.hs` — the hybrid steal/share scheduler.
- `…/Utils/Semaphore.hs`, `…/Utils/OnceChan/Linear.hs` — linear sync + one-shot channels.

Worked examples exported here: `qsortDC`/`fftDC` (scheduler-backed).
Simpler baselines: `sequentialDivideAndConquer'`, `naiveDivideAndConquer'` (fork-per-node via `parBO`).

### Borrow-aware mutable vector — `src/Data/Vector/Mutable/Linear/Borrow.hs`

`Vector a` owns each element *linearly* (so it can nest other mutable resources); it is `LinearOnly` and intentionally **not** `Copyable`.
`splitAt` splitting a borrow into two disjoint sub-borrows is the key primitive for parallelism.
Includes a demonstrative in-place parallel `qsort` (budgeted `parBO`; the heavier version is `qsortDC` above).

## Conventions & workflow

- **`(<>)` over `(++)`** for all concatenation, including lists and strings.
- **One sentence per line.** Never fold a line in the middle of a sentence — insert a newline only at a sentence boundary, and let the editor soft-wrap whatever is long.
  This governs every kind of prose you write: Markdown files, Haddock and ordinary source comments, and commit-message bodies.
  It keeps diffs sentence-scoped, so rewording one sentence never reflows the paragraph around it.
- **Lifetime parameter names:** quantify lifetime parameters as `α`, `β`, `γ`, using primes or numeric suffixes when more are needed.
  Do not use prose names such as `lifetime`, `scope`, or `inner` for lifetime type variables.
- **Multiplicity determines ownership:** data bound nonlinearly (through an ordinary arrow / `%Many`) is GC-owned.
  Do not require or invoke `Clone`, `Dupable`, or `Consumable` merely to copy, retain, or discard that data, including elements reached through an unrestricted standard container.
  By the Linear Haskell convention, a nonlinearly bindable resource may be consumed repeatedly; exactly-once consumption applies only to a linearly bound resource.
  Therefore final consumption of a linear container may consume its entries without tracking whether an entry originally came from a `%Many` source.
  Follow the multiplicity at the binding/use boundary, not the surface syntax of a datatype constructor: a linearly bound value can be stored in an ordinary-looking polymorphic datatype while the resulting value remains linear.
  Do not give a polymorphic container shallow capability instances merely because its fields lack explicit `%1`; preserve component constraints and operations unless the component type is concretely unrestricted.
- **`Copyable` is separate from nonlinear ownership:** `copy` always consumes a `Borrow` linearly.
  It is not an operation on `%Many` data and must not be used to process an unrestricted source.
  For example, a `V.Vector a ->` source and its elements are GC-owned, so cloning its buffer neither requires `Copyable a` nor calls `copy`.
  Evaluating `copy` must complete the copy and return its result in WHNF, so callers may rely on the copy having completed before the borrow is recovered or its lifetime ends.
- **Moving into GC ownership requires `Movable`:** when a consuming operation transfers linearly owned contents into an unrestricted `Ur`-wrapped container, require `Movable`, not `Copyable`, and invoke `move` for every owned piece.
  A `Movable` instance may need to deep-copy before returning the GC-owned value; a bare constraint that is never used is not sufficient.
  `Copyable` instead describes copying from a live borrow.
- **Element ownership is an independent container axis:** distinguish element-owning containers, which bind their entries linearly and exclusively own the represented resources, from non-element-owning containers, whose entries are GC-owned and bind nonlinearly.
  Consuming materialization of an element-owning container must invoke `move` for every entry; a non-element-owning container may freeze its backing storage in O(1) because no element crosses from linear into nonlinear ownership.
  Both modes may still linearly own their mutable backing storage.
  Do not infer element ownership from boxed versus unboxed representation, fixed versus growable storage, or mutable versus immutable access: it is a separate design choice.
- **No `package.yaml`/hpack** — edit `pure-borrow.cabal` directly, then run cabal-gild.
  The `exposed-modules`/`other-modules` lists are generated by `-- cabal-gild: discover` pragmas (with `--exclude` globs for `Utils`), so after adding/removing/renaming a module re-run cabal-gild instead of hand-editing the list.
- **Format before compiling**, enforced by CI and by the shared PostToolUse hooks in `.agents/hooks/` (wired from both Claude and Codex config):
  - `.hs`/`.lhs`/`.hsig` → **fourmolu** (config `fourmolu.yaml`; CI pins fourmolu 0.20.0.0).
  - `.cabal`/`cabal.project*` → **cabal-gild** (≥ 1.6).
    Use the integration in your agent (the `haskell-format` / `haskell-cabal-gild` skills, or the hooks) rather than reformatting by hand.
- **Strict warnings** (`common defaults` in `pure-borrow.cabal`): `-Wall -Wcompat -Wunused-packages …`.
  `-Wunused-packages` means a now-unused `build-depends` entry breaks the build — drop deps you stop using.
- **Flags:** both are `manual` and default `False`.
  - `examples` — demo executables + `demo-impl` lib; enabled locally via `cabal.project`.
  - `slow` — restores the previous, sublifetime-allocating implementations of `sharing_`, `reborrowing_` and `copyAtMut`, behind `-DPURE_BORROW_SLOW_SCOPES`.
    The two variants must stay observationally equivalent: the same test suites run under both, and CI builds `+slow` on the pinned GHC.
    Use it to A/B the erased scopes, or to check whether a suspected miscompilation is attributable to them.
    ```bash
    cabal build all --flags=+slow && cabal test --flags=+slow
    ```
- CI (`.github/workflows/haskell.yml`) runs a GHC matrix (9.10.3/9.12.4/9.14.1 via `ci/configs/*.project`): build, all test suites, `cabal check`, Haddock-for-Hackage, and a fourmolu check.
- **You do not have to test against every GHC version** — that is CI's responsibility, and the per-GHC project files under `ci/configs/` exist precisely for it.
  Locally, work with the pinned `ghc-9.12.4` (from `cabal.project.local`); only reach for another version via `cabal --project-file=ci/configs/ghc-<ver>.project …` when reproducing a version-specific failure.
- Trust the **LSP before a full build**, and prefer **local docs/source over remote** (read the cabal store / `dist-newstyle` / repo-cache before hitting Hackage or remote Hoogle) to avoid loading Haskell's public infrastructure.
- **Commits** must follow **Conventional Commits** style (https://www.conventionalcommits.org) — a `type(scope): summary` subject (e.g. `perf(fft): …`, `fix: …`), with a brief summary of the changes in the message body.
  You **must** make the coauthorship explicit by appending a `Co-authored-by:` trailer.
  - **Do not include internal session information** in commit messages.
    No agent/session metadata trailers such as `Claude-Session:` (or links to session transcripts), and no tool-internal identifiers.
    Keep the message about the change itself; the only agent-related trailer allowed is `Co-authored-by:`.
