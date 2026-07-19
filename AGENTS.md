# AGENTS.md

This file is the source of truth for coding-agent guidance in this repository. Keep
agent-agnostic project instructions here, and keep tool-specific files as thin adapters.
`CLAUDE.md` is intentionally a symlink to this file so Claude Code and Codex read the same
repository policy.

## Overview

**pure-borrow** realizes **Rust-style borrowing in Linear Haskell, purely** — compile-time
ownership and memory safety with no runtime overhead, plus safe deterministic parallelism.
It is the artifact of the paper *Pure Borrow: Linear Haskell Meets Rust-Style Borrowing*
(Y. Matsushita & H. Ishii, PLDI 2026; [arXiv:2604.15290](https://arxiv.org/abs/2604.15290)).
The package is already released on Hackage (version `0.0.0.0`); current work is
**incremental improvement** (notably performance) of a published, paper-backed library — so
preserve the public API and the soundness invariants unless a change is deliberate.

`Control.Monad.Borrow.Pure` is the umbrella module and carries the full Haddock tutorial;
read it before designing changes to the core.

## Language & toolchain

- **GHC 9.10.2+** required (Linear Types); **9.12.4+ recommended** — pinned locally to
  `ghc-9.12.4` via `cabal.project.local`. `tested-with: 9.10.3 || 9.12.4 || 9.14.1`.
- `default-language: GHC2021`, with `LinearTypes` enabled by default everywhere.
- **Interpreter caveat:** GHC < 9.12.3 segfaults evaluating some linear programs in GHCi /
  the HLS eval plugin (a compiler bug). Compiled code is fine on 9.10.2+. Use 9.12.3+ if you
  need the REPL. The `pure-borrow-doctests` suite is disabled for GHC < 9.12.3.
- Cabal **nix-style builds only**; `cabal.project` (+ `cabal.project.local`) is the source of
  truth. Do not use stack or invoke `ghc` directly. cabal-install ≥ 3.14.2 recommended.

## Build / test / bench / run

`cabal.project` sets the `+examples` flag and `cabal.project.local` adds `+artifact` and
enables tests+benchmarks, so locally every component below is buildable.

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

Prefer HLS (via the haskell skill) for iterating; it is far faster than a full `cabal build`
for typecheck / hover / go-to-definition / find-references / rename.

### Tests — tasty + tasty-discover (property tests via `falsify`)

`test/Main.hs` is only the `tasty-discover` driver (`-optF --tree-display`); real tests live
in `test/**/*Spec.hs`, in modules mirroring the source tree. Filter with tasty's `-p`:

```bash
cabal test pure-borrow-test --test-options='-p "Lifetime"'   # run a subset by pattern
```

`test/Control/Monad/Borrow/Pure/Lifetime/TypingCases.hs` holds type-level (compile-time)
constraint checks, not runtime assertions.

### Benchmarks & profiling

Benchmarks are `tasty-bench` executables; pass options through cabal, e.g.
`cabal bench qsort-bench --benchmark-options='--csv bench-results/qsort.csv -j1 --time-mode=wall +RTS -N -s'`.
For profiled runs use the dedicated project file: `cabal --project-file=cabal-bench.project ...`
(enables `-fprof-late`/`-fprof-auto` profiling). CSV/plots land in `bench-results/`.

### Artifact runner

With `+artifact` (on locally), `cabal run artifact-runner -- <bench|demo|quick> ...` drives
the paper's benchmarks/demos and produces CSVs.

## Architecture

Everything lives under `src/`. The public entry point is `Control.Monad.Borrow.Pure`.

### Module-suffix convention (strict — respect the boundary)

- **No suffix = safe public API.** User/application code should import only these
  (`Control.Monad.Borrow.Pure`, `Data.Vector.Mutable.Linear.Borrow`, `Data.Ref.Linear.Borrow`,
  `Control.Concurrent.DivideConquer.Linear`, `…/Lifetime.hs`, `…/Lifetime/Token.hs`, …).
- **`.Internal` = real definitions, `{-# OPTIONS_HADDOCK hide #-}`.** Exposed (so other
  modules can import) but omitted from docs; contains the actual newtypes and the
  `Unsafe*` constructors.
- **`.Unsafe` = trusted escape hatches** (`unsafeSystemIOToBO`, `unsafeCastBO`, `Alias(..)`,
  `LinearOnly(..)`, …). Every use is a proof obligation that lifetime/linearity invariants
  hold. Only the data-structure and scheduler modules should import these.
- **`Utils` / `Utils/**` = truly private** (`other-modules`, not exposed).
- **`Experimental.*`** modules are exposed but unstable (`Borrows`, `Loop`, `Reborrowable`,
  and `Data.Record.Linear.Borrow.Experimental.*` record-splitting).

### Core: the `BO` monad — `src/Control/Monad/Borrow/Pure/BO/Internal.hs`

`BO α a` is morally a **linear `ST` monad** whose phantom state token is indexed by a
*lifetime* `α` (kind `Lifetime`) instead of `ST`'s `s`. Run with `runBO`/`runBO_`/`srunBO`,
which require a `Linearly` witness. Purity of parallelism (`parBO`) is provided by trusted
`unsafe*` primitives that fork real IO and `evaluate` into `MVar`s — observationally pure
thanks to the phantom-state + linearity discipline.

Borrow types are all one zero-cost representation, `Alias ak α a`:
- `Mut α a` — mutable/exclusive borrow (affine + `LinearOnly`).
- `Share α a` — shared/read-only borrow (`Dupable`+`Movable`, freely copied/dropped).
- `Lend α a` — the capability to `reclaim` the original once the lifetime ends.
- Intro/elim: `borrow`, `share`, `reborrow`, `reclaim`/`reclaim'`, plus `split*` machinery
  (generic, via `Generics.Linear`) that turns a borrow of a structure into a structure of
  borrows.

### Lifetimes — `src/Control/Monad/Borrow/Pure/Lifetime/`

- `Lifetime/Internal.hs` — the type-level algebra: `Lifetime = Al Nat | (:/\) | Static`, a
  free bounded lower-semilattice; `/\` is meet. The outlives relation `(<=)`/`(>=)` is a
  layered class hierarchy with explicit GADT witnesses and `INCOHERENT` instances that
  hand-implement transitivity/associativity of subtyping (no typechecker plugin).
- `Lifetime/Token/Internal.hs` — zero-cost value-level tokens (`Now`, `EndToken`/`End`,
  `newLifetime`), the `After α a` finalizer monad, and the linearity witnesses (`Linearly`,
  `linearly`, `LinearOnly`). Several `NOINLINE`/`noinline` annotations here deliberately
  defeat CSE / full-laziness that would otherwise duplicate linear tokens — **do not
  "clean these up".**

### Parallel divide-and-conquer — `src/Control/Concurrent/DivideConquer/Linear.hs`

A borrow-safe **work-stealing/work-sharing** divide-and-conquer skeleton. The user supplies a
`DivideConquer` record (`initialise`/`divide`/`conquer`); `divide` splits a `Mut` borrow into
a traversable of disjoint sub-borrows in the parent lifetime. Substrate:
- `src/Control/Concurrent/Queue/ChaseLev.hs` — lock-free Chase-Lev deque.
- `…/DivideConquer/Utils/QueuePool.hs` — the hybrid steal/share scheduler.
- `…/Utils/Semaphore.hs`, `…/Utils/OnceChan/Linear.hs` — linear sync + one-shot channels.

Worked examples exported here: `qsortDC`/`fftDC` (scheduler-backed). Simpler baselines:
`sequentialDivideAndConquer'`, `naiveDivideAndConquer'` (fork-per-node via `parBO`).

### Borrow-aware mutable vector — `src/Data/Vector/Mutable/Linear/Borrow.hs`

`Vector a` owns each element *linearly* (so it can nest other mutable resources); it is
`LinearOnly` and intentionally **not** `Copyable`. `splitAt` splitting a borrow into two
disjoint sub-borrows is the key primitive for parallelism. Includes a demonstrative in-place
parallel `qsort` (budgeted `parBO`; the heavier version is `qsortDC` above).

## Conventions & workflow

- **`(<>)` over `(++)`** for all concatenation, including lists and strings.
- **No `package.yaml`/hpack** — edit `pure-borrow.cabal` directly, then run cabal-gild. The
  `exposed-modules`/`other-modules` lists are generated by `-- cabal-gild: discover` pragmas
  (with `--exclude` globs for `Utils`), so after adding/removing/renaming a module re-run
  cabal-gild instead of hand-editing the list.
- **Format before compiling**, enforced by CI and by the shared PostToolUse hooks in
  `.agents/hooks/` (wired from both Claude and Codex config):
  - `.hs`/`.lhs`/`.hsig` → **fourmolu** (config `fourmolu.yaml`; CI pins fourmolu 0.18.0.0).
  - `.cabal`/`cabal.project*` → **cabal-gild** (≥ 1.6). Use the integration in your agent
    (the `haskell-format` / `haskell-cabal-gild` skills, or the hooks) rather than
    reformatting by hand.
- **Strict warnings** (`common defaults` in `pure-borrow.cabal`): `-Wall -Wcompat
  -Wunused-packages …`. `-Wunused-packages` means a now-unused `build-depends` entry breaks
  the build — drop deps you stop using.
- **Flags:** `examples` (demo executables + `demo-impl` lib) and `artifact` (artifact-runner)
  are `manual`, default `False`; enabled locally via `cabal.project`/`cabal.project.local`.
- CI (`.github/workflows/haskell.yml`) runs a GHC matrix (9.10.3/9.12.4/9.14.1 via
  `ci/configs/*.project`): build, all test suites, `cabal check`, Haddock-for-Hackage, and a
  fourmolu check.
- **You do not have to test against every GHC version** — that is CI's responsibility, and
  the per-GHC project files under `ci/configs/` exist precisely for it. Locally, work with
  the pinned `ghc-9.12.4` (from `cabal.project.local`); only reach for another version via
  `cabal --project-file=ci/configs/ghc-<ver>.project …` when reproducing a version-specific
  failure.
- Trust the **LSP before a full build**, and prefer **local docs/source over remote** (read
  the cabal store / `dist-newstyle` / repo-cache before hitting Hackage or remote Hoogle) to
  avoid loading Haskell's public infrastructure.
- **Commits** must follow **Conventional Commits** style
  (https://www.conventionalcommits.org) — a `type(scope): summary` subject (e.g. `perf(fft):
  …`, `fix: …`), with a brief summary of the changes in the message body. You **must** make
  the coauthorship explicit by appending a `Co-authored-by:` trailer.
  - **Do not include internal session information** in commit messages. No agent/session
    metadata trailers such as `Claude-Session:` (or links to session transcripts), and no
    tool-internal identifiers. Keep the message about the change itself; the only
    agent-related trailer allowed is `Co-authored-by:`.

## Agent configuration

- `AGENTS.md` is the single source of truth for repository instructions; `CLAUDE.md` is a
  symlink to it so Claude and Codex read the same policy.
- Haskell tooling comes from two Claude Code marketplaces, and is mirrored for Codex so both
  agents get the same capabilities:
  - **Claude Code** — `.claude/settings.json` enables the marketplace plugins:
    `haskell-lsp-plugin`, `haskell-haddock-skill`, `haskell-skill`
    (`konn-haskell-claude-tools`, https://github.com/konn/haskell-claude-marketplace) and
    `hoogle` (`claude-hoogle`, https://github.com/m4dc4p/claude-hoogle). Add them with
    `/plugin marketplace add …` then `/plugin install …@…` if not already present.
  - **Codex** — the same skills are committed under `.codex/skills/` (`haskell`, `haddock`,
    `haskell-format`, `haskell-cabal-gild`, `hoogle`) with their helper scripts, so Codex
    uses them without the Claude plugin cache.
  - **Formatting hooks** are shared, not plugin-specific: `.agents/hooks/*.sh` run fourmolu
    and cabal-gild on edited files, wired from `.claude/settings.json` (PostToolUse) and
    `.codex/hooks.json`. The marketplace's own `haskell-format-skill`/`haskell-cabal-gild-skill`
    plugins are therefore left **disabled** in `.claude/settings.json` to avoid double
    formatting — the shared hooks cover both agents.
- Shared automation belongs under `.agents/`. Agent-specific directories (`.claude/`,
  `.codex/`) should hold only configuration, symlinks, or adapters. Prefer symlinks over
  copied files when both agents can consume the same artifact safely.
