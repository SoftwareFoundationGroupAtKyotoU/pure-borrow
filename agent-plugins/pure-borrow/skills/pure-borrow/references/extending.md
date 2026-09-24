# Extending pure-borrow: new borrowable data structures

Read this only when implementing a new owner type, a new borrow operation, or a new scope combinator on top of pure-borrow.
Application code should import only the safe modules (no suffix); the linear-haskell skill's `references/implementing-primitives.md` applies here as well.

## Module conventions

- **No suffix**: the safe public API.
- **`.Internal`**: the real definitions and `Unsafe*` constructors, hidden from Haddock; some escape hatches, such as `unsafeCastBO`, live only here.
- **`.Unsafe`**: trusted escape hatches.
  `Control.Monad.Borrow.Pure.BO.Unsafe` exports `BO (..)`, `Alias (..)`, `unsafeUnalias`, `unsafeMapAlias`, `unsafeCastAlias`, `reviveAlias`, and the conversions between `BO` and `IO`, linear `IO`, and `ST`; `Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe` exports token constructors, `LinearOnly`/`LinearOnlyWitness`, and the hidden capability class `Ended`; the public `End` is a sealed synonym.
  Import only the names you need, and treat every use as a proof obligation: write down the invariant it relies on and why it holds.

## An owner type

A typical mutable container:

- is a `newtype` (or `data`) over the underlying mutable storage, with `type role … nominal` so its parameter cannot be coerced, and with its constructor hidden;
- is allocated only through functions taking `Linearly %1 ->`, so that every owner is linearly bound, which also makes it `LinearOnly` (define the instance with `LinearOnlyWitness` from the token `.Unsafe` module);
- has `Consumable` (an element-owning container consumes every element, so it needs `Consumable a`);
- has `Clone` if it can be deep-copied inside `BO`, cloning each element through its own `Clone a` instance;
  never use `dup` on an element held through a `Share`: it consumes its argument, and its laws do not identify which result is the original;
- **bans `Copyable` and `Movable`** with `Unsatisfiable`, since a copy or move would let unrestricted code alias its mutable state;
- if it is element-owning, materialises into unrestricted form only with `Movable a`, calling `move` on every element (a bare, unused constraint is not enough); a non-element-owning container may freeze its storage in O(1);
- is disposable: an element-owning container whose elements may be non-`Movable` needs a `Consumable` instance, or users can never get rid of it.

## Operations on borrows

- Take `Borrow bk α X` (read-only operations, for both `Mut` and `Share`) or `Mut α X` (mutations), and run in `BO β` with an `(α >= β)` constraint, returning the borrow alongside the result.
- Perform effects with `unsafeSystemIOToBO` (or the ST/linear-IO conversions), so that the `BO` state token orders them.
  A pure read through a borrow bypasses the token: GHC may serve it from an earlier evaluation, and nothing stops a captured `Share` from being read after its lifetime has ended, so make every read of state that can change a `BO` action.
- A mutating operation on an element-owning container must hand back what it displaced (`set` returns the old element), because linear elements cannot be dropped.
- Sub-borrow operations (`splitAt`, element borrows, `split`) must produce **disjoint** pieces: two live `Mut`s must never reach overlapping memory, a `Share` must never outlive the exclusivity it was carved from, and a borrow must never be placed in a nonlinear field.
- `copy` implementations must finish copying (component copies included) before returning in WHNF.
- A clone must finish its independent storage copy inside the returned `BO` action, before the borrow's lifetime ends.
  Allocate and fill through the state thread, or use a `Linearly` token obtained there with a copying function protected by both `NOINLINE` and `noinline`.
  Merely forcing a token-independent pure copy can let GHC merge repeated clones of the same borrow.
  A container with GC-owned elements copies its storage and shares those elements without `Clone a`; an element-owning container clones each element through `Clone a` and never consumes the original.

## Scope combinators and delimiters

- A delimiter that runs a continuation over a sublifetime and then gives the caller back the borrow it was given must return it through `reviveAlias` (or `reviveAliases` for bundles), never as the caller's own occurrence.
  Header reads now go through the state token, but keep this identity barrier: restored borrow occurrences must remain distinct across scopes, including callers using the lower-level API.
  See `Note [Restoring a borrow must break its Core identity]` in `Control.Monad.Borrow.Pure.BO.Internal`.
- The same obligation applies to each method of a `Reborrowable` instance independently.
- A `Lend` must be neither duplicated nor dropped, and `reclaim` must not run before its lifetime ends.
  A delimiter discharging `After` obtains its end token from the state thread with the supplied trusted helpers; it must not build a constant `UnsafeEnd` beside the result.
- `Linearly`, `Now`, and `EndToken` carry an intentionally lazy field that keeps their identity unknown to GHC.
  Match a linear token using `UnsafeLinearlyToken`, `UnsafeNowToken`, or `UnsafeEndToken`, passing the field on when converting one token into another.
  The old names are pattern synonyms and cannot match linear values.
  Never replace these types with nullary constructors or newtypes; a function returning two tokens must be `NOINLINE` and applied through `noinline`, since two equal-looking tokens can let GHC merge allocations.
- Public `(<=)`, `End`, and `(<:)` constraints are sealed synonyms; changing their hidden instances is a lifetime-soundness change, not an application extension point.

## Effects and inlining

- Every binding whose body reaches `unsafePerformIO`, including `consume` methods that traverse elements, must be protected from inlining and worker/wrapper: use `OPAQUE`, or `NOINLINE` together with a right-hand side wrapped in `GHC.Exts.noinline`.
  Plain `NOINLINE` still allows worker/wrapper, after which GHC can merge two allocations into one; `INLINE` on such a binding is a bug.
- Make each effect mention an argument of the enclosing function (the `Linearly` token or the input), or GHC's full laziness may float it out into a single shared top-level value.
- Prefer `unsafePerformIO`, which runs `noDuplicate#`, over `unsafeDupablePerformIO` and bare `runRW#`; the dupable forms are only acceptable with an argument that the thunk can never be entered twice.
  Never use `unsafeThaw`-then-`unsafeFreeze` tricks or `runST . unsafeIOToST` to fake purity.
- The `NOINLINE`/`noinline` annotations on the library's own token plumbing (`linearly`, `askLinearly`, `dup2` of `Linearly`, `withLinearly`) are deliberate: do not "clean them up".

## Testing

- Write "must not typecheck" cases for every new API surface: using a `Mut` twice, letting a borrow escape its scope, overlapping sub-borrows, copying a mutable type.
  Most type errors can be deferred with `-fdefer-type-errors` and observed at run time, including multiplicity mismatches between arrow types, but usage errors (`… arising from multiplicity of 'x'`) are reported at compile time even when deferred; keep those in a separate compile-fail test.
- Test at the optimisation level you ship (`-O2`); inlining and CSE hazards do not show at `-O0`, and a result that differs between `-O0` and `-O2` is a bug.
- Have someone who did not write the code try to construct a well-typed program that breaks aliasing-xor-mutability; a green test suite does not establish soundness.
