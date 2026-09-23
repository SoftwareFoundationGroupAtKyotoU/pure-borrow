# Extending pure-borrow: new borrowable data structures

Read this only when implementing a new owner type, a new borrow operation, or a new scope combinator on top of pure-borrow.
Application code should import only the safe modules (no suffix); the linear-haskell skill's `references/implementing-primitives.md` applies here as well.

## Module conventions

- **No suffix**: the safe public API.
- **`.Internal`**: the real definitions and `Unsafe*` constructors, hidden from Haddock; some escape hatches, such as `unsafeCastBO`, live only here.
- **`.Unsafe`**: trusted escape hatches.
  `Control.Monad.Borrow.Pure.BO.Unsafe` exports `BO (..)`, `Alias (..)`, `unsafeUnalias`, `unsafeMapAlias`, `unsafeCastAlias`, `reviveAlias`, and the conversions between `BO` and `IO`, linear `IO`, and `ST`; `Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe` exports the constructors of `Linearly`, `LinearOnly`, `LinearOnlyWitness`, `Now`, `End`, and `EndToken`.
  Import only the names you need, and treat every use as a proof obligation: write down the invariant it relies on and why it holds.

## An owner type

A typical mutable container:

- is a `newtype` (or `data`) over the underlying mutable storage, with `type role … nominal` so its parameter cannot be coerced, and with its constructor hidden;
- is allocated only through functions taking `Linearly %1 ->`, so that every owner is linearly bound, which also makes it `LinearOnly` (define the instance with `LinearOnlyWitness` from the token `.Unsafe` module);
- has `Consumable` (an element-owning container consumes every element, so it needs `Consumable a`);
- has `Clone` if it can be deep-copied inside `BO`, cloning each element through its own `Clone a` instance;
  if you duplicate elements with `dup` instead, remember that it consumes the element you only hold through a `Share` and that one of its results may be the original storage (for `Ref` and linear-base's `Array` it is the first), so keep only the independent copy;
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

## Scope combinators and delimiters

- A delimiter that runs a continuation over a sublifetime and then gives the caller back the borrow it was given must return it through `reviveAlias` (or `reviveAliases` for bundles), never as the caller's own occurrence.
  Some header reads (sizes, buffer pointers behind a `Ref`) do not go through the state token, so GHC may otherwise serve a post-scope read from a pre-scope one, across every write the scope performed.
  See `Note [Restoring a borrow must break its Core identity]` in `Control.Monad.Borrow.Pure.BO.Internal`.
- The same obligation applies to each method of a `Reborrowable` instance independently.
- A `Lend` must be neither duplicated nor dropped, and `reclaim` must not run before its lifetime ends.

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
