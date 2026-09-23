# Lifetimes and sublifetimes

## The algebra

- `Lifetime` is a kind.
  Its values form a free bounded lower semilattice: atomic lifetimes created by `runBO` and the scope combinators, the meet `α /\ β` (the longest lifetime shorter than both), and `Static`, which never ends.
- `α <= β` means `α` is a sublifetime of `β`; `β >= α` reads "`β` outlives `α`".
  GHC can derive `α <= α`, `α /\ β <= α`, `α /\ β <= β`, `α <= Static`, `α <= β /\ γ` from `α <= β` and `α <= γ`, and reassociations of `/\`.
- There is no type-checker plugin: the relation is implemented by layered `INCOHERENT` instances.
  Transitivity (`α <= β`, `β <= γ` ⊢ `α <= γ`) and monotonicity are **not** derived.
  If a constraint that "obviously" holds is rejected, restate the signature in terms of `/\` so that one of the rules above applies directly, or make the helper polymorphic in the lifetimes (below).

A lifetime is the lifetime-indexed analogue of the `s` in `ST s`, refined with this ordering.

## Where lifetimes come from

- `runBO lin (bo :: forall α. BO α (After α a))` runs `bo` in a fresh lifetime `α` and then evaluates the `After α` result once `α` has ended.
- `borrowM` inside `BO α` yields `Mut α a` and `Lend α a` for that same `α`.
- The scope combinators (`reborrowing`, `sharing`, `srunBO`, …) quantify over a fresh `β` and hand out borrows at `β /\ α`, i.e. at some sublifetime of `α`.

Read any `forall β. … (β /\ α) …` as "for every sublifetime of `α`".
Quantifying over all `β` and taking the meet expresses `forall β <= α` without needing bounded quantification in the type checker.

## Outlives constraints on operations

Container operations are usually polymorphic in the computation's lifetime:

```haskell
VL.modify :: (α >= β) => Int -> (a %1 -> a) %1 -> Mut α (VL.Vector a) %1 -> BO β (Mut α (VL.Vector a))
VL.copyAt :: (Copyable a, α >= β) => Int -> Share α (VL.Vector a) -> BO β (Ur a)
```

A borrow valid for `α` may be used by any computation running during a sublifetime `β` of `α`.
Write your own helpers the same way, so they work both in the ambient lifetime and inside scopes:

```haskell
bumpHead :: (α >= β) => Mut α (VL.Vector Int) %1 -> BO β (Mut α (VL.Vector Int))
bumpHead = VL.modify 0 (+ 1)
```

When a helper is only ever used at a single lifetime, `Mut α x %1 -> BO α r` is simplest.
Some modules (for example the hash map) use one lifetime for both; call them in a scope whose `BO` lifetime matches the borrow's.

## Subtyping

`upcast :: (a <: b) => a %1 -> b` coerces along the subtyping relation, which lifts `<=` to other types:

| Type | Subtype when |
| --- | --- |
| `Mut α a <: Mut β a` | `α >= β` (a longer-lived exclusive borrow can be used as a shorter one) |
| `Share α a <: Share β b` | `α >= β`, `a <: b` |
| `BO α a <: BO β b` | `α >= β`, `a <: b` |
| `Lend α a <: Lend β b` | `α <= β`, `a <: b` (reclaiming later is always fine) |
| `After α a <: After β b` | `α <= β`, `a <: b` |

`subShare :: (α >= β) => Share α a -> Share β a` is the inference-friendly special case for shared borrows.
Type applications often help `upcast` pick the target lifetime.

## Ending lifetimes: `After` and `End`

- `reclaim :: End α => Lend α a %1 -> a` needs `End α`, which exists only inside an `After α` value, i.e. once `α` has ended.
- `pureAfter :: (End α => a) %1 -> BO α (After α a)` packages such a finaliser as the result of a `BO α` computation.
- `After α` is a control applicative and monad, so several lenders combine as `(,) Control.<$> reclaim' l1 Control.<*> reclaim' l2`.
- The primed scope combinators (`reborrowing'`, `sharing'`) let the continuation return `After β r`, for resources borrowed *inside* the scope that must be reclaimed when the scope ends.
- Inside `After`, use only lenders and owned values, never a borrow of the lifetime that is ending: a captured `Share` still typechecks there but can observe stale data.

## Running a phase in a sublifetime

```haskell
srunBO  :: (forall α. BO (α /\ β) (After α a)) %1 -> BO β a
srunBO_ :: (forall α. BO (α /\ β) a) %1 -> BO β a
```

Use `srunBO` to borrow something only for part of a longer computation and get the owned value back in the outer lifetime.
A `borrowM` inside the scope yields a `Lend (α /\ β) a`, while `srunBO` expects an `After α a`, so return `upcast (reclaim' lend)` rather than `pureAfter (reclaim lend)` (which fails with `Couldn't match type 'α' with 'α /\ β'`):

```haskell
phase :: VL.Vector Int %1 -> BO β (VL.Vector Int)
phase vec = srunBO Control.do
  (mvec, lend) <- borrowM vec
  mvec <- VL.modify 0 (+ 1) mvec
  Control.pure (consume mvec)
  Control.pure (upcast (reclaim' lend))
```

The same `upcast (reclaim' lend)` works for resources borrowed inside `reborrowing'` and `sharing'`.
The lower-level token API (`Now`, `newLifetime`, `endLifetime`, `scope_`, `sexecBO`) and the manual reassociation helpers (`assocLBO`, `assocRBO`, `assocBorrowL`, …) in `Control.Monad.Borrow.Pure.BO` are rarely needed in application code.

## Reading lifetime errors

| Symptom | Likely cause | Fix |
| --- | --- | --- |
| "`β` would escape its scope" / "Couldn't match type `β0` with …" in a scope | A borrow at the scope's private lifetime is being returned | Return only unrestricted data, the restored outer borrow, or an `After β` finaliser |
| `No instance for (α <= β)` / `(β >= α)` | Lifetimes of the borrow and of the `BO` do not line up, or transitivity would be needed | Make the helper polymorphic with `(α >= β) =>`, `upcast`/`subShare` explicitly, or restate with `/\` |
| `No instance for (End α)` | `reclaim` used outside `After` | Move it into `pureAfter`, or use `reclaim'` |
| Ambiguous lifetime variables | A local binding with no signature | Add a signature, `ScopedTypeVariables`, or type applications |
| Errors about impredicative instantiation | Passing a rank-2 continuation through `$` or a data constructor | Enable `ImpredicativeTypes`, use `BlockArguments` instead of `$` |
