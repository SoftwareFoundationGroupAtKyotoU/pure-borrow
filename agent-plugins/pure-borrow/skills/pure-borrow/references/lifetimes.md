# Lifetimes and sublifetimes

## The algebra

- `Lifetime` is a kind.
  Its values form a free bounded lower semilattice: atomic lifetimes created by `runBO` and the scope combinators, the meet `α /\ β` (the longest lifetime shorter than both), and `Static`, which never ends.
- `α <= β` means `α` is a sublifetime of `β`; `β >= α` reads "`β` outlives `α`".
  GHC can derive `α <= α`, `α /\ β <= α`, `α /\ β <= β`, `α <= Static`, `α <= β /\ γ` from `α <= β` and `α <= γ`, and reassociations of `/\`.
- `(<=)`, `End`, `(<:)`, and `Subtype1` are classes whose instances the library supplies; write none of your own, except `(<:)` and `Subtype1` for a type of your own, derived as in "Subtyping" below.
  Use the library's supplied constraints and `upcast`/`upcast1` instead of manufacturing lifetime or subtyping evidence.
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

A type of your own gets the relation componentwise by deriving it via `Generically`, once it has the `Generic` instance of linear-generics (`deriveGeneric ''Two`):

```haskell
import Generics.Linear (Generically (..)) -- the constructor too, not only the type

deriving via Generically (Two b) instance (a <: b) => Two a <: Two b
```

Both types must apply the same type constructor, differ in at most eight trailing arguments (from the first differing argument to the last), and share one parametric `Generic` instance that only takes a value apart and puts it back, as `deriveGeneric` generates.
Distinct type constructors and separate `Generic` instances on one type constructor are rejected.

`Data.Coerce.Directed` (re-exported by `Control.Monad.Borrow.Pure.BO`) also provides `Subtype1` — the lifting of `(<:)` to unary type constructors, analogous to `Eq1` — and `upcast1 :: (Subtype1 f g, a <: b) => f a %1 -> g b`.
The library supplies `Subtype1` for `BO α`, `Share α`, `Lend α`, `After α`, `FUN p c`, `[]`, `Maybe`, `NonEmpty`, `Identity`, `(,) c`, and `Either c`.
`Mut` is invariant in the borrowed type, so `Subtype1 (Mut α) (Mut β)` is rejected; for a type whose parameter is under a `Mut`, derive `(<:)` via `Generically` instead.
A functor gets both `Subtype1` and `(<:)` via `Generically1` once it has one parametric `Generic1` instance (from `deriveGenericAnd1`), covering one functor or up to eight trailing arguments before its last parameter:

```haskell
import Data.Coerce.Directed (Subtype1)
import Generics.Linear (Generically1 (..))
import Generics.Linear.TH (deriveGenericAnd1)

data View α a = View (Share α a) Int

deriveGenericAnd1 ''View

deriving via Generically1 (View β) instance (α >= β) => Subtype1 (View α) (View β)
deriving via Generically1 (View β) b instance (α >= β, a <: b) => View α a <: View β b
```

`Subtype1` is for a type constructor applied to its parameter inside another `Generically1` derivation and for `upcast1`; it does not imply `(<:)` without the `Generically1 (View β) b` (or `Generically`) instance.

In the module that defines a newtype, `deriving via AsCoercible Meters instance Int <: Meters`, with `AsCoercible (..)` imported from `Data.Coerce.Directed`, lets modules that cannot see its constructor upcast `Int` to `Meters`.
A derivation needs the via type and the target to be representationally equal where it is written, which is why the constructor must be in scope.
`genericUpcast` converts between two types whose `Generic` representations have the same shape and names, field by field, without a `(<:)` instance: it rebuilds the value through `from` and `to`, so it also takes two applications of a type whose first differing argument is ninth from the end or further.

## Ending lifetimes: `After` and `End`

- `reclaim :: End α => Lend α a %1 -> a` needs `End α`, which exists only inside an `After α` value, i.e. once `α` has ended.
- `pureAfter :: (End α => a) %1 -> BO α (After α a)` packages such a finaliser as the result of a `BO α` computation.
- `After α` is a control applicative and monad, so several lenders combine as `(,) Control.<$> reclaim' l1 Control.<*> reclaim' l2`.
- The primed scope combinators (`reborrowing'`, `sharing'`) let the continuation return `After β r`, for resources borrowed *inside* the scope that must be reclaimed when the scope ends.
- Inside `After`, work with lenders and owned values: a borrow of the lifetime that has ended is of no use there.
  It cannot be returned: `After α (Share α a)` is a valid type, but the function that delimits `α`, such as `runBO` or `srunBO`, rejects a result that mentions `α`.
  A `Share` that the finaliser merely captures, or that leaves hidden in an existential type, is accepted, but its contents can no longer be read: every read is a `BO` action in a lifetime that the ended one outlives, and none can run any more.
  `copy` still works on it, and safely, since a `Copyable` type has no mutable state.
  A borrow of another lifetime that is still running is unaffected.

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
`nowStatic :: BO α (Now Static)` is an action in `Control.Monad.Borrow.Pure.BO`, not an unrestricted token constant; bind it inside `BO` when an explicit `Now Static` is needed.

## Reading lifetime errors

| Symptom | Likely cause | Fix |
| --- | --- | --- |
| "`β` would escape its scope" / "Couldn't match type `β0` with …" in a scope | A borrow at the scope's private lifetime is being returned | Return only unrestricted data, the restored outer borrow, or an `After β` finaliser |
| `No instance for 'β <=!! α'` / `Could not deduce 'γ <=!! α'` | Lifetimes of the borrow and of the `BO` do not line up, or transitivity would be needed | Add `α >= β` to the signature, not the `<=!!` that GHC suggests: make the helper polymorphic with `(α >= β) =>`, `upcast`/`subShare` explicitly, or restate with `/\` |
| `No instance for 'End α'` | `reclaim` used outside `After` | Move it into `pureAfter`, or use `reclaim'` |
| `Couldn't match type 'α' with 'β' arising from a use of 'upcast'` at a type of your own | The type has no `(<:)` instance, so `upcast` falls back to `Coercible`, under which a lifetime is nominal | Derive `(<:)` via `Generically` or `Generically1`, as in "Subtyping" above; a `Subtype1` instance alone does not give it |
| `Overlapping instances for γ <= α0`, or an ambiguity error that suggests `AllowAmbiguousTypes` | GHC cannot determine a lifetime: an intermediate one, one in a local binding with no signature, or one that occurs only in constraints | Name it with a type application, as in `step @α @γ (step @α @α s)`, or add a signature; drop constraints on a lifetime that does not occur in the type |
| Errors about impredicative instantiation | Passing a rank-2 continuation through `$` or a data constructor | Enable `ImpredicativeTypes`, use `BlockArguments` instead of `$` |
