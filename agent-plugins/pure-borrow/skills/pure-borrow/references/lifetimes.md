# Lifetimes and sublifetimes

## The algebra

- `Lifetime` is a kind.
  Its values form a free bounded lower semilattice: atomic lifetimes created by `runBO` and the scope combinators, the meet `α /\ β` (the longest lifetime shorter than both), and `Static`, which never ends.
- `α <= β` means `α` is a sublifetime of `β`; `β >= α` reads "`β` outlives `α`".
  GHC can derive `α <= α`, `α /\ β <= α`, `α /\ β <= β`, `α <= Static`, `α <= β /\ γ` from `α <= β` and `α <= γ`, and reassociations of `/\`.
- `(<=)`, `End`, and `(<:)` are classes whose instances the library supplies; write none of your own, except `(<:)` for a type of your own, derived as in "Subtyping" below.
  Use the library's supplied constraints and `upcast` instead of manufacturing lifetime or subtyping evidence.
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

A type of your own gets the relation field by field from `deriveSubtype`, a Template Haskell macro in `Data.Coerce.Directed.Unsafe` that reads the declaration of the type:

```haskell
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-} -- and LinearTypes, which implies MonoLocalBinds
import Data.Coerce.Directed.Unsafe (deriveSubtype)

data Two a = Two a a

deriveSubtype ''Two -- declares (a <: a') => Two a <: Two a'
```

It treats each field as covariant, so never use it when defining a mutable data structure: if the type holds pure but mutable data of your own, this variance can violate soundness.
Splice it after the declarations of the type and of every type in its recursive group; its Haddock lists the extensions it needs and the types it refuses.
A parameter that no field mentions stays fixed in the instance, but `upcast` still converts whatever `coerce` converts, so give such a parameter a nominal role if it must stay fixed.
Deriving `(<:)` via `Generically`, as 0.1.0.0 allowed, is rejected: it trusted a `Rep` that anyone can write by hand.

In the module that defines a newtype, `deriving via AsCoercible Meters instance Int <: Meters`, with `AsCoercible (..)` imported from `Data.Coerce.Directed`, lets modules that cannot see its constructor upcast `Int` to `Meters`.
A derivation needs the via type and the target to be representationally equal where it is written, which is why the constructor must be in scope.
`genericUpcast` converts between two instantiations of one data type, such as `Two a` and `Two b`, without any instance.

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
`nowStatic :: BO α (Now Static)` is an action in `Control.Monad.Borrow.Pure.BO`, not an unrestricted token constant; bind it inside `BO` when an explicit `Now Static` is needed.

## Reading lifetime errors

| Symptom | Likely cause | Fix |
| --- | --- | --- |
| "`β` would escape its scope" / "Couldn't match type `β0` with …" in a scope | A borrow at the scope's private lifetime is being returned | Return only unrestricted data, the restored outer borrow, or an `After β` finaliser |
| `No instance for 'β <=!! α'` / `Could not deduce 'γ <=!! α'` | Lifetimes of the borrow and of the `BO` do not line up, or transitivity would be needed | Add `α >= β` to the signature, not the `<=!!` that GHC suggests: make the helper polymorphic with `(α >= β) =>`, `upcast`/`subShare` explicitly, or restate with `/\` |
| `No instance for 'End α'` | `reclaim` used outside `After` | Move it into `pureAfter`, or use `reclaim'` |
| `Overlapping instances for γ <= α0`, or an ambiguity error that suggests `AllowAmbiguousTypes` | GHC cannot determine a lifetime: an intermediate one, one in a local binding with no signature, or one that occurs only in constraints | Name it with a type application, as in `step @α @γ (step @α @α s)`, or add a signature; drop constraints on a lifetime that does not occur in the type |
| Errors about impredicative instantiation | Passing a rank-2 continuation through `$` or a data constructor | Enable `ImpredicativeTypes`, use `BlockArguments` instead of `$` |
