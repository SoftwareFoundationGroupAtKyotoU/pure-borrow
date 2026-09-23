---
name: linear-haskell
description: >-
  Write, review, and debug Linear Haskell (GHC LinearTypes with linear-base) on GHC 9.10.3 and 9.12.4+.
  Use it when a module enables LinearTypes, when a package depends on linear-base or linear-generics, or when code contains linear arrows (%1, %m, ⊸), Ur, Consumable, Dupable, Movable, Prelude.Linear, Control.Functor.Linear, linear let (let %1), or QualifiedDo blocks such as Control.do.
  Use it also to fix errors like "Couldn't match type 'Many' with 'One'" and to design mutable, in-place, parallel, or resource-safe APIs and data types with linear types.
license: BSD-3-Clause
compatibility: >-
  Haskell projects on GHC 9.10.3 or 9.12.4+ (9.14 adds record-field multiplicities) using linear-base >= 0.5.
metadata:
  author: Hiromi Ishii
---

# Linear Haskell

This skill covers linear types as implemented by GHC (`LinearTypes`) and the `linear-base` library.
Linear Haskell moves fast, so prefer the version notes here over older blog posts: several restrictions described in 2020–2023 material (no linear `let`, no linear `if`) are gone.

Read the relevant reference before writing non-trivial code:

- [references/syntax.md](references/syntax.md) — binding linear results, `let %1`, strict patterns, shadowing, `QualifiedDo`, conditionals, GHC version differences.
- [references/data-and-classes.md](references/data-and-classes.md) — `Ur`, `Consumable`/`Dupable`/`Movable`, designing data types, generic deriving, banning instances.
- [references/functors-and-effects.md](references/functors-and-effects.md) — data vs. control functors, linear monads, `UrT`, `RIO`, allocation styles.
- [references/troubleshooting.md](references/troubleshooting.md) — error messages and their real causes.
- [references/implementing-primitives.md](references/implementing-primitives.md) — only when implementing a linear API on top of `unsafe*` primitives.
- [references/further-reading.md](references/further-reading.md) — papers, the GHC User's Guide, and background articles.

## The one invariant

`f :: a %1 -> b` is a promise made by the **callee**:

> If the application `f x` is consumed exactly once, then `x` is consumed exactly once.

It constrains how `f` uses its argument, not how callers obtain it: an unrestricted value can always be passed to a linear function.

"Consumed exactly once" is defined by the shape of the value:

- A value of a primitive type (`Int#`, `Double`, …) is consumed by evaluating it to a value.
- A function is consumed by applying it to one argument and consuming the result exactly once.
- A value of an algebraic data type is consumed by evaluating it to WHNF and consuming each of its **linear** fields exactly once; its nonlinear fields may be used any number of times, including zero.

If evaluation raises an exception, the result was never consumed exactly once, so the invariant holds vacuously.
Linearity therefore does **not** guarantee that resources are released when an exception is thrown.

In practice, every linearly bound variable must be used exactly once in every branch: returned, passed to a linear function whose result is itself used once, pattern-matched with each linear field used once, or applied as a function.

## Multiplicity as ownership

A useful mental model reads multiplicities as ownership:

- `a -> b`: the argument is **GC-owned**.
  The garbage collector takes responsibility for releasing it for the rest of the computation, and it may have many owners.
- `a %1 -> b`: the function treats the argument as owned **exclusively by exactly one owner**, itself, and takes responsibility for releasing it.

This is a model, not a statement about memory: almost every Haskell value still lives on the GC heap.
The arrow alone does not make a resource unique, since an unrestricted value may be passed to a linear function too.
Uniqueness comes from how an API hands resources out: with hidden constructors, and only through a linear continuation, beside an existing linear resource, or in exchange for a consumed linearity token.
A unique owner of GC-heap or static data "releases" it by handing it back to the GC, which is what most `consume` implementations do.
Only when the resource lives off-heap (Haskell holds just a pointer) is the owner obliged to actually free it.

Consequences:

- Bind mutable resources **linearly** whenever they are mutated purely or shared with parallel computations: a unique owner cannot observe a destructive update, and disjoint owners cannot race.
- Bind immutable data **nonlinearly**; there is nothing to protect.
- An exception abandons whatever the unique owner held.
  For off-heap resources use `System.IO.Resource.Linear` (`RIO`, which releases registered resources when an exception escapes; not the `rio` package) or `Foreign.Marshal.Pure` (pools freed on exceptions).

## The linear-base vocabulary

- `Ur a` is an `a` that may be used nonlinearly: it lives on the GC heap and may have many owners.
  Matching `Ur x` yields an unrestricted `x`.
- `Consumable a` means the resource can be released linearly and purely (like Rust's `Drop`): `consume :: a %1 -> ()`, `lseq :: Consumable a => a %1 -> b %1 -> b`.
- `Dupable a` means the resource can be duplicated any finite number of times during the computation: `dup :: a %1 -> (a, a)`, `dup3`, ….
  Duplicates must be independent, so any mutable state is copied and purity and determinism survive.
- `Movable a` means the value can be moved onto the GC heap, where it may have many owners: `move :: a %1 -> Ur a`.

A type that is mutated purely or concurrently **must not** be `Movable`: an unrestricted copy could be observed while the original is mutated.
Ban the instance explicitly so the mistake produces a readable error:

```haskell
{-# LANGUAGE DataKinds, UndecidableInstances #-}
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)

instance
  (Unsatisfiable ('Text "Histogram owns a mutable Array; it cannot be moved into Ur")) =>
  Movable Histogram
  where
  move = unsatisfiable
```

Use GHC's `GHC.TypeError.Unsatisfiable` (GHC ≥ 9.8): it also discharges the `Dupable` superclass, so the ban works even for a type that is not `Dupable`.
`Prelude.Linear.Unsatisfiable` from linear-base works as well, but only when a genuine `Dupable` instance exists.

Other everyday helpers: `unur :: Ur a %1 -> a`; `forget :: (a %1 -> b) %1 -> a -> b` (or eta-expand `\x -> f x`) to pass a linear function where an unrestricted one is expected; `Data.Unrestricted.Linear.AsMovable` to derive `Consumable` and `Dupable` from a `Movable` instance.
`Unsafe.Linear.toLinear` goes the other way and is unsafe: use it only with a proof that the function really is linear.

## Module setup

```haskell
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Control.Functor.Linear qualified as Control
import Data.Array.Mutable.Linear (Array)
import Data.Array.Mutable.Linear qualified as Array
import Data.Functor.Linear qualified as Data
import Prelude.Linear
import Prelude qualified as P
```

The examples assume `default-language: GHC2021`; under Haskell2010 also enable `ImportQualifiedPost`.
`Prelude.Linear` replaces many Prelude functions with linear versions, which often return the consumed input alongside the result (`length :: [a] %1 -> (Ur Int, [a])`).
For values you already hold unrestricted (for example after matching `Ur xs`), use the ordinary functions through the qualified `P.` import.
Keep modules written in the linear style separate from ordinary modules where practical.

## Threading linear resources

Linear APIs take a resource and hand it back, e.g. `Array.read :: Array a %1 -> Int -> (Ur a, Array a)`.
Bind the returned resource with one of these forms:

```haskell
-- case: each alternative binds fresh, non-recursive names, so shadowing is safe.
doubleAtCase :: Int -> Array Int %1 -> Array Int
doubleAtCase i arr = case Array.read arr i of
  (Ur v, arr) -> Array.write arr i (v * 2)

-- linear let (GHC >= 9.10): a non-variable pattern must be strict (!), and names must be fresh.
doubleAtLet :: Int -> Array Int %1 -> Array Int
doubleAtLet i arr0 =
  let %1 !(Ur v, arr1) = Array.read arr0 i
   in Array.write arr1 i (v * 2)

-- a linear monad with QualifiedDo: '<-' never recurses, so shadowing is safe.
bump :: Int -> Control.State (Array Int) ()
bump i = Control.do
  Ur v <- Control.state (\arr -> Array.read arr i)
  Control.modify (\arr -> Array.write arr i (v + 1))

-- allocation (scope-passing): only a Movable result may leave the scope.
doubled :: [Int]
doubled = unur (Array.fromList [1, 2, 3] (Array.toList . doubleAtCase 0))
```

The older `Array.read arr i & \(Ur v, arr) -> …` style (linear `&` plus a lambda) still works everywhere.

**Shadowing.**
Reusing the name of a consumed linear variable (with `-Wno-name-shadowing`) is customary, because it makes the dead variable unreachable.
But `let` and `where` are recursive: in `let %1 !(Ur v, arr) = Array.read arr i`, the `arr` on the right refers to the new binding.
For a linear variable GHC rejects this with a misleading `Couldn't match type 'Many' with 'One' arising from multiplicity of 'arr'`; for an unrestricted variable it compiles and loops at run time.
When you shadow, use `case` or a qualified `do` block; with `let`, always use fresh names (`arr0`, `arr1`).
For pure code, prefer a data-flow `do` (an identity bind, where `(Ur v, arr) <- Array.read arr i` is plain reverse application): pure-borrow ships it as `Control.Syntax.DataFlow`, and [references/syntax.md](references/syntax.md) has the 20-line module to copy otherwise.

**Patterns and conditionals.**

- Lazy pattern bindings (`let (a, b) = …`, `~(a, b)`) are always unrestricted (under `Strict`, only `~` patterns are lazy); use `let !(a, b) = …` or `case`.
- `@`-patterns and view patterns are not linear.
- A wildcard `_` on a linear field is an error, and GHC may report it at the binder of the *scrutinee* rather than at the `_`.
  Consume the field instead (`i \`lseq\` …`).
- `if c then … else …` works when `c` is computed from linear values, but that consumes them: `dup` first if a branch still needs the value.
- Guards and `MultiWayIf` must not mention linear variables, because they use their conditions unrestrictedly; guards over unrestricted variables are fine.
  Branch on a linear value with `if` or with `case` on a `Bool`.

## Diagnosing multiplicity errors

Up to at least GHC 9.14, linear-type errors are enigmatic.
A violation is typically reported only as `Couldn't match type 'Many' with 'One' arising from multiplicity of 'v'`, located at the binder of `v` and quoting the whole equation instead of the use that went wrong, and GHC may stop reporting the other type errors in that definition.
Do not guess; bisect:

1. Replace the suspicious code with `undefined` applied to the linear variables it has not consumed yet, and check that the definition now compiles.
2. Revive the original code piece by piece (one branch, one binding, or the tail of a `do` block) until the error comes back.
3. The piece revived last contains the violation; fix it there.

```haskell
-- sumsToZero :: Vector Int %1 -> Ur Bool   consumes the vector
isTrivial :: Int -> Vector Int %1 -> Ur Bool
isTrivial x v = if x == 0 then Ur True else sumsToZero v
-- error: Couldn't match type 'Many' with 'One' arising from multiplicity of 'v'

isTrivial x v = if x == 0 then undefined x v else undefined x v   -- compiles
isTrivial x v = if x == 0 then Ur True else undefined x v         -- fails again: the then-branch drops v
isTrivial x v = if x == 0 then undefined x v else sumsToZero v    -- compiles: the else-branch is fine

isTrivial x v = if x == 0 then v `lseq` Ur True else sumsToZero v -- the fix
```

`x` is unrestricted, so passing it to `undefined` as well is harmless.
Never feed a linear variable that the surrounding code has already consumed (for example in a condition): the stub itself would then use it twice.
Common causes are listed in [references/troubleshooting.md](references/troubleshooting.md).

## Data functors and control functors

Linear Haskell splits the functor hierarchy in two:

| | `Data.Functor.Linear` (data functors) | `Control.Functor.Linear` (control functors) |
| --- | --- | --- |
| `fmap` | `(a %1 -> b) -> f a %1 -> f b` | `(a %1 -> b) %1 -> f a %1 -> f b` |
| Intuition | Containers holding zero or more elements, so the function may run many times | Effects through which exactly one value flows, so the function runs once |
| Examples | lists, `Maybe`, `V n`, `Ur` | linear `State`, `Reader`, `RIO`, linear `IO`, pure-borrow's `BO` |
| Monad | none | `Monad`, with `(>>=) :: m a %1 -> (a %1 -> m b) %1 -> m b` |

Every control functor is also a data functor, and `Data.traverse` (the `Traversable` of `Data.Functor.Linear`) takes a *control* applicative.
Use `QualifiedDo` with `Control.do`; statements without `<-` must have type `m ()`, because `(>>) :: m () %1 -> m a %1 -> m a` cannot discard anything else.
`QualifiedDo` only rewrites the `do` desugaring, so `Control.pure` and `Control.return` must still be qualified.
See [references/functors-and-effects.md](references/functors-and-effects.md) for `UrT` (interop with ordinary monads) and `RIO`.

## Designing data types

- Bind a field **linearly** when it carries mutable state or anything else that needs a unique owner.
- Bind a field **nonlinearly** when its value is immutable or already `Movable`.
- Ordinary `data` declarations (including record syntax) make every field linear, even an `Int`: this is what lets existing types such as tuples and `Maybe` hold linear values.
- To mix nonlinear and linear fields, use GADT syntax, even for a plain ADT:

  ```haskell
  data Histogram where
    Histogram :: !Text -> !(Array Int) %1 -> Histogram
  ```

- GHC ≥ 9.14 also accepts record syntax, `data R = R { label %'Many :: Text, buf :: Array Int }` (with `DataKinds` and `import GHC.Exts (Multiplicity (..))`); use GADT syntax while GHC 9.10/9.12 must be supported.
- `newtype` fields are always linear.
- Give mutable types `Consumable` (and `Dupable` only if duplication means an independent deep copy), and ban `Movable`.
- Derive instances with `deriveGeneric ''T` from linear-generics (`Generics.Linear.TH`, needs `TemplateHaskell`, `TypeFamilies`, `DataKinds`) and `deriving via Generically T instance Consumable T`.
  linear-generics' Template Haskell cannot derive `Generic` for a type with any nonlinear field, which includes every type mixing nonlinear and linear fields: hand-roll those instances, dropping or sharing nonlinear fields freely.
  Details and worked examples are in [references/data-and-classes.md](references/data-and-classes.md).

## Allocating linear resources

- **Scope-passing** (linear-base): `Array.fromList :: Movable b => [a] -> (Array a %1 -> b) %1 -> b` (linear-base < 0.5 used `Ur b`).
  The resource cannot escape because only a `Movable` result leaves the scope, but allocating several resources nests continuations and pins them to the outermost scope.
- **Token-based**: allocators take a `Linearly` token, a witness that the context is linear which only `linearly :: Movable a => (Linearly %1 -> a) %1 -> a` can create from nothing.
  linear-base does not ship it; pure-borrow does, and [references/functors-and-effects.md](references/functors-and-effects.md) explains the pattern.

## GHC version notes

| Topic | GHC 9.10.3 | GHC 9.12.4+ |
| --- | --- | --- |
| Linear `let`/`where`, `let %1 …` | yes | yes |
| Constructor patterns (`Ur x <- …`, `(Ur x, v) <- …`, `MkT a b <- …`) in a qualified `do` whose module does not export `fail` | **rejected** with `Not in scope: 'M.fail'` (compiler bug; variables and tuples of variables are fine) | accepted |
| Evaluating linear code in GHCi / the HLS eval plugin | may segfault (GHC bug; compiled code is fine) | fine from 9.12.3 |
| Record-field multiplicities (`x %'Many :: a`) | no | 9.14+ only |

On GHC 9.10, avoid the `fail` bug by requiring GHC ≥ 9.12 if the user allows dropping 9.10, or else by binding a variable and matching it with `case`: `(len, v) <- size v` followed by `case len of Ur n -> …`.
`Control.do` from linear-base is not affected, because `Control.Functor.Linear` exports `fail`.

## Checklist before handing code back

- Every linear variable is used exactly once on every path, including error branches: apply the error to it (`error "msg" x`) or `lseq` it away.
- No shadowed name appears on the right-hand side of its own `let`/`where`.
- Mutable resources are never `Movable`; their bans are explicit.
- Unrestricted results leave linear scopes only through `Ur`/`Movable`.
- The code compiles on every GHC the project supports; see [references/syntax.md](references/syntax.md) for the version matrix.
