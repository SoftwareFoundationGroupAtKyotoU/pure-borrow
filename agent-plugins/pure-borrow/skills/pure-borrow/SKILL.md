---
name: pure-borrow
description: >-
  Write, review, and debug Haskell code using the pure-borrow library, which brings Rust-style borrowing to Linear Haskell: the BO monad, lifetimes, Mut/Share/Lend borrows, Linearly tokens, reborrowing and sharing scopes, pure parallelism with parBO, Copyable/Clone, and borrow-aware vectors, references and hash maps.
  Use it when a package depends on pure-borrow, when code imports Control.Monad.Borrow.Pure, Control.Syntax.DataFlow, Control.Concurrent.DivideConquer.Linear, Data.Ref.Linear or a Data.*.Linear.Borrow module, or when it mentions BO, runBO, borrowM, reclaim, pureAfter, reborrowing, sharing, Mut α, Share α or Lend α.
  Always load the linear-haskell skill too.
license: BSD-3-Clause
compatibility: >-
  pure-borrow >= 0.1 on GHC 9.10.3 or 9.12.4+ (recommended) with linear-base >= 0.7.
  Requires the linear-haskell skill.
metadata:
  author: Hiromi Ishii
  requires-skill: linear-haskell
---

# Pure Borrow

pure-borrow realises Rust-style borrowing in Linear Haskell, purely: compile-time ownership, the *aliased-xor-mutable* discipline, and deterministic parallelism with no runtime bookkeeping.
It is the artifact of the PLDI 2026 paper *Pure Borrow: Linear Haskell Meets Rust-Style Borrowing* by Matsushita and Ishii.

**Load the `linear-haskell` skill first** (in Claude Code it is named `linear-haskell:linear-haskell`).
Everything here assumes its rules on linear arrows, `Ur`, `Consumable`/`Dupable`/`Movable`, `let %1`, shadowing, and `QualifiedDo`.
If that skill is not installed, tell the user to install it (`linear-haskell@pure-borrow` from this repository's plugin marketplace, or `npx skills add SoftwareFoundationGroupAtKyotoU/pure-borrow --skill linear-haskell`) instead of continuing without it.

References:

- [references/api.md](references/api.md) — signatures of the core API, grouped by task.
- [references/lifetimes.md](references/lifetimes.md) — the lifetime algebra, sublifetimes, subtyping, and reading lifetime type errors.
- [references/data-types.md](references/data-types.md) — `Copyable`, `Clone`, `Movable`, generic deriving, splitting borrows of your own types.
- [references/containers-and-parallelism.md](references/containers-and-parallelism.md) — vectors, growable vectors, `Ref`, hash maps, `parBO`, `Par`, divide-and-conquer.
- [references/troubleshooting.md](references/troubleshooting.md) — common errors and fixes.
- [references/extending.md](references/extending.md) — soundness obligations when implementing a new borrowable data structure with the `.Unsafe` modules.
- [references/further-reading.md](references/further-reading.md) — the paper, the tutorial Haddock, and background on `Linearly`.

## The model

Three views of a resource of type `a`, all indexed by a lifetime `α`:

- `Lend α a` is the unique **lender**; after `α` ends, `reclaim` turns it back into the owned `a` as a direct value.
  It is linear and must be neither dropped nor duplicated.
- `Mut α a` is a **mutable borrow** with exclusive read/write access during `α`.
  It is always bound linearly, every mutating operation hands it back, and it is `Consumable` (affine): dropping it just ends your access.
- `Share α a` is a **shared** (aliased) borrow with read-only access during `α`.
  It is introduced inside `Ur` and is `Dupable`, `Movable`, and `Consumable`, so it can be used any number of times.

The lifetimes in the types enforce aliased-xor-mutable by construction: a `Mut` can only be shared by giving it up (`share`) or by suspending it for a sublifetime (`sharing`), and a `Lend` can only be reclaimed once `α` has ended, after which no borrow of `α` may be used.
Nothing is checked at run time: all three are the same zero-cost representation, and lenders and borrowers may live in unrelated parts of the program ("non-local" interaction), which is what makes splitting and parallelism possible.

Two more ingredients:

- **`Linearly`** is a token witnessing that the current context is linear.
  Only `linearly :: Movable a => (Linearly %1 -> a) %1 -> a` can create one from nothing; it can be `dup`ed and `consume`d, and is never `Movable`.
  Every allocator takes one (`VL.fromList :: [a] %1 -> Linearly %1 -> VL.Vector a`), which is why a freshly allocated resource is always linearly owned.
  Inside `BO`, get one with `askLinearly`/`asksLinearly`, or recover one from a value that can only exist linearly (`LinearOnly`) with `withLinearly`.
  It stands in for the linear constraint `Linearly %1 =>` proposed for GHC.
- **`BO α`** is the monad of computations performed *during* lifetime `α`: morally a linear `ST` whose state token is indexed by a lifetime.
  It is a control monad, so use `Control.do`.

## Module setup

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Prelude.Linear
```

The examples assume `default-language: GHC2021`; under Haskell2010 also enable `ImportQualifiedPost`.
`Control.Monad.Borrow.Pure` is the prelude of the library (its Haddock is the tutorial); `Control.Monad.Borrow.Pure.BO` adds the lower-level combinators.
Import container modules qualified.
Use `Control.do` for `BO`, and `DataFlow.do` (from `Control.Syntax.DataFlow`) for pure code that threads linear values, such as `Linearly` tokens: its `<-` is plain reverse application and never recursive, so rebinding the same name is safe where a `let` would loop.
Name lifetime variables `α`, `β`, `γ` as the library does.

## The canonical skeleton

```haskell
basic :: (Int, [Int])
basic = linearly \lin -> runBO lin Control.do
  (mvec, lend) <- borrowLinearlyM (VL.fromList [0, 1, 2])  -- allocate, then Mut + Lend
  mvec <- VL.modify 0 (+ 3) mvec
  mvec <- VL.modify 2 (+ 5) mvec
  let !(Ur svec) = share mvec                                -- give up the Mut for an unrestricted Share
  Ur n <- VL.copyAt 0 svec
  pureAfter (n, unur (VL.toList (reclaim lend)))             -- runs after the lifetime ends
-- basic == (3, [3, 1, 7])
```

- `runBO :: Linearly %1 -> (forall α. BO α (After α a)) %1 -> a` runs a fresh lifetime and then the `After α` post-processing; `runBO_` returns the direct result, and `runBOLend` reclaims a returned `Lend`.
- `After α a` is a finaliser evaluated once `α` has ended; `pureAfter` builds one, and inside it `reclaim :: End α => Lend α a %1 -> a` is available.
- Inside `pureAfter`, only reclaim lenders and work with owned values: never use a `Mut` or `Share` of the ending lifetime there, even though a captured `Share` still typechecks, because it can observe stale data after the owner changes.
  Copy anything you need out of a borrow into `Ur` before `pureAfter`.
- `linearly` needs a `Movable` result, so finish by converting owned resources to unrestricted values (`VL.toList` needs `Movable` elements) or freeing them.
- `modifyBO`, `modifyBO_`, `modifyLinearOnlyBO`, and `modifyLinearOnlyBO_` wrap the whole skeleton for "mutate this owned value in place".

To allocate several resources outside `BO`, duplicate the token in a `DataFlow.do` block and `borrowM` each owner:

```haskell
twoVectors :: ([Int], [Int])
twoVectors = linearly \lin -> DataFlow.do
  (lin, lin1) <- dup lin
  (lin, lin2) <- dup lin
  xs <- VL.fromList [1, 2] lin1
  ys <- VL.fromList [10, 20] lin2
  runBO lin Control.do
    (mxs, lendXs) <- borrowM xs
    (mys, lendYs) <- borrowM ys
    (Ur x, mxs) <- VL.copyAtMut 0 mxs
    mys <- VL.modify 0 (+ x) mys
    Control.pure (consume mxs)
    Control.pure (consume mys)
    pureAfter (unur (VL.toList (reclaim lendXs)), unur (VL.toList (reclaim lendYs)))
-- twoVectors == ([1, 2], [11, 20])
```

## Writing reusable functions

Take the borrow at one lifetime and run in any sublifetime of it, as the container operations do, so the helper works both in the ambient lifetime and inside scopes:

```haskell
addEach :: (α >= β) => Int -> Mut α (VL.Vector Int) %1 -> BO β (Mut α (VL.Vector Int))
addEach k v = case VL.size v of
  (Ur n, v) -> go 0 n v
  where
    go :: (α >= β) => Int -> Int -> Mut α (VL.Vector Int) %1 -> BO β (Mut α (VL.Vector Int))
    go i n v
      | i >= n = Control.pure v   -- guards over unrestricted Ints are fine
      | otherwise = Control.do
          v <- VL.modify i (+ k) v
          go (i + 1) n v

addAll :: Int -> VL.Vector Int %1 -> VL.Vector Int
addAll k v = modifyLinearOnlyBO_ v \mv -> consume Control.<$> addEach k mv
```

## Choosing a borrow operation

| Goal | Use |
| --- | --- |
| Borrow an owned linear value inside `BO` | `borrowM :: a %1 -> BO α (Mut α a, Lend α a)` |
| Allocate and borrow at once | `borrowLinearlyM :: (Linearly %1 -> a) %1 -> BO α (Mut α a, Lend α a)` |
| Borrow outside `BO`, at a lifetime chosen by the caller | `borrow :: a %1 -> Linearly %1 -> (Mut α a, Lend α a)` (from `Control.Monad.Borrow.Pure.BO`) |
| Stop mutating and read freely for the rest of `α` | `share :: Borrow bk α a %1 -> Ur (Share α a)` |
| Read temporarily, then mutate again | `sharing`, `sharing_`, `sharing'`, or infix `(<$~)`, `(<$=)` |
| Mutate through a shorter-lived borrow (e.g. to split it), then get the original back | `reborrowing`, `reborrowing_`, `reborrowing'`, or infix `(<%~)`, `(<%=)` |
| Get the owned value back after the lifetime | `reclaim` inside `pureAfter`/`After`, or `reclaim'` |
| Copy a plain value out of a borrow | `copy`, `copyMut`, container-specific `copyAt`/`copyAtMut` |
| Duplicate a mutable value | `clone :: Clone a => Share α a %1 -> BO α a` |
| Split a borrow of a structure into borrows of its parts | `splitPair`, `splitEither`, `split` (`DistributesAlias`), `VL.splitAt` |
| Run two computations in parallel | `parBO :: BO α a %1 -> BO α b %1 -> BO α (a, b)`, `Par`, `mapConcurrentlyOf` |

The scope combinators take a rank-2 continuation over a fresh sublifetime `β`:

```haskell
reborrowing :: Mut α a %1 -> (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 -> BO α' (r, Mut α a)
sharing     :: Mut α a %1 -> (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 -> BO α' (r, Mut α a)
```

Read `forall β. … (β /\ α) …` as "for every sublifetime of `α`": the continuation must work whichever sublifetime the combinator picks, so nothing typed with `β` can escape, which is exactly what makes handing back the original `Mut` sound.
The `_` variants discard a `Consumable` result, and the `'` variants let the continuation return an `After β r` finaliser.
Inside `reborrowing_`, end with `consume Control.<$> op mvec` rather than returning the reborrowed `Mut`; the combinator hands the outer one back.
These scopes are erased at compile time: no lifetime token or lender is allocated.
See [references/lifetimes.md](references/lifetimes.md) for `srunBO`, `upcast`, `subShare`, and the outlives constraints (`α >= β`).

## Parallelism

```haskell
parallel :: (Int, [Int])
parallel = linearly \lin -> runBO lin Control.do
  (mvec, lend) <- borrowLinearlyM (VL.fromList [0, 1, 2])
  mvec <- reborrowing_ mvec \mvec -> Control.do
    let !(mvec1, mvec2) = VL.splitAt 1 mvec     -- disjoint mutable borrows
    consume
      Control.<$> parBO
        (Control.do mvec1 <- VL.modify 0 (+ 3) mvec1; VL.modify 0 (* 4) mvec1)
        (VL.modify 1 (+ 5) mvec2)
  (Ur n, mvec) <- VL.copyAtMut 0 mvec
  Control.pure (consume mvec)
  pureAfter (n, unur (VL.toList (reclaim lend)))
-- parallel == (12, [12, 1, 7])
```

`parBO` forks real threads (compile with `-threaded`, run with `+RTS -N`), yet the result is deterministic: the two branches can only hold disjoint *mutable* borrows (read-only `Share`s may overlap).
It does not forward exceptions: if a branch throws, the parent blocks and dies with `thread blocked indefinitely in an MVar operation`, so validate inputs before forking.
`reborrowing_` returns the whole `Mut` once both halves have been dropped, so there is no need to reunite them by hand.
For many tasks use the `Par α` applicative (`runPar`, `mapConcurrentlyOf`, `forConcurrentlyOf`); for scalable divide-and-conquer use `Control.Concurrent.DivideConquer.Linear`.

## Containers

- The boxed `Data.Vector.Mutable.Linear.Borrow` vector owns its elements linearly and `get` returns a borrow of the element, but it has no `Consumable` instance: it can only be finished with `toVector`/`toList`, which need `Movable` elements.
  For elements that are not `Movable` (such as `Ref`s or other vectors) use the growable vector `Data.Vector.Mutable.Growable.Linear.Borrow`, which is `Consumable`; a boxed vector of `Ref`s can never be disposed of.
- For plain numbers prefer `Data.Vector.Unboxed.Mutable.Linear.Borrow` (almost the same API, and `Consumable`) or `Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted`, whose elements are GC-owned and read back as `Ur a`.
- `Data.Ref.Linear` with `Data.Ref.Linear.Borrow` gives a mutable cell, and `Data.HashMap.RobinHood.Mutable.Linear.Borrow` a hash map with GC-owned keys and values.

Details are in [references/containers-and-parallelism.md](references/containers-and-parallelism.md).

## Copyable, Clone, and Movable

| Class | Signature | For |
| --- | --- | --- |
| `Copyable a` | `copy :: Borrow bk α a %1 -> a` | Values without mutable state; the copy is pure, complete once in WHNF, and may outlive the borrow |
| `Clone a` | `clone :: Share α a %1 -> BO α a` | Any value, including mutable ones; the independent duplicate is created inside `BO`, so it stays linear |
| `Movable a` | `move :: a %1 -> Ur a` | Moving an **owned** value to the GC heap, e.g. when a container's elements leave linear ownership (`VL.toVector`, `VL.toList`) |

A type containing mutable state **must not** be `Copyable` nor `Movable`, and **must** ban both explicitly with `Unsatisfiable`: a `Share` is `Movable`, so a `copy` taken through a moved `Share` could leak an alias of mutable state into unrestricted code.
Derive the instances with `deriving via Generically T` after `deriveGeneric ''T` (linear-generics), and `Clone` from `Copyable` with `deriving via AsCopyable T`; details, and what to do for types mixing nonlinear and linear fields, are in [references/data-types.md](references/data-types.md).

Choose by ownership, not by representation: a value bound nonlinearly (`->`, `Ur`) is GC-owned and needs no `Copyable`/`Clone`/`Dupable`/`Consumable` to be copied, kept, or dropped.

## Rules of thumb

- Keep every `Lend` until a `pureAfter`/`After` block reclaims it.
- `share` consumes the `Mut` for the rest of the lifetime; use `sharing` when you want to mutate again afterwards.
- Borrows cannot outlive their lifetime: nothing typed with a scope's private lifetime may appear in the result of a `reborrowing`/`sharing` continuation or of `runBO`.
- Prefer the checked operations (`get`, `set`, `modify`, `swap`) over `unsafeGet`, `unsafeSet`, and friends, which skip bounds checks.
  `unsafeFromVector`, `unsafeFromMutable`, and `unsafeInplace` are worse: they alias existing storage or bypass element ownership, so never use them in application code.
- For read-only loops, `share` once outside the loop and use `subShare` inside it instead of opening a `sharing` scope per iteration; keep loop workers `INLINE`/`INLINABLE`.
- Never import `.Internal` or `.Unsafe` modules in application code.
  The only exception this skill documents is a hand-rolled `Clone` for a type mixing nonlinear and linear fields ([references/data-types.md](references/data-types.md)), which must carry a written soundness argument; see [references/extending.md](references/extending.md) before writing any such code.
