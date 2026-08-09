# Revision history for pure-borrow

## Unreleased

### Breaking changes

- `Alias` carries its lifetime in the alias kind instead of as its own parameter, so a kind-polymorphic function can abstract over an alias including its lifetime:

    ```haskell
    -- before
    newtype Alias ak α a = UnsafeAlias a
    data AliasKind = Borrow BorrowKind | Lend
    -- after
    newtype Alias ak a = UnsafeAlias a
    data AliasKind = Borrow BorrowKind Lifetime | Lend Lifetime
    ```

  `Mut α`, `Share α` and `Lend α` are unchanged for users.
  The bundle is now `Aliases k xs`, with `Borrows bk α`, `Muts α`, `Shares α` and `Lends α` as synonyms; `Experimental.Loop` no longer re-exports its constructors, and `Experimental.Borrows` absorbs the reborrow instances from `Experimental.Reborrowable`.
- `Reborrowable` takes the lifetime through associated types, so an instance is written for the applied type:

    ```haskell
    -- before
    class Reborrowable bor where
      locally' :: bor α a %1 -> (forall β. bor (β /\ α) a %1 -> BO (β /\ α') (After β r)) %1 -> BO α' (r, bor α a)
    -- after
    class (bor ~ WithLifetime bor (LifetimeOf bor)) => Reborrowable bor where
      type LifetimeOf bor :: Lifetime
      type WithLifetime bor (β :: Lifetime) :: k -> Type
      locally' :: bor a %1 -> (forall β. WithLifetime bor (β /\ LifetimeOf bor) a %1 -> BO (β /\ α') (After β r)) %1 -> BO α' (r, bor a)
    ```

- `Data.Vector.Mutable.Linear.Borrow`'s `toVector` and `toList` require `Movable` rather than `Copyable`, and are now \(O(n)\): each element goes through `move`, which is what hands a linearly owned buffer to a GC-owned result.
- `Affine (Aliases k xs)` is constrained to borrow kinds, so a `Lends` bundle can no longer be discarded in safe code, matching the scalar `Lend`.
- `Control.Concurrent.STM.TMDeque` and `TMDequeRingBuffer` are removed; the scheduler runs on `Control.Concurrent.Queue.ChaseLev`.

### Clarified

- `Copyable.copy` must complete the copy and return it in WHNF.
  This was always the contract — a `copy` returning a thunk that still reaches into the borrowed structure was never sound — but nothing stated or enforced it.
  The class Haddock now says so, and the generic machinery forces each component copy, so derived instances discharge it for free.

### New

- Five borrow-aware mutable vector families beside the existing boxed `Data.Vector.Mutable.Linear.Borrow`.
  Element-owning: fixed unboxed, and growable boxed and unboxed.
  Backend-generic and *not* element-owning, with GC-owned entries and \(O(1)\) consuming freeze: `Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted` and its growable counterpart.
  `…Borrow.Experimental.Multiplicity` parameterizes the same storage by element multiplicity.
- The growable families keep their length and backing buffer behind a stable header, and project the initialized prefix through `getContents` or the rank-2 `withContent`/`withContent_` as a fixed borrow of the same kind and lifetime.
  Growth replaces the buffer, so it is unavailable while a content borrow is live.
- Added a Robin Hood hash table with backward-shift deletion.
  `Data.HashMap.RobinHood.Mutable.Linear` is the owned table, whose operations are ordinary linear functions; `Data.HashMap.RobinHood.Mutable.Linear.Borrow` keeps one behind a linear `Ref` so that it can be mutated, and grown, through a `Mut` borrow.
  Its keys and values are GC-owned, and it caches a fingerprint per slot so that a key with a cheap hash and an expensive equality is rejected without a full comparison.
  This adds a dependency on `hashable`.
- `subShare` shortens a `Share` without opening a scope, and `Par` is an applicative for parallel composition inside `BO`, with directly inlinable methods.
- `BO.Unsafe` exports `unsafeCastAlias`, a `coerceLin` retagging that replaces a bare `unsafeCoerce`.

### Performance

- Delimiting a sublifetime got much cheaper in `srunBO`/`srunBO_`, `sharing`/`sharing'`/`sharing_` and `reborrowing`/`reborrowing'`/`reborrowing_`.
  The `slow` flag restores the old, safe-but-slow implementation.
- `BO` and `After` methods are directly inlinable, and the divide-and-conquer scheduler uses a Chase-Lev deque with half-stealing and weighted victim selection.
  The qsort and FFT examples are written over the unrestricted vectors and specialize to primitive unboxed array operations.
- A `pure-borrow-inspection` suite pins the optimized Core of the scopes and of the qsort/FFT roots, inverting each obligation under `slow`.

## 0.0.0.0 -- 2026-05-05

This is the first release on Hackage :tada:
Please refer to our paper for details.
Besides the parts covered by the paper, we are providing the following experimental features:

- Bulk borrows by `Borrows` heterogeneous list.
- `Reborrowable` type class for abstraction over reborrowable borrow-like objects.
- Looping structure.
- Record splitting.
