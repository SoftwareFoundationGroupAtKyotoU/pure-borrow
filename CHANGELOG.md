# Revision history for pure-borrow

## 0.2.0.0 - unreleased

Each breaking change closes a soundness hole: a program that typechecks against 0.1.0.0 using only the safe modules and breaks an invariant of the library.

### Breaking changes

- In the three growable vector modules, `size`, `capacity` and `getContents` are `BO` actions, since as pure functions of a borrow they could return the size or contents from another point of the computation, even from after the borrow's lifetime had ended:

    ```haskell
    -- before
    getContents :: Borrow bk α (GrowableVector a) %1 -> Borrow bk α (Fixed.Vector a)
    -- after
    getContents :: (α >= β) => Borrow bk α (GrowableVector a) %1 -> BO β (Borrow bk α (Fixed.Vector a))
    ```

  Bind the result with `<-`, and `move` a shared projection before passing it to a reader that takes it unrestricted, such as `Fixed.copyAt`: `Ur content <- move Control.<$> getContents shared`.
  A helper that calls one of them may need `α >= β` in its signature.
- `LinearOnlyWitness` now has a `representational` role instead of the `phantom` one it had before, which unsoundly allowed virtually any type to derive `LinearOnly`.
- `nowStatic :: BO α (Now Static)` replaces the top-level `nowStatic :: Now Static`, and has moved to `Control.Monad.Borrow.Pure.BO`.
  The previous, pure `nowStatic` could unsoundly let a `Linearly` token spill into a non-linear context.
- Fix the bug where the subtyping relation `(<:)` wrongly made `Many` a subtype of `One`, which allowed any nonlinear function to be unsoundly upcast into a linear one.
- `Control.Monad.Borrow.Pure.Experimental.Loop.foldBorrowOf` is removed: it ran the `Fold` purely on the value behind a borrow, so reads of a mutable container were not ordered by `BO` and could see writes made after the borrow's lifetime.
  Replace `foldBorrowOf fld` with `foldBorrowVia sp` for a splitter `sp` such as `Control.Monad.Borrow.Pure.split`.
  `foldBorrow`, which was `foldBorrowOf foldMap` with the `foldMap` of the same module, is now `foldBorrowVia split`, so it requires `DistributesAlias t`.
  It rejects a borrow of an `Either e a` or `(e, a)` with "Use splitEither directly!" or "Use splitPair instead!".
- `assocBOEq`, `assocBorrowEq` and `assocLendEq` are removed, since they let a program prove any two lifetimes equal and reclaim a resource while it was still borrowed.
  Use `assocLBO`, `assocRBO`, `assocBorrowL`, `assocBorrowR`, `assocLendL` or `assocLendR` instead.
- `modifyBoxedVector` requires `Movable a`, since its callback could store a linearly owned value, such as a `Ref`, into the GC-owned result.
- `modifyBoxedMVector` is renamed `unsafeModifyBoxedMVector`, with the same `Movable` requirement.
  The caller keeps the storage, so after catching an exception from it the storage must not be read or reused.
- `Clone` for `Ref`, the boxed `Vector` and the multiplicity-polymorphic vector requires `Clone` of the contents instead of `Dupable`, since cloning through `Dupable` could consume the original or hand it to the clone while a `Share` could still read it.
  A `Copyable` type gets the instance with `deriving via AsCopyable T instance Clone T`, a record or sum of clonable fields with `deriving anyclass instance Clone T` and a linear-generics `Generic` instance, a newtype with `deriving newtype Clone`, and an immutable type such as `Text` can be stored as `Ur Text`.
  Contents that are `Clone` but not `Dupable`, as in `Ref (Vector (Ref Int))`, can now be cloned, and linear-base's `Array`, `Vector`, `HashMap` and `Set` still can.
- The multiplicity-polymorphic vector's `fromList` requires `KnownMultiplicity p`, which both multiplicities have, so that it evaluates its elements only where it owns them.
  A function polymorphic in `p` that calls it is rejected with "No instance for `KnownMultiplicity p`"; add the constraint to its signature.
- `Data.HashMap.RobinHood.Mutable.Linear` exports only `HashMap`, `Hashable`, `fromList`, `lookup`, `member`, `size`, `capacity`, `foldMapWithKey` and `toList`, since its `insert`, `insertMany`, `delete`, `alter`, `alterF`, `unsafeInsertPrepared` and `union` updated a table they were given in place, so a call left unevaluated where both branches of a `parBO` reach it could run twice and leave the table inconsistent.
  Build a table that changes with `Data.HashMap.RobinHood.Mutable.Linear.Borrow` instead, whose `HashMap` is a different type and whose mutations and queries are `BO` actions on a borrow of it.
  Each removed name has a namesake there, `InsertPlan`, `lookupForInsert` and `unsafeInsertPrepared` included, except `new`, which is its `empty`, and `insertMany`, for which fold its `insert` over the list.
- The instances listed under "New" overlap with orphan instances that a user may have written for 0.1.0.0, such as `Clone (Ur Text)`, `Clone (Array a)` for linear-base's `Array`, `Clone` for a boxed growable or unboxed vector, or a `(<:)` instance for `Maybe`; delete the orphan.
  Call sites of an old shallow vector-cloning orphan now need `Clone a`, since those vectors own their elements.
  An orphan as general as the new instance, such as `Clone (Array a)`, is rejected with "Duplicate instance declarations"; a narrower one, such as `Clone (Array Int)`, compiles, and each use of it is rejected with "Overlapping instances".

### Changed

- `parBO` rethrows a branch's exception, unchanged, once the other branch has stopped, instead of blocking forever or dying with `BlockedIndefinitelyOnMVar`; `Par`, `mapConcurrentlyOf`, `naiveDivideAndConquer` and `Data.Vector.Mutable.Linear.Borrow.qsort` inherit this.
  Stopping reaches only the other branch: whatever it had started with a nested `parBO` runs to completion, so `mapConcurrentlyOf` over a list stops none of the other elements when the first one throws, and all of them when the last one does.
  A branch in a loop that does not allocate cannot be stopped until the loop ends; compile the module that contains such a loop with `-fno-omit-yields` if the rethrow must be prompt.
  An asynchronous exception to the caller, such as a `timeout`, does not stop the branches: forcing the interrupted value again collects their results, and a value that is dropped instead leaves them running to completion.
  A computation's thread stays in memory after it finishes: the second computation's until the first one finishes, and the first computation's until the thread running `parBO` runs again; `Par` and `mapConcurrentlyOf` over a list put the short computation first.
- `Data.Ref.Linear.Borrow.update`, `modify`, `swap` and `readShare`, and the borrow-aware hash map's queries, `take`, `take_` and `swap`, read and write inside `BO`, at their place in the sequence; a write could previously land only when its result was forced, after the lifetime had ended.
- `reclaim` forces the `EndToken` it is discharged with; `withEnd` leaves it alone.
- `copy` of a mutable container is rejected with a message that points to `clone` inside `BO`, for linear-base's `Array`, `Vector`, `HashMap` and `Set` as well as pure-borrow's references and vectors.
- `Data.Ref.Linear.new` and `unsafeWriteRef`, and the `fromList` of the boxed, unboxed, unboxed growable and multiplicity-polymorphic vectors, evaluate the value they store to weak head normal form; the multiplicity-polymorphic vector does so only at `One`, where it owns its elements.
  So do the writes through a `Mut`, and storing a `Share` evaluates the value it points to.
  The thread that evaluates an owner, usually the parent before a `parBO` forks, therefore computes its contents, and a `fromList` its elements one after another, where branches that read different contents used to compute them in parallel; the borrow-aware hash map's `fromList` and `union` likewise build their tables as the map is evaluated.
  A placeholder such as `undefined` raises when its owner is borrowed, even if nothing reads it, and in a worker when the `divide` of `divideAndConquer` or `divideAndConquer'` returns it inside an owner (see "Clarified").
  To keep an expensive value for the branch that needs it, build it there, or store a GC-owned one in a lazy box such as `Ur`, which is evaluated only to the box.
- At a key that both tables hold, the borrow-aware hash map's `extend` keeps the value of the table it inserts, as its `insert` does, and its `union` keeps the second table's value.
  Both used to keep the value of whichever table had fewer entries.
- The `toList` of both Robin Hood hash maps, and the `foldMapWithKey` of `Data.HashMap.RobinHood.Mutable.Linear`, visit the entries in a different order, which stays unspecified.

### Fixed

- The hash maps of `Data.HashMap.RobinHood.Mutable.Linear` and `Data.HashMap.RobinHood.Mutable.Linear.Borrow` no longer lose an entry, or store one twice, when many keys collide.
  They did when a table grew and its keys had hashes alike in their low bits, such as `Int` keys that are multiples of 1024, and when about 128 keys had the same hash.
  A table holds at most 128 keys with the same hash: an insertion past that raises an error, where it used to drop entries silently, and so can one into a table where many hashes are each shared by dozens of keys (see "Colliding keys" in the documentation of either module).
- A pure read of an owner handed back by `runBO`, `runBOLend`, `modifyBO`, `modifyBO_`, `reclaim` or a scope that discharges an `After` sees the writes made during the scope; at `-O2` it could return what the owner held before the scope.
- `reclaim` fails on a bottom `EndToken` given to `withEnd`, instead of handing back an owner whose borrows are still live.
- Every allocation made with a `Linearly` token returns a fresh resource, even when the token is forced or the `runBO` that makes it has no free variables; at `-O2` two such allocations could return one reference.
- `Data.Ref.Linear.new`, `atomicModify`, `atomicModify_` and `unsafeWriteRef`, and runs of `runBO` and `modifyBO`, perform their effects once, even when several threads force the same call, as both branches of a `parBO` can through a `Share`; an increment could be applied twice, and two branches could see different references.
- `Data.Ref.Linear.atomicModify_` crashed on a `Ref Int`, and on a `Ref` of a list stored the new head instead of the new list.
  `atomicModify` stored the first component of its function's result, meant to be returned, instead of the second, the new contents.
- Cloning one shared `Ref` of a linear-base `Array` more than once, as a loop does, gave every clone the same array at `-O2`, so a write to one clone reached them all.
  Each clone now copies the array.
- The work-stealing scheduler behind `divideAndConquer`, `divideAndConquer'`, `qsortDC` and `fftDC` runs every task exactly once; it could run a task twice and lose another, so that `qsortDC` occasionally hung or returned a vector it had not sorted.
- `stealHalf` of `Control.Concurrent.Queue.ChaseLev` may return fewer than half the tasks, and the deque no longer misplaces its elements when it grows or, on ARM64, hands a thief what a slot held before the owner's write.
- Deriving `Control.Monad.Borrow.Pure.Experimental.Loop.Foldable` via `Generics.Linear.Generically1` failed for every type with a field mentioning its parameter, for lack of instances for `Par1` and `:.:`.
  It still fails on a field that does not mention the parameter, such as `Int`.
- A call that updates memory in place, such as linear-base's `Array.map f arr`, runs once when both branches of a `parBO` read or clone through a `Share` a value that `Data.Ref.Linear.new`, the `fromList` of the boxed, unboxed or multiplicity-polymorphic vectors, or the multiplicity-polymorphic vector's `write` at `One` stores, or a value that such a call computes, as an `Int` summed from an `Array.map` is.
  It could run twice: `Ref.new (Array.map (+ 1) arr)` added 2 to some elements, a clone could copy the array halfway through, and an `Array.map` that changes the element type crashed the program.
  The same held for a call of one of those constructors left unevaluated where both branches reach it, such as a component of a pair, and for a list whose cells make the call as they are produced.
  The borrow-aware hash map's `union` no longer stores its insertions unevaluated, which let two branches that looked keys up in the union lose entries or answer a lookup wrongly.

### Clarified

- `divideAndConquer` and `divideAndConquer'` require `divide` and `conquer` to raise no exception: they do not propagate one raised during the parallel computation, and block instead.
- The callback of `unsafeInplace` must only rearrange elements, never duplicate, drop or replace one; the boxed vector's `unsafeFromMutable` requires every element to be initialised.
- The owners evaluate what they store, but not a lazy field inside it, such as a component of a pair or the payload of a `Just`, and not a call forced outside a `BO` action, as by a bang on an argument.
  Such a call of one of linear-base's in-place operations, such as `set`, `write` and `map` of `Data.Array.Mutable.Linear`, must be evaluated before it is stored where several branches of a `parBO` reach it, with a strict field, `StrictData` or the linear `$!` of `Prelude.Linear`, as in `Just PL.$! Array.map f arr`, and so must each such call that a record, a list or a vector holds.
  Two branches that force it at once can both run it: `Array.map (+ 1)` can add 2, a clone can copy an array halfway through an update, and a linear-base container can be left inconsistent.

### New

- `Consumable` for the boxed `Vector` of `Data.Vector.Mutable.Linear.Borrow`, which gives a vector of non-`Movable` elements, such as `Ref`s, a way out.
- `Clone` for `Ur`, `Data.Monoid.Sum`, `Data.Monoid.Product`, `Min`, `Max`, `Arg` and `Complex`.
- `Clone` for linear-base's `Array`, `Vector`, `HashMap` and `Set`, which copies the container and shares its GC-owned elements, requiring nothing of them; a `Vector` or `HashMap` keeps its capacity.
- Deep `Clone` for the boxed growable vector and the fixed and growable unboxed vectors, requiring `Clone a` (and `Unbox a` for the unboxed families).
  Growable copies retain length and capacity and clone only initialized elements.
- `Clone` and `Copyable` for `Identity`, `Down`, `Const`, `Dual`, semigroup `First` and `Last`, `WrappedMonoid`, `Any`, `All`, `Alt`, and tuples through arity six.
  Newtype wrappers delegate to the contained value's instance; an unboxed representation does not imply a shallow copy is safe.
- `Clone` for the hash map of `Data.HashMap.RobinHood.Mutable.Linear`, which copies the table and shares its GC-owned keys and values.
- `DistributesAlias` for `NonEmpty`.
- `foldBorrowVia` and `traverseBorrowOf_` in `Control.Monad.Borrow.Pure.Experimental.Loop`, to fold over the borrows a splitter makes and to run an action on each element a `Fold` visits.
- `upcast` works componentwise on `Maybe` and `NonEmpty`, as it does on lists.
- `Control.Monad.Borrow.Pure.BO.Unsafe` exports `restoreWithEnd`, `reviveAliasWithEnd#`, `endHere` and `withEndL`, for a delimiter of your own that discharges an `After`.

### Performance

- `modifyBoxedVector` is slower by one pass over the vector.
- A `push` that grows an unboxed growable vector allocates less; the other operations of the growable vectors are unchanged.
- `parBO` is slower and allocates more, for its exception handling: a fork-join whose branches do little is noticeably slower, and the divide-and-conquer FFT and quicksort built on `parBO` slightly so.
  Its finished threads also stay in memory longer.
- `reclaim`, the runs of the `runBO` family, `modifyBO` and `modifyBO_` included, and the scopes that discharge an `After` (`sharing'`, `reborrowing'`, `reborrowings'`, `srunBO`) are slightly slower, and each run allocates slightly more; `sharing`, `reborrowing` and the `_` variants are unchanged.
- Owner-level operations on a `Ref` are slower when the program runs on several capabilities, and unchanged on one.
- `clone` of a boxed vector of references allocates more, since it copies each reference when the clone is taken; `clone` of a vector of values such as `Int` allocates as before.
- `clone` of a linear-base `Array`, and so of a `Ref` or boxed `Vector` of arrays, is slightly slower, more so on GHC 9.10.
  Code that clones one borrow repeatedly, as a loop does, is slower, since every clone now gets an array of its own (see "Fixed").
- Stealing work from the deque of `Control.Concurrent.Queue.ChaseLev` is slower, more so on x86-64, though the work-stealing quicksort and FFT show no measurable slowdown.
- Allocations that take a `Linearly` token, such as `Ref.new` and the constructors of the vectors, are slightly slower.
- Both Robin Hood hash maps are noticeably slower, and allocate slightly more, on keys whose hashes are consecutive numbers, such as `Int` keys counted up from 1.
  Such keys used to land in consecutive slots without ever colliding, and a table no longer places keys by the low bits of their hashes alone (see "Fixed"); keys whose hashes are already scattered are about as fast as before.
- `foldBorrow` over a list or `NonEmpty` allocates more.
- Storing a value in a `Ref` or a vector is slightly slower, since it evaluates the value, and `fromList` of the vectors is slower; at `One`, the multiplicity-polymorphic vector's `fromList` allocates more per element.
- A loop of `modifyBO_` calls that hand the owner back is slower and allocates more, and, unless the owner is forced, it builds up a chain of delayed updates.

## 0.1.0.0 - 2026-09-19

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
- `DivideConquer` is now generalized to run on generic vector and qsort benchmarks are now run against unboxed vectors.

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
- The divide-and-conquer scheduler now correctly exposes `Result` to allow users to write their own algorithm.
  Now it further supports non-trivial "conquer" phase and now comes with FFT example.

### Performance

- Delimiting a sublifetime got much cheaper in `srunBO`/`srunBO_`, `sharing`/`sharing'`/`sharing_`, `reborrowing`/`reborrowing'`/`reborrowing_` and `reborrowings`/`reborrowings'`/`reborrowings_`.
  The `slow` flag restores the old, safe-but-slow implementation.
  Measured on a scope-dominated microbenchmark, the plural scopes went from 40 bytes and 4.5 ns per crossing to 0 bytes and 1.2–1.6 ns.
  The FFT example picks this up through `iterReborrowing_`, which crosses a plural scope per iteration of its setup phase: −4.8% allocation and −5.6% wall at 2^20 points.

## 0.0.0.0 -- 2026-05-05

This is the first release on Hackage :tada:
Please refer to our paper for details.
Besides the parts covered by the paper, we are providing the following experimental features:

- Bulk borrows by `Borrows` heterogeneous list.
- `Reborrowable` type class for abstraction over reborrowable borrow-like objects.
- Looping structure.
- Record splitting.
