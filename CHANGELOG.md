# Revision history for pure-borrow

## 0.2.0.0 - unreleased

Each breaking change closes a soundness hole: a program that typechecks against 0.1.0.0 using only the safe modules and breaks an invariant of the library.

### Breaking changes

- In the three growable vector modules, `size`, `capacity` and `getContents` are `BO` actions.
  As pure functions of a borrow they could run after the borrow's lifetime had ended and the vector had grown, and GHC could serve one read for another, so they returned stale sizes and contents:

    ```haskell
    -- before
    getContents :: Borrow bk α (GrowableVector a) %1 -> Borrow bk α (Fixed.Vector a)
    -- after
    getContents :: (α >= β) => Borrow bk α (GrowableVector a) %1 -> BO β (Borrow bk α (Fixed.Vector a))
    ```

  Write `content <- getContents borrow`.
  A shared projection is bound linearly, while readers such as `Fixed.copyAt` take a shared borrow unrestricted, so move it before reading, even once: `Ur content <- move Control.<$> getContents shared`.
  Every other header read of those modules now happens inside `BO` as well, at its place in the sequence.
  If GHC then asks for a constraint such as `β <=!! α` in a helper's signature, add `α >= β`.
- `End`, `(<=)` and `(<:)` are exported from the safe modules as synonyms of classes they do not export, so no instance can be written or derived against them.
  Every such instance was unsound, and some drew no warning at all: `deriving via (α :: Lifetime) instance End α` let `reclaim` run at any time, `instance Static <= α` lengthened any borrow, and `deriving via (Mut Static (Ref T)) instance Mut α (Ref T) <: Mut Static (Ref T)` did the same through `upcast (m, ())`.
  Constraints written with them are unchanged.
  `End` and `(<=)` can still be partially applied; `(<:)` takes both arguments, so where you wrote `(<:) a` alone, define a class with it as the superclass, as the Haddock of `(<:)` shows.
  The classes themselves are `Ended`, `SubLifetime` and `Subtype`, exported from `Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe`, `Control.Monad.Borrow.Pure.Lifetime.Internal` and `Data.Coerce.Directed.Unsafe`.
  A subtyping instance for a type of your own now names the class, with `Data.Coerce.Directed.Unsafe` imported, and takes on its obligation:

    ```haskell
    -- before
    deriving via Generically (T b) instance (a <: b) => T a <: T b
    -- after
    deriving via Generically (T b) instance (a <: b) => Subtype (T a) (T b)
    ```

  To convert a value of a type with a linear-generics `Generic` instance without declaring anything, use `genericUpcast`.
  GHC names `(<=)` in its messages by the class, `SubLifetime`, or by the helper `<=!!`.
  "Could not deduce `γ <=!! α`" asks for an outlives constraint that no hypothesis gives directly, since hypotheses are not chained: add it, as `α >= γ`, to your signature.
  "Overlapping instances for `SubLifetime`" with a lifetime that GHC made up, such as `α0`, means a lifetime GHC cannot determine: with `step :: (α >= β) => Share α Int -> Share β Int`, the intermediate one of `step (step s)`, say, or one that appears only in constraints.
  Name that lifetime with a type application, as in `step @α @γ (step @α @α s)`, or drop the constraints on a lifetime that does not occur in the type.
  Evidence made up with `GHC.Exts.withDict`, which GHC classifies as unsafe, is still accepted, and a program that uses it is outside the library's guarantee, like one that uses `unsafeCoerce`.
- A `LinearOnly` instance must inherit an existing one, through `deriving newtype` or `deriving via` a representationally equal type.
  An instance without its method, including `deriving anyclass`, compiled with a `-Wmissing-methods` warning and let `withLinearly` mint an unrestricted `Linearly`; it is now a compile-time error that says how to write the instance, and writing the method takes `Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe`.
  `withLinearly` forces the witness, so an instance derived via its own type no longer mints a token either.
  Deriving from a type of a different representation, which gave a free `Linearly`, is rejected by the role of the witness.
  For a record of resources, which could only get an empty instance before, call `withLinearly` on one of its `LinearOnly` fields and rebuild the record; it needs no instance.
  An instance written through `.Unsafe` for such a record must ensure that every constructor holds a `LinearOnly` resource in a strict field: otherwise `withLinearly (Env undefined 0)` mints a token from a record that holds no resource, and so does `withLinearly (EnvOpt Nothing 0)` for a strict field of type `Maybe (Ref Int)`.
- `nowStatic :: BO α (Now Static)` replaces the top-level `nowStatic :: Now Static`, with which `withLinearly` minted an unrestricted `Linearly`.
  `Control.Monad.Borrow.Pure.Lifetime.Token` no longer exports it; `Control.Monad.Borrow.Pure.BO`, which exported the old one as well, exports the new one.
  To run a `BO Static` action inside `BO β`, `upcast` it; `execBO` runs one with the token directly, and `consume (aff now)` drops the token it hands back.
- `upcast` between function types compares multiplicities: a linear function can be used where an unrestricted one is expected, never the other way round.
  0.1.0.0 compared `One` with `Many` whatever its arguments were, and so accepted `(a -> b)` to `(a %1 -> b)`, which duplicates any linear resource.
  `(a %m -> b)` to `(a -> b)`, `(a %1 -> b)` to `(a %m -> b)` and `(a %m -> b)` to itself still work for a multiplicity variable `m`; `(a -> b)` to `(a %m -> b)`, `(a %m -> b)` to `(a %1 -> b)` and between two different variables are rejected with "Cannot satisfy: multiplicity … <= …", so require `((a %p -> b) <: (a %q -> b))` in the signature to abstract over them.
  The orphan `Compare` instance on `Multiplicity` is removed.
- `Control.Monad.Borrow.Pure.Experimental.Loop.foldBorrowOf` is removed, and `foldBorrow` requires `DistributesAlias t`: folding a mutable container through a borrow read its elements in pure code.
  For a container with that instance, write `Loop.foldMap k (split borrow)`, with the `foldMap` of `Control.Monad.Borrow.Pure.Experimental.Loop`; for any other fold, split the borrow and fold its parts.
  Lists, `Maybe` and `NonEmpty` have the instance, and a container of your own gets it with `deriving anyclass instance DistributesAlias T`.
  A fold over an `Either e` or `(e, a)` borrow is rejected with "Use splitEither directly!" or "Use splitPair instead!": split the borrow and fold the component.
- `modifyBoxedVector` requires `Movable a` and passes every element through `move` on the way out, one extra \(O(n)\) pass: its callback could store a linearly owned value, such as a `Ref`, into the GC-owned result.
- `modifyBoxedMVector` is renamed `unsafeModifyBoxedMVector`, with the same `Movable` requirement.
  The caller keeps the storage, so after catching an exception from it the storage must not be read or reused.
  The old name remains only as a compile-time error that names the replacement and that obligation.
- `Clone` for `Ref`, the boxed `Vector` and the multiplicity vector requires `Clone` of the contents instead of `Dupable`, and clones each piece of the contents through a shared borrow of it, without consuming the original or writing to it.
  `dup2` consumes its argument, and linear-base's laws do not say which of its two copies, if either, is the original.
  With a `Dupable` that returns two fresh copies, 0.1.0.0 consumed the original twice, and with one that returns the original second, a clone that kept the first copy would hand the original to the clone while a live `Share` could still read it.
  Give the contents a `Clone` instance.
  A `Copyable` type gets one with `deriving via AsCopyable T instance Clone T`, with `AsCopyable` from `Control.Monad.Borrow.Pure.Clone`.
  A record or sum type of clonable fields gets one with `deriving anyclass instance Clone T`, once it has the `Generic` instance of linear-generics, from `$(deriveGeneric ''T)` of `Generics.Linear.TH`, which takes `DataKinds`, `TemplateHaskell` and `TypeFamilies`.
  An immutable, GC-owned type with neither, such as `Text` or `ByteString`, can be stored as `Ur Text`, since `Clone (Ur a)` shares its payload.
  A newtype over a type that has an instance gets one with `deriving newtype Clone`.
  A type that holds a resource with no instance needs one written by hand, as the header of `Control.Monad.Borrow.Pure.Clone` describes.
  For another package's type, write it for a newtype over that type rather than as an orphan, which an instance added later by this library or by that package would break.
  Contents that are `Clone` but not `Dupable`, as in `Ref (Vector (Ref Int))`, can now be cloned.
  Contents that are `Dupable` but none of the above can no longer be cloned through a borrow.
  Among linear-base's types, `Data.Array.Mutable.Linear.Array` keeps being cloned, now through an instance of its own, while `Data.Vector.Mutable.Linear.Vector`, `Data.HashMap.Mutable.Linear.HashMap` and `Data.Set.Mutable.Linear.Set` have none, so a `Ref` of one of them, which 0.1.0.0 cloned, is rejected with "No instance for" their `Clone`: use pure-borrow's own boxed `Vector` and hash map instead.
- `Linearly`, `Now` and `EndToken` carry a field, so that GHC cannot learn which value a token is, and `Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe` exports their constructors as `UnsafeLinearlyToken`, `UnsafeNowToken` and `UnsafeEndToken`.
  The old names `UnsafeLinearly`, `UnsafeNow` and `UnsafeEnd` remain as patterns, which build a token and match an unrestricted one as the constructors did.
  GHC does not let a pattern synonym match a linearly bound value, and rejects such a match with "Couldn't match type ‘Many’ with ‘One’" arising from "a non-linear pattern" "(pattern synonyms aren't linear)", or, in a `case`, arising from the "multiplicity of" the variable matched.
  Where you matched a linear token, write the constructor instead, as in `\(UnsafeLinearlyToken _) -> ()`.
  A token built with a pattern or a constructor is a constant, which GHC may share between allocations: build one only inside a function that is `NOINLINE` and applied through `noinline`.
  Where you take a token apart and return another, pass the field on; a function that returns two tokens must itself be `NOINLINE` and applied through `noinline`, as `dup2` is, since two tokens with the same field are one expression.
- The instances listed under "New" overlap with orphan instances that a user may have written for 0.1.0.0, such as `Clone (Ur Text)`, `Clone (Array a)` or a `Subtype` instance for `Maybe`; delete the orphan.
  An orphan as general as the new instance, such as `Clone (Array a)`, is rejected with "Duplicate instance declarations"; a narrower one, such as `Clone (Array Int)`, compiles, and each use of it is rejected with "Overlapping instances".

### Changed

- `parBO` rethrows a branch's exception, unchanged, once the other branch has stopped, instead of blocking forever or dying with `BlockedIndefinitelyOnMVar`; `Par`, `mapConcurrentlyOf`, `naiveDivideAndConquer` and the parallel `qsort` inherit this.
  Stopping reaches only the other branch: whatever it had started with a nested `parBO` runs to completion, so `mapConcurrentlyOf` over a list stops none of the other elements when the first one throws, and all of them when the last one does.
  A branch in a loop that does not allocate cannot be stopped until the loop ends; compile the module that contains such a loop with `-fno-omit-yields` if the rethrow must be prompt.
  An asynchronous exception to the caller, such as a `timeout`, does not stop the branches: forcing the interrupted value again collects their results, and a value that is dropped instead leaves them running to completion.
  A computation's thread, stack included, about 1 KB, stays in memory after it finishes: the second computation's until the first one finishes, and the first computation's until the thread running `parBO` runs again; `Par` and `mapConcurrentlyOf` over a list put the short computation first.
- `Data.Ref.Linear.Borrow.update`, `modify`, `swap` and `readShare`, and the hash map's queries and `take`, `take_` and `swap`, read and write inside `BO`, at their place in the sequence; a write could previously land only when its result was forced, after the lifetime had ended.
- `reclaim` forces the `EndToken` it is discharged with; `withEnd` leaves it alone.

### Fixed

- An owner handed back after a scope, by `runBO`, `runBOLend`, `modifyBO`, `modifyBO_`, the scopes that discharge an `After` or `reclaim` itself, could be read by a pure operation before the scope's writes, once GHC merged that read with an earlier one: after `(r1, r2) <- dup2 r0`, a scope that bumped `r1` and then `Ref.free (reclaim lend)` returned the contents from before the bump at `-O2`, and for a `Ref (Ref a)` it handed out a second owner of a reference it had given away.
  The owner now comes back through a barrier that depends on the end of the lifetime, also in a module compiled with `-fno-state-hack` or one that forces the `EndToken` it discharges an `After` with, and `dup2` on a `Ref` no longer hands back the reference it read.
- `withEnd` given a bottom `EndToken`, which anyone can write, let `reclaim` hand an owner back while its borrows were still live, so that two `Mut`s reached one resource; it now fails instead.
- Forcing a `Linearly` token, with a bang, `$!`, a strict field or a module compiled with `Strict`, let GHC merge the allocations made with it: after `case dup2 lin of (!l1, !l2)`, `Ref.new seed l1` and `Ref.new seed l2` were one reference, and a function that allocated a reference or a vector from a forced token returned the same one on every call.
  Forcing a `Now` made the end token of every lifetime one shared constant.
  Without anything forced, a `runBO` whose action has no free variables, such as `runBO_ lin (asksLinearly (Ref.new 0))`, was computed once for the whole program, and every call returned the same reference.
- `Data.Ref.Linear.new`, `atomicModify`, `atomicModify_` and `unsafeWriteRef`, and computations run by `runBO` and `modifyBO`, performed their effects once per thread when several threads forced the same unevaluated call, for example one stored in a `Ref` and read through a `Share` by both branches of a `parBO`: an increment could be applied twice, and two branches could see different references.
- `Data.Ref.Linear.atomicModify_` could crash or store an ill-typed value, and `atomicModify` stored the old value rather than the new one.
- Cloning one shared `Ref` of a linear-base `Array` more than once, as a loop does, gave every clone the same array at `-O2`: GHC made the pure `dup2` of the contents once for all the clones, so a write to one clone reached them all.
  Each clone now copies the array.

### New

- `Consumable` for the boxed `Vector` of `Data.Vector.Mutable.Linear.Borrow`, which gives a vector of non-`Movable` elements, such as `Ref`s, a way out.
- `Clone` for `Ur`, `Sum`, `Product`, `Min`, `Max`, `Arg` and `Complex`.
- `Clone` for linear-base's `Data.Array.Mutable.Linear.Array`, which copies the array into a new one and shares its GC-owned elements, so it requires nothing of them.
- `Clone` for the owned hash map of `Data.HashMap.RobinHood.Mutable.Linear`, which copies its slot array; the borrow-aware hash map clones through it.
- `DistributesAlias` for `NonEmpty`.
- `upcast` works componentwise on `Maybe` and `NonEmpty`, as it does on lists.
- `Control.Monad.Borrow.Pure.BO.Unsafe` exports `restoreWithEnd`, `reviveAliasWithEnd#`, `endHere` and `withEndL`, for a delimiter of your own that discharges an `After`: its end token must come from the state thread rather than from the `UnsafeEnd` constructor.

### Clarified

- The callback of `unsafeInplace` must only rearrange elements, never duplicate, drop or replace one; `unsafeFromMutable` requires every element to be initialised.
- `copy` of linear-base's `Array` or `Vector` is rejected with a message that says why, where it used to name linear-base's `Unsatisfiable` class, and for the array the message points to `clone`.

### Performance

- The growable vectors read and write their header inline, in the state thread, where 0.1.0.0 made an out-of-line call: a `push` that grows an unboxed vector, and the plural scope benchmark that threads a bundle, allocate 28% less, and the other growable and scope benchmarks are unchanged.
- `parBO` allocates about 13.5% more per call, 2627 bytes against 2314, for the exception handling.
  In time, it costs about 20 ns more per call at `-N1`: measured with interleaved runs against 0.1.0.0 on GHC 9.12.4, the fork-join benchmark, whose branches do almost nothing, runs 25–35% slower at `-N1` and 9–28% slower at `-N4`, and the divide-and-conquer FFT on 2^20 points runs 9% slower.
  On the quicksort of 32,768 elements at `-N10`, where the unchanged introsort varies by ±2% between rounds, the divide-and-conquer version built on `parBO` runs 2% slower, in every round; the budgeted parallel and the sequential versions are unchanged within that noise, and the work-stealing version, which does not use `parBO`, is not slower (2–8% faster).
  Its finished threads also stay in memory longer: the parallel divide-and-conquer FFT benchmark on 2^20 points peaks at 137 MB at `-N1`, against about 105 MB for 0.1.0.0, and at 123 MB against 118 MB at `-N4`.
- Every `reclaim`, every run of the `runBO` family, and every crossing of a scope that discharges an `After` (`sharing'`, `reborrowing'`, `reborrowings'`, `srunBO`) makes one or two more out-of-line calls; `sharing`, `reborrowing` and the `_` variants are unchanged.
- Every run of the `runBO` family, `modifyBO` and `modifyBO_` included, allocates its lifetime tokens, the `Now` and the end token with its `Ur`, 48 bytes, where 0.1.0.0 used static tokens shared by all runs: a loop of `modifyBO_` allocates 80 bytes per iteration against 32, and the benchmarks that run `BO` once per iteration 48 bytes more.
- The `noDuplicate#` guard costs about 9 ns per owner-level `Ref` operation with several capabilities, and nothing with one.
- `clone` of a boxed vector of references allocates about 25% more than 0.1.0.0, 4.16 MB against 3.31 MB per clone of 100,000 `Ref Int`s, because each element's clone is a new `Ref`, allocated when the clone is taken rather than left as a thunk.
  `clone` of a vector of values such as `Int` allocates exactly what it did in 0.1.0.0.
- `clone` of a linear-base `Array`, and so of a `Ref` or boxed `Vector` of arrays, copies each array in one pass with the array's own `dup2`, as 0.1.0.0 did through `Dupable`.
  It costs a bare `cloneMutableArray#` plus about 2.5 ns and 16 bytes per clone on GHC 9.12 and later; on 9.10, whose `evaluate` allocates a thunk around the copy, it costs 48 bytes and about 5 ns more, or 30 ns more at 1,000 elements.
  Code that clones one borrow repeatedly, as a loop does, now pays for a copy per clone, where 0.1.0.0 made one copy and gave it to every clone (see "Fixed").

### Known issues

- `divideAndConquer`, `divideAndConquer'`, `qsortDC` and `fftDC` do not propagate an exception raised by `divide` or `conquer`; the caller blocks instead.
- `qsortDC`, on the work-stealing scheduler, occasionally never returns: in a benchmark sweep at `-N10`, 1 of 45 work-stealing benchmarks ran past a 10 s timeout, where a sort takes about a millisecond, and 0.1.0.0 did the same in 2 of 45.
  The cause is not known yet.
- A pure value whose evaluation writes memory it did not allocate can perform those writes twice if two threads force it at the same moment, for example both branches of a `parBO` reading it through a `Share`.
  `Ref`'s pure operations and `runBO`/`modifyBO` computations are protected, but the owned hash map's `insert`, `delete` and `alter` in `Data.HashMap.RobinHood.Mutable.Linear` are not.
  Neither are linear-base's in-place operations: `set`, `write`, `unsafeSet`, `unsafeWrite`, `map` and `fmap` of `Data.Array.Mutable.Linear`, and linear-base's `Vector`, `HashMap` and `Set`, which are built on them.
  Any field that stores such a call unevaluated, as `Ref.new` and `Data.Vector.Mutable.Linear.Borrow.fromList` do, or a lazy field of a record, a `Maybe` or a list, can have the call run twice when two branches read or clone it through a `Share` at once.
  A lone write run twice writes the same value twice, but `map`, `fmap` and chains of reads and writes read what the first run wrote, and a clone taken meanwhile can copy what the second run has written so far: `Array.map (+ 1)` adds 2 to some elements, a clone can copy an array halfway through an update, or hold a value that a chain wrote to one place only on the way, and a `map` that changes the element type reads the first run's results at the wrong type and crashes the program.
  Force such a call before storing it where several branches can reach it, e.g. `Ref.new $! HashMap.insert k v m` or `Ref.new $! Array.map f arr`; `$!` reaches only the outermost constructor, so force each call that a record, a list or a vector holds.
  The protection holds under GHC's default lazy blackholing; `-feager-blackholing` on the module that builds the value can defeat it, and single-capability programs are unaffected.

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
