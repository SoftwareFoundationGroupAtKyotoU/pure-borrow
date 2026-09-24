# Containers and parallelism

Import every container module qualified; many function names clash with `Prelude.Linear`.
Owned containers are allocated with a `Linearly` token and are `LinearOnly`; all access goes through `Mut`/`Share` borrows inside `BO`.

## Element ownership

Containers differ in whether they own their elements linearly:

| Kind | Modules | Elements | `get` returns | Materialising |
| --- | --- | --- | --- | --- |
| Element-owning | `Data.Vector.Mutable.Linear.Borrow` (boxed), `Data.Vector.Mutable.Growable.Linear.Borrow` (boxed, growable), and their `Unboxed` counterparts | bound linearly; may themselves be mutable, including boxed representations of unboxed elements | a borrow of the element, `Borrow bk α a` | `toVector`/`toList` require `Movable a` and `move` every element |
| Non-element-owning vectors | `Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted` (any `vector` backend) and its growable variant | unrestricted, GC-owned | the value itself, `(Ur a, borrow)` | `toVector` freezes in O(1), no `Movable` needed |
| Hash map | `Data.HashMap.RobinHood.Mutable.Linear.Borrow` | keys and values unrestricted, GC-owned | `lookup` returns `Ur (Maybe v)` | `toList` copies the entries in O(n) through a borrow |

Element ownership is a design choice independent of boxed vs. unboxed storage: pick the non-element-owning vectors for plain data you only read and write, and the element-owning ones for nested mutable structures.
Because an element-owning `set` cannot drop the old element, it returns it: `set :: Int -> a %1 -> Mut α (Vector a) %1 -> BO β (a, Mut α (Vector a))`.
Fixed and growable element-owning vectors have `Consumable` instances requiring `Consumable a`, so either can own and eventually consume `Ref`s.
Their `Clone` instances require `Clone a` and clone each initialized element deeply; the unboxed families also require `Unbox a`, and growable clones preserve capacity.
An `Unbox` representation alone does not make a buffer copy sound: boxed representations can hold linear references.
The boxed growable vector's `fromList` takes an unrestricted list, so start from `empty` and `push` linear elements one by one; the fixed boxed and unboxed `fromList`s take their elements linearly.

## Boxed borrow vectors (`Data.Vector.Mutable.Linear.Borrow`, as `VL`)

```haskell
VL.fromList   :: [a] %1 -> Linearly %1 -> VL.Vector a
VL.constant   :: Int -> a -> Linearly %1 -> VL.Vector a
VL.fromVector :: V.Vector a -> Linearly %1 -> VL.Vector a
VL.toVector   :: Movable a => VL.Vector a %1 -> Ur (V.Vector a)
VL.toList     :: Movable a => VL.Vector a %1 -> Ur [a]
VL.size       :: Borrow bk α (VL.Vector a) %1 -> (Ur Int, Borrow bk α (VL.Vector a))
VL.get        :: (HasCallStack, α >= β) => Int -> Borrow bk α (VL.Vector a) %1 -> BO β (Borrow bk α a)
VL.set        :: (HasCallStack, α >= β) => Int -> a %1 -> Mut α (VL.Vector a) %1 -> BO β (a, Mut α (VL.Vector a))
VL.modify     :: (α >= β) => Int -> (a %1 -> a) %1 -> Mut α (VL.Vector a) %1 -> BO β (Mut α (VL.Vector a))
VL.update     :: (α >= β) => Int -> (a %1 -> BO β (b, a)) %1 -> Mut α (VL.Vector a) %1 -> BO β (b, Mut α (VL.Vector a))
VL.swap       :: (HasCallStack, α >= β) => Mut α (VL.Vector a) %1 -> Int -> Int -> BO β (Mut α (VL.Vector a))
VL.copyAt     :: (Copyable a, α >= β) => Int -> Share α (VL.Vector a) -> BO β (Ur a)
VL.copyAtMut  :: (Copyable a, α >= β) => Int -> Mut α (VL.Vector a) %1 -> BO β (Ur a, Mut α (VL.Vector a))
VL.splitAt    :: Int %1 -> Borrow bk α (VL.Vector a) %1 -> (Borrow bk α (VL.Vector a), Borrow bk α (VL.Vector a))
VL.indicesMut :: (HasCallStack, α >= β) => Mut α (VL.Vector a) %1 -> [Int] %1 -> BO β [Mut α a]   -- distinct indices
```

- Consume the vector to consume each of its owned elements, or materialize it with `toVector`/`toList`, which call `move` on each element and need `Movable a`.
- `VL.indicesMut` consumes the vector borrow and returns borrows of the requested elements; call it inside a `reborrowing` scope when you need the whole vector again afterwards.
- `unsafeGet`, `unsafeSet`, `unsafeSwap`, `unsafeIndicesMut`, … only skip bounds (and, for `indicesMut`, distinctness) checks.
  `unsafeFromVector` (thaws a GC-owned vector in place), `unsafeFromMutable` (aliases the caller's `MVector`), and `unsafeInplace` (runs an `ST` action that may duplicate or drop owned elements) break ownership unless you prove otherwise; keep them out of application code.
- The unboxed module has nearly the same API with a `U.Unbox a` constraint; it lacks `indicesMut` and `unsafeInplace`, and adds `copyToVector`.
- The growable modules add `push`, `extend`, `reserve`, `capacity`, and `withCapacity`, but have no `splitAt`.
  To split a growable vector, for example for parallelism, open `withContent` (which lends its logical contents as a fixed-size vector for a sublifetime) and split that.
  Growable `size`, `capacity`, and `getContents` run in `BO`, since their header can change: bind their results with `Control.do`.
  `size` and `capacity` return `(Ur count, borrow)`, so thread or consume the returned borrow.
  `getContents` consumes the growable borrow and returns a fixed view; when called on a `Share`, its result is still linearly bound by the action, so use `Ur content <- move Control.<$> getContents shared` before passing it to an unrestricted reader.

## Non-element-owning vectors and hash maps

- `Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted` works over any `vector` backend (`Vector v a` with `G.Vector v a`); `get` returns `(Ur a, borrow)`, `write`/`set` overwrite freely, `toVector` freezes in O(1).
- `Data.HashMap.RobinHood.Mutable.Linear.Borrow` (keys and values GC-owned): `empty :: Int -> Linearly %1 -> HashMap k v`, `fromList`, `insert`/`delete :: … -> Mut α (HashMap k v) %1 -> BO α (Ur (Maybe v), Mut α (HashMap k v))`, `alter`, `lookup`/`member`/`size`/`toList` on any borrow.
  Its operations use one lifetime for both the borrow and the `BO`.

## References (`Data.Ref.Linear`, `Data.Ref.Linear.Borrow`)

```haskell
Ref.new      :: a %1 -> Linearly %1 -> Ref a
Ref.free     :: Ref a %1 -> a
RefB.update  :: (α >= β) => (a %1 -> BO β (b, a)) %1 -> Mut α (Ref a) %1 -> BO β (b, Mut α (Ref a))
RefB.modify  :: (α >= β) => (a %1 -> a) %1 -> Mut α (Ref a) %1 -> BO β (Mut α (Ref a))
RefB.swap    :: (α >= β) => Mut α (Ref a) %1 -> Mut α (Ref a) %1 -> BO β (Mut α (Ref a), Mut α (Ref a))
RefB.readShare :: (α >= β) => Share α (Ref a) %1 -> BO β (Ur (Share α a))
RefB.copyRef :: (Copyable a, α >= β) => Borrow k α (Ref a) %1 -> BO β a
```

`Ref a` owns its content linearly and is `Consumable` when `a` is.
`Ref.new` evaluates its contents to WHNF when the reference itself is evaluated, which borrowing it does; the element-owning `fromList`s likewise evaluate each element before storing it.
An `undefined` placeholder therefore raises even if the borrowed owner is never read, and expensive elements can be computed sequentially in the parent before a fork.
Build expensive work inside its branch, or wrap a GC-owned value in a lazy `Ur` box when it should remain unevaluated.
WHNF does not force nested lazy fields: evaluate an in-place linear-base operation before placing it in a field that multiple branches may read, using a strict field or the linear `$!` from `Prelude.Linear`.
A `Share` is represented by its target, so storing a `Share` in a strict owner evaluates that target too.
The runner guards evaluation demanded inside its action, including unboxed writes, while retaining strict arithmetic inside the run.
An explicit force outside the action, such as a bang on a wrapper function's argument, remains outside that guard; `-feager-blackholing` also defeats the documented protection.
Prefer `update` over read-then-write: it traverses once and can report what it replaced.

## Parallelism

- `parBO :: BO α a %1 -> BO α b %1 -> BO α (a, b)` runs both computations on separate threads and waits for both.
  Determinism comes from the types: the only way to give each side mutable access is to split a borrow into disjoint pieces first (read-only `Share`s may overlap).
- If a branch throws, `parBO` stops its sibling, waits until it stops, and rethrows the original exception; a loop without allocation can delay cancellation.
  Only that sibling is stopped: forks it started in a nested `parBO` can continue.
  An asynchronous exception to the waiting parent does not stop the branches, so a timeout bounds waiting rather than work.
  The divide-and-conquer scheduler below still does not propagate worker exceptions reliably; this fix applies to `parBO`.
- `Par α` is an applicative whose `(<*>)`/`liftA2` run their arguments in parallel: `runPar (f Control.<$> Par a Control.<*> Par b)`.
- `mapConcurrentlyOf :: Traversal s t a b -> (a %1 -> BO α b) -> s %1 -> BO α t` (and `forConcurrentlyOf`) processes every focus of a linear traversal in parallel, e.g. the element borrows returned by `VL.indicesMut`.
- Open a `reborrowing_` scope around the split-and-fork so that the whole `Mut` comes back without manual reunification.
- Forking per recursion step does not scale by itself; limit the depth with a budget (see `VL.qsort`) or use the work-stealing scheduler below.
- Use `-threaded` and `+RTS -N` to get real parallelism.

### Divide and conquer (`Control.Concurrent.DivideConquer.Linear`)

A borrow-safe work-stealing skeleton.
You describe the algorithm as a record over a mutable borrow:

```haskell
data DivideConquer c α t a r = DivideConquer
  { initialise :: forall β. (α >= β) => Mut β a %1 -> BO β (Ur c)
  , divide     :: forall β. (α >= β) => c -> Mut β a %1 -> BO β (Result c β t a r)
  , conquer    :: Conquer c α t a r
  }

data Result c β t a r = Done !r | Continue !(t (Ur c, Mut β a))

divideAndConquer ::
  (Data.Traversable t, α >= β, RandomGen g) =>
  g -> Int {- workers -} -> DivideConquer c α t a () -> Mut α a %1 -> BO β (Mut α a)
```

`divide` either finishes a piece or splits its borrow into a traversable of disjoint sub-borrows with per-piece data `c`; `conquer` combines results (or `NoConquer`).
`qsortDC`/`fftDC` are complete examples; `sequentialDivideAndConquer` and `naiveDivideAndConquer` (fork per node via `parBO`) run the same description without the scheduler, which is useful for testing.

## Performance notes

- The sublifetime scopes are erased at compile time; what remains is one opaque call at each scope exit.
- In read-only loops, `share` once outside and `subShare` inside rather than opening a `sharing` scope per iteration.
- Functions with hot loops over operations returning `(Ur a, container)` should be `INLINE`, `INLINABLE`, or specialised, so GHC can remove the transient tuples and `Ur` boxes.
- Moving a whole structured value (`move`, generic `Movable`) can hide a deep copy; move only the fields you need.
