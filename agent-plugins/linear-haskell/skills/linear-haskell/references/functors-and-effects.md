# Functors, monads, effects, and allocation

## Two functor hierarchies

linear-base splits `Functor`/`Applicative` into a *data* hierarchy (`Data.Functor.Linear`, import qualified as `Data`) and a *control* hierarchy (`Control.Functor.Linear`, import qualified as `Control`):

| Method | Data (containers) | Control (effects) |
| --- | --- | --- |
| `fmap` | `(a %1 -> b) -> f a %1 -> f b` | `(a %1 -> b) %1 -> f a %1 -> f b` |
| `pure` | `a -> f a` | `a %1 -> f a` |
| `(<*>)` | `f (a %1 -> b) %1 -> f a %1 -> f b` | same |
| `liftA2` | `(a %1 -> b %1 -> c) -> f a %1 -> f b %1 -> f c` | `(a %1 -> b %1 -> c) %1 -> f a %1 -> f b %1 -> f c` |
| `(>>=)` | — | `m a %1 -> (a %1 -> m b) %1 -> m b` |

- A data functor may contain zero or many elements (lists, `Maybe`, `V n`, `Ur`), so the mapped function must be unrestricted.
- A control functor threads exactly one value through an effect (linear `State`, `Reader`, `IO`, `RIO`, pure-borrow's `BO`), so the function is used once and may itself be linear.
- There is no data monad; `Monad` and `MonadFail` exist only in the control hierarchy.
- Every control `Functor`/`Applicative` is a data one, so `Data.fmap` works on control functors too.
- `Data.traverse :: (Traversable t, Control.Applicative f) => (a %1 -> f b) -> t a %1 -> f (t b)`.

## Writing monadic code

```haskell
import Control.Functor.Linear qualified as Control

bump :: Int -> Control.State (Array Int) ()
bump i = Control.do
  Ur v <- Control.state (\arr -> Array.read arr i)
  Control.modify (\arr -> Array.write arr i (v + 1))

bumpAll :: Control.State (Array Int) ()
bumpAll = Control.do
  bump 0
  bump 1

run :: Array Int %1 -> ((), Array Int)
run = Control.runState bumpAll
```

- Statements without `<-` must have type `m ()`.
- Use `Control.pure`/`Control.return`; `QualifiedDo` does not qualify them for you.
- `Control.get :: (Applicative m, Dupable s) => StateT s m s` duplicates the whole state; for a mutable state that is a deep copy.
  Prefer `Control.state` with a function that hands the state back, as above.

## Interoperating with ordinary monads: `UrT`

```haskell
newtype UrT m a = UrT (m (Ur a))
runUrT :: UrT m a %1 -> m (Ur a)
liftUrT :: (Movable a, Control.Functor m) => m a %1 -> UrT m a
evalUrT :: Control.Functor m => UrT m a %1 -> m a
```

`UrT m` is an ordinary Prelude `Monad` whenever `m` is a linear one, because every result is unrestricted.
Use it to run base combinators (`traverse`, `mapM_`, `foldM`, …) over linear effects when the results are unrestricted, e.g. `runUrT (traverse (\k -> UrT (lookupU k)) keys)`.
It adds an `Ur` box per step; it does not help when results are linear.

## Effects and exceptions

Linearity only promises release when the result is fully consumed; an exception bypasses that.

- **Linear `IO`** (`System.IO.Linear`): `fromSystemIO`, `fromSystemIOU :: System.IO a -> IO (Ur a)`, `withLinearIO :: IO (Ur a) -> System.IO a`.
- **`RIO`** (`System.IO.Resource.Linear`, import qualified; unrelated to the `rio` package): resources acquired in `RIO` are released if an exception escapes `run`.

  ```haskell
  import System.IO.Resource.Linear qualified as RIO

  writeHello :: FilePath -> P.IO ()
  writeHello path = RIO.run Control.do
    h <- RIO.openFile path RIO.WriteMode
    h <- RIO.hPutStrLn h (Text.pack "hello")
    RIO.hClose h
    Control.pure (Ur ())
  ```

  New kinds of resources are registered with `unsafeAcquire` / `unsafeFromSystemIOResource`.
- **Manual off-heap memory** (`Foreign.Marshal.Pure`, import qualified): `withPool :: Movable b => (Pool %1 -> b) %1 -> b`, `alloc :: Representable a => a %1 -> Pool %1 -> Box a`, `deconstruct :: Representable a => Box a %1 -> a`.
  If an exception is raised, everything still allocated in the pool is deallocated.
  `dup` on a `Pool` returns another handle to the *same* pool (unlike the independent copies `Dupable` usually means), so you can allocate from several places; `withPool` creates a separate pool.

## Allocation styles

1. **Scope-passing** (linear-base containers):

   ```haskell
   Array.alloc :: (HasCallStack, Movable b) => Int -> a -> (Array a %1 -> b) %1 -> b
   Array.fromList :: (HasCallStack, Movable b) => [a] -> (Array a %1 -> b) %1 -> b
   ```

   Only a `Movable` result can leave, so the resource cannot escape (linear-base < 0.5 required `Ur b`).
   Allocating several resources nests the continuations, and none of them can be returned early, because a linear resource is not `Movable` (the "sticky end of scopes" problem).
2. **Beside an existing resource**: `Array.allocBeside :: Int -> a -> Array b %1 -> (Array a, Array b)` uses an existing linear value as evidence of a linear context.
3. **Linearity tokens**: a dedicated `Linearly` token, which only `linearly :: Movable a => (Linearly %1 -> a) %1 -> a` can create from nothing, is `Dupable` and `Consumable` but never `Movable`, and every allocator takes one: `fromList :: [a] %1 -> Linearly %1 -> Vector a`.
   Allocation becomes sequential code instead of nested continuations.
   linear-base does not provide this; pure-borrow's `Control.Monad.Borrow.Pure` does (`linearly`, `Linearly`, `withLinearly`), and it approximates the proposed linear constraints (`Linearly %1 => …`, GHC proposal 621).

## Other linear-base modules worth knowing

- `Control.Optics.Linear`: linear lenses, prisms and traversals (`lens`, `traverseOf`, …).
- `Data.Array.Polarized` (`Push`/`Pull` arrays): build immutable vectors with a single allocation without relying on fusion.
- `Data.HashMap.Mutable.Linear`, `Data.Set.Mutable.Linear`, `Data.Vector.Mutable.Linear`, `Data.Array.Mutable.Linear`: scope-passing mutable containers whose elements are unrestricted.
- `Streaming.Linear`: linear streams.
- `Data.V.Linear`: fixed-length vectors, handy for returning *n* linear values.
