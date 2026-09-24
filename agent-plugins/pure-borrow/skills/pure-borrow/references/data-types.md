# Your own data types with pure-borrow

## Which class does what

| Class | Signature | Meaning |
| --- | --- | --- |
| `Copyable a` | `copy :: Borrow bk α a %1 -> a` | Pure copy out of a live borrow. Evaluating `copy` must finish the whole copy and return it in WHNF, so the result never depends on the borrow staying alive. |
| `Clone a` | `clone :: Share α a %1 -> BO α a` | Effectful duplicate inside `BO`, so the copy stays linear. Works for mutable values too. |
| `Movable a` | `move :: a %1 -> Ur a` | Move an **owned** value onto the GC heap. Required when a container's elements leave linear ownership. |
| `Consumable a` | `consume :: a %1 -> ()` | Release an owned value. Borrows (`Mut`, `Share`) are always `Consumable`; a `Lend` never is. |

- `copy` consumes the borrow linearly.
  That costs nothing for a `Share` (it is unrestricted), but a `Mut` is gone afterwards; `copyMut :: Copyable a => Mut α a %1 -> Ur a` wraps the copy in `Ur` for convenience.
- `copy` is **not** an operation on nonlinear data.
  A value bound through an ordinary arrow or held in `Ur` is GC-owned: copy, keep, or drop it freely, without `Copyable`, `Clone`, `Dupable`, or `Consumable`.

## Mutable types are never `Copyable` (or `Movable`)

A `Share` is `Movable`, so a shared borrow can reach unrestricted code (`Ur (Share α a)`) and outlive the `BO` computation that created it.
If `copy` worked on a type with mutable state, copying through such a `Share` would give unrestricted code an alias of state that someone else still mutates.
`Clone` avoids the problem because its result only exists inside `BO`, i.e. linearly.

The library already bans `Copyable` for `Vector`, `GrowableVector`, `Ref`, `HashMap`, and linear-base's `Array`/`Vector`/`HashMap`/`Set`, with messages like `VL.Vector Int cannot be copied!`.
Ban it, and `Movable`, for your own mutable types:

```haskell
{-# LANGUAGE DataKinds, UndecidableInstances #-}
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)

instance
  (Unsatisfiable ('Text "Account owns a mutable Ref; clone it inside BO instead")) =>
  Copyable Account
  where
  copy = unsatisfiable

instance
  (Unsatisfiable ('Text "Account owns a mutable Ref and cannot be moved into Ur")) =>
  Movable Account
  where
  move = unsatisfiable
```

## Built-in clones

Pure-borrow's element-owning fixed and growable vectors clone each initialized element with `Clone a`, including the unboxed families (`Unbox a` is also needed).
Growable copies keep their capacity.
Linear-base's `Array`, `Vector`, `HashMap`, and `Set` instead copy their backing storage and share their GC-owned elements, with no element `Clone` constraint.
The library includes `Clone` and `Copyable` for `Identity`, `Down`, `Const`, `Dual`, semigroup `First`/`Last`, `WrappedMonoid`, `Any`, `All`, `Alt`, and tuples through arity six, with the corresponding component constraints.
For a custom unboxed newtype, derive `Clone` from its contents; never infer that a shallow buffer copy is safe from `Unbox` alone.
Do not add an overlapping orphan for these instances.

## Deriving

All of the following were checked with GHC 9.10.3 and 9.12.4.
Generic deriving needs linear-generics' `Generic` (from `Generics.Linear.TH`, not GHC's `deriving Generic`) and the extensions `TemplateHaskell`, `TypeFamilies`, `DataKinds`, and `DerivingVia`.

An immutable record:

```haskell
import Control.Monad.Borrow.Pure.Clone (AsCopyable (..))
import Data.Unrestricted.Linear (AsMovable (..))
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear.Generically (Generically (..))

data Point = Point !Int !Int

deriveGeneric ''Point

deriving via Generically Point instance Copyable Point
deriving via AsCopyable Point instance Clone Point           -- Clone from Copyable
deriving via Generically Point instance Movable Point
deriving via AsMovable Point instance Consumable Point       -- Consumable/Dupable from Movable
deriving via AsMovable Point instance Dupable Point
```

A record owning mutable state:

```haskell
data Account = Account !Int !(Ref Int)   -- plain syntax: every field linear

deriveGeneric ''Account

deriving via Generically Account instance Clone Account          -- needs Clone Int, Clone (Ref Int)
deriving via Generically Account instance Consumable Account
-- plus the Copyable/Movable bans above
```

Remember that every field of a plain `data` declaration is linear: consuming an `Account` means consuming its `Int` too (`i \`lseq\` …`), not matching it with `_`.

### Types mixing nonlinear and linear fields

linear-generics cannot derive `Generic` for a type whose constructor mixes nonlinear and linear fields (GADT syntax such as `Tagged :: !Text -> !(Ref Int) %1 -> Tagged`), so none of the `Generically` instances are available for it.
Hand-roll the instances instead:

- linear-base classes (`Consumable`, `Dupable`) are ordinary pattern matches: drop or share the nonlinear fields, and handle the linear ones.
- `DistributesAlias` exports no methods, so it can only be derived; such a type cannot have `split`.
  Never try to route a borrow through a nonlinear field: two live `Mut`s to the same data would follow.
- `Clone` and `Copyable` must look inside a borrow, which only the trusted `Control.Monad.Borrow.Pure.BO.Unsafe` module allows.
  Treat such an instance as a proof obligation and state why it is sound; clone each linear field with its own `Clone` instance, as below, rather than through `Dupable`:

  ```haskell
  import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..))

  data Tagged where
    Tagged :: !Text -> !(Ref Int) %1 -> Tagged

  -- Trusted: the label is immutable and GC-owned, so the clone may share it;
  -- the Ref is cloned through a shared borrow of the same lifetime.
  instance Clone Tagged where
    clone (UnsafeAlias (Tagged label ref)) = Tagged label Control.<$> clone (UnsafeAlias ref)
  ```

To keep the linear-base classes and `Copyable` derivable instead, use plain syntax and wrap the unrestricted field in `Ur` (`data Tagged = Tagged !(Ur Text) !(Ref Int)`): `Ur a` has `Consumable`, `Dupable`, `Movable`, and `Copyable` instances of its own.
`Ur a` also has `Clone`, which shares its GC-owned payload without requiring `Clone a`; do not add an orphan instance for it.

## Splitting borrows of your own types

`split :: DistributesAlias f => Alias ak (f x) %1 -> f (Alias ak x)` turns a borrow of a structure into a structure of borrows, for any kind of alias (`Mut`, `Share`, or `Lend`):

```haskell
{-# LANGUAGE DeriveAnyClass, TemplateHaskell, TypeFamilies, DataKinds #-}
import Generics.Linear.TH (deriveGenericAnd1)

data Pair a = Pair a a

deriveGenericAnd1 ''Pair

deriving anyclass instance DistributesAlias Pair

bumpBoth :: Mut α (Pair (Ref Int)) %1 -> BO α ()
bumpBoth mp = case split mp of
  Pair ml mr -> Control.do
    ml <- RefB.modify (+ 1) ml
    mr <- RefB.modify (+ 1) mr
    Control.pure (ml `lseq` consume mr)
```

- `DistributesAlias` is only derivable when the parameter occurs in linear fields; nonlinear fields and fields of other types are rejected with custom errors.
- Use `splitPair` for tuples and `splitEither` for `Either` (the class refuses them with a hint).
- `Data.Record.Linear.Borrow.Experimental.Split` and `…PatternMatch` split borrows of records field by field (experimental).
- The two halves of a `Mut` can be used in parallel (`parBO`), because they are disjoint.

## Allocating composite values

Allocators take a `Linearly` token; duplicate it for each component:

```haskell
newPair :: Linearly %1 -> Pair (Ref Int)
newPair l = case dup l of
  (l1, l2) -> Pair (Ref.new 1 l1) (Ref.new 10 l2)

-- inside BO: allocate and borrow in one step
(mp, lend) <- borrowLinearlyM newPair
```
