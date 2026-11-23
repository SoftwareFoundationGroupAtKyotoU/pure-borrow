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

linear-generics' Template Haskell cannot derive `Generic` for a type with a nonlinear field, such as `Tagged :: !Text -> !(Ref Int) %1 -> Tagged` in GADT syntax.
That limits the splice, not the classes: write the instance of linear-generics' `Generic` by hand, with each nonlinear field under `MP1 'Many`, and the `Generically` instances derive as for any other type.

```haskell
{-# LANGUAGE DataKinds, DerivingVia, TypeFamilies, UndecidableInstances #-}
import GHC.Exts qualified as GHC
import GHC.Generics qualified as GHC
import Generics.Linear
import Prelude.Linear.GenericUtil (FixupMetaData)

data Tagged where
  Tagged :: !Text -> !(Ref Int) %1 -> Tagged
  deriving stock (GHC.Generic)   -- GHC's own Generic, read only for the metadata

instance Generic Tagged where
  type Rep Tagged =
    FixupMetaData Tagged
      (D1 GHC.Any (C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (Rec0 Text)) :*: S1 GHC.Any (Rec0 (Ref Int)))))
  -- from and to have a polymorphic multiplicity, so match in a helper that has a linear type.
  from tagged = fromTagged tagged
    where
      fromTagged :: Tagged %1 -> Rep Tagged p
      fromTagged (Tagged label ref) = M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (K1 ref)))
  to rep = toTagged rep
    where
      toTagged :: Rep Tagged p %1 -> Tagged
      toTagged (M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (K1 ref)))) = Tagged label ref

deriving via Generically Tagged instance Clone Tagged   -- shares the label, clones the Ref
deriving via Generically Tagged instance Consumable Tagged
-- plus the Copyable/Movable bans above
```

- `FixupMetaData`, from linear-base, fills the positions written `GHC.Any` with the metadata of GHC's `Generic` instance, which is derived for that alone.
- A representation that puts a linear field under `MP1 'Many` does not compile, because `from` and `to` are linear, so a derived instance never shares a linear field by mistake.
- The derived instances ask nothing of a nonlinear field's type, since the field is GC-owned: `Clone` and `Dupable` share it, `Movable` keeps it, and `Consumable` drops it.
  `Copyable` is the exception and still requires a `Copyable` instance of that type.
- `split` stays unavailable.
  A `DistributesAlias` instance can only be derived, since the class exports no methods, and the deriving rejects every nonlinear field of a hand-written instance of linear-generics' `Generic1` with `Nonlinear fields cannot distribute borrows!`.
  For a nonlinear field that holds the type parameter the rejection is essential: `split` would store a `Mut` in it, and two live `Mut`s to the same data would follow.

Two alternatives need no hand-written instance:

- Use plain syntax and wrap the unrestricted field in `Ur` (`data Tagged = Tagged !(Ur Text) !(Ref Int)`), which `deriveGeneric` accepts: `Ur a` has `Consumable`, `Dupable`, `Movable`, and `Copyable` instances of its own.
  `Ur a` also has `Clone`, which shares its GC-owned payload without requiring `Clone a`; do not add an orphan instance for it.
- Hand-roll `Consumable` and `Dupable`, which are ordinary pattern matches: drop or share the nonlinear fields, and handle the linear ones.
  Do not hand-roll `Clone` or `Copyable`: they must look inside a borrow, which only the trusted `Control.Monad.Borrow.Pure.BO.Unsafe` module allows.

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
