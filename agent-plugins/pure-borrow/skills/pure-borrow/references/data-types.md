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

The simplest way to keep a GC-owned value beside linear ones is a linear field of type `Ur c`, as in `data Tagged = Tagged !(Ur Text) !(Ref Int)`, which linear-generics' Template Haskell accepts.
`Ur a` has `Consumable`, `Dupable`, `Movable` and `Copyable` instances of its own, so every class above derives over such a field, `Copyable` without asking anything of `a`, and `split` keeps it as it is (see "Splitting borrows of your own types" below).
`Ur a` also has `Clone`, which shares its GC-owned payload without requiring `Clone a`; do not add an orphan instance for it.
The derived `Clone` and `Copyable` evaluate what the box holds, and a strict field `!(Ur Int)` holds a boxed `Int`, where a nonlinear `!Int` field is unboxed under optimisation.

A field that must be nonlinear, because the constructor takes the value unwrapped or the field must stay unboxed, needs a hand-written instance.
linear-generics' Template Haskell cannot derive `Generic` for a type with a nonlinear field, such as `Tagged :: !Text -> !(Ref Int) %1 -> Tagged` in GADT syntax.
That limits the splice, not the classes: write the instance of linear-generics' `Generic` by hand, with each nonlinear field under `MP1 'Many`, and the `Generically` instances derive as for any other type.

```haskell
{-# LANGUAGE DataKinds, DerivingVia, TypeFamilies, UndecidableInstances #-}
import Data.Ref.Linear (Ref)
import Data.Text (Text)
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
  {-# INLINE [1] from #-}
  from tagged = fromTagged tagged
    where
      fromTagged :: Tagged %1 -> Rep Tagged p
      fromTagged (Tagged label ref) = M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (K1 ref)))
  {-# INLINE [1] to #-}
  to rep = toTagged rep
    where
      toTagged :: Rep Tagged p %1 -> Tagged
      toTagged (M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (K1 ref)))) = Tagged label ref

deriving via Generically Tagged instance Clone Tagged   -- shares the label, clones the Ref
deriving via Generically Tagged instance Consumable Tagged
-- plus the Copyable/Movable bans above
```

- `FixupMetaData`, from linear-base, fills the positions written `GHC.Any` with the metadata of GHC's `Generic` instance, which is derived for that alone.
- `from` and `to` must do nothing but take the constructor apart and put it back.
  GHC checks their multiplicities, so a linear field cannot go under `MP1 'Many` as it is; never make one fit by moving it or by any other operation, because the derived instances would apply that operation to a structure that is only borrowed.
- Mark `from` and `to` `INLINE [1]`, so that the generic representation of a type with many fields does not survive at run time.
- The derived instances ask nothing of a nonlinear field's type, since the field is GC-owned: `Clone` and `Dupable` share it, `Movable` keeps it, and `Consumable` drops it.
  `Copyable` is the exception and still requires a `Copyable` instance of that type.
- `split` keeps such a field too, given a hand-written instance of linear-generics' `Generic1`: see "Splitting borrows of your own types" below.
- The derived `Clone` shares a nonlinear field without evaluating it, where it evaluates what an `Ur` field holds; the derived `Copyable` evaluates both.
- Never write such an instance for an owner type that hides its constructor: see the owner-type rules in [extending.md](extending.md).

Without any generic instance, hand-roll `Consumable` and `Dupable`, which are ordinary pattern matches: drop or share the nonlinear fields, and handle the linear ones.
Do not hand-roll `Clone` or `Copyable`: they must look inside a borrow, which only the trusted `Control.Monad.Borrow.Pure.BO.Unsafe` module allows.

## Splitting borrows of your own types

`split :: DistributesAlias f => Alias ak (f x) %1 -> f (Alias ak x)` turns a borrow of a structure into a structure of borrows, for any kind of alias (`Mut`, `Share`, or `Lend`):

```haskell
{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, TemplateHaskell, TypeFamilies #-}
import Data.Ref.Linear (Ref)
import Data.Ref.Linear.Borrow qualified as RefB
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

- A `DistributesAlias` instance is always derived, since the class exports no methods.
- Each linear field must be the last type parameter, a type with a `DistributesAlias` instance applied to it once, or `Ur c` for a `c` that does not mention that parameter: with the instance that `deriveGenericAnd1` makes, `Maybe a` and `!(Ur Text)` derive, and `Maybe [a]` does not.
  Any other linear field whose type does not mention that parameter is rejected with a custom error, as `!Text`, `!Int` and `Maybe (Ur Text)` are.
  So is `Ur a`, since a borrow stored in it would be unrestricted, while `Ur (Maybe a)` gets GHC's `No instance for DistributesAlias (Par1 :.: Ur)`.
- `split` keeps an `Ur c` field as it is, and never evaluates what it holds.
  A lazy field may hold an unevaluated box, which the lender and the split result then share; a strict field, `!(Ur c)`, evaluates the box when the structure is built.
- A sum beyond the sizes for which linear-generics' Template Haskell marks `from1` and `to1` `INLINE [1]` (two to four constructors with more than five fields in one of them, five to eight with more than two, nine to sixteen with more than one, or more than sixteen constructors) keeps its generic representation at run time, and boxes each unpacked strict `Ur` field again on every `split`; a hand-written `Generic1` instance marked `INLINE [1]`, as below, avoids both.
- A nonlinear field must have a type that does not mention that parameter, and `split` keeps it as it is.
  One that mentions the parameter is rejected with a custom error: `split` would store a `Mut` in it, and an unrestricted `Mut` breaks the linearity that every borrow relies on.
- Use `splitPair` for tuples and `splitEither` for `Either` (the class refuses them with a hint).
- `Data.Record.Linear.Borrow.Experimental.Split` and `…PatternMatch` split borrows of records field by field (experimental).
- The two halves of a `Mut` can be used in parallel (`parBO`), because they are disjoint.

Wrap a GC-owned field in `Ur`, and the Template Haskell suffices:

```haskell
{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, TemplateHaskell, TypeFamilies #-}
import Data.Ref.Linear (Ref)
import Data.Ref.Linear.Borrow qualified as RefB
import Data.Text (Text)
import Generics.Linear.TH (deriveGenericAnd1)

data Labelled a = Labelled !(Ur Text) a

deriveGenericAnd1 ''Labelled

deriving anyclass instance DistributesAlias Labelled

bumpLabelled :: Mut α (Labelled (Ref Int)) %1 -> BO α (Ur Text)
bumpLabelled ml = case split ml of
  Labelled label mref -> Control.do
    mref <- RefB.modify (+ 1) mref
    Control.pure (mref `lseq` label)
```

A field whose type depends on another parameter of the type goes under `Ur` the same way: `data Keyed k a = Keyed !(Ur k) a` derives `DistributesAlias (Keyed k)`.

A type whose constructor must take the field unwrapped needs a nonlinear field, and its instance of linear-generics' `Generic1` written by hand, as its `Generic` instance is in "Types mixing nonlinear and linear fields" above, with the field as `MP1 'Many (K1 i c)` directly under its `S1`:

```haskell
{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, TypeFamilies #-}
import Data.Ref.Linear (Ref)
import Data.Ref.Linear.Borrow qualified as RefB
import Data.Text (Text)
import GHC.Exts qualified as GHC
import Generics.Linear

data Named a where
  Named :: !Text -> a %1 -> Named a

instance Generic1 Named where
  type Rep1 Named =
    D1 GHC.Any (C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (Rec0 Text)) :*: S1 GHC.Any Par1))
  {-# INLINE [1] from1 #-}
  from1 named = fromNamed named
    where
      fromNamed :: Named a %1 -> Rep1 Named a
      fromNamed (Named label a) = M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (Par1 a)))
  {-# INLINE [1] to1 #-}
  to1 rep = toNamed rep
    where
      toNamed :: Rep1 Named a %1 -> Named a
      toNamed (M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (Par1 a)))) = Named label a

deriving anyclass instance DistributesAlias Named

bumpNamed :: Mut α (Named (Ref Int)) %1 -> BO α (Ur Text)
bumpNamed ml = case split ml of
  Named label mref -> Control.do
    mref <- RefB.modify (+ 1) mref
    Control.pure (mref `lseq` Ur label)
```

A linear field `g a` is represented as `Par1 :.: g` and built with `Comp1 (Par1 x)`.
The metadata positions are left as `GHC.Any` here, which `split` never reads; linear-base's `FixupMetaData1` fills them as `FixupMetaData` does above.

## Allocating composite values

Allocators take a `Linearly` token; duplicate it for each component:

```haskell
newPair :: Linearly %1 -> Pair (Ref Int)
newPair l = case dup l of
  (l1, l2) -> Pair (Ref.new 1 l1) (Ref.new 10 l2)

-- inside BO: allocate and borrow in one step
(mp, lend) <- borrowLinearlyM newPair
```
