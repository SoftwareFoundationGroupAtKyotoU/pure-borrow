# Data types and the resource classes

## `Ur`

```haskell
data Ur a where
  Ur :: a -> Ur a   -- GADT syntax: the field is unrestricted
```

- Matching `Ur x` (strictly) is enough to consume an `Ur a`; the unwrapped `x` may then be used any number of times.
- A linearly bound value cannot be placed in `Ur` directly: the constructor's field is unrestricted.
  Only `move` (for `Movable` types) or a type-specific operation such as `freeze`/`toList` produces an `Ur`.
- `data Ur a = Ur a` would be wrong: ordinary fields are linear.
- Intuition: `Ur a` is "an `a` on the GC heap with possibly many owners"; for logicians it is the exponential `!a`.
- Helpers: `unur :: Ur a %1 -> a`, `Data.Unrestricted.Linear.lift`/`lift2` to map over `Ur`, and `UrT` (see `references/functors-and-effects.md`).

## The three classes

```haskell
class Consumable a where
  consume :: a %1 -> ()

class Consumable a => Dupable a where
  dup2 :: a %1 -> (a, a)   -- or dupR :: a %1 -> Replicator a; define at least one

class Dupable a => Movable a where
  move :: a %1 -> Ur a
```

| Class | Mental model | Obligation |
| --- | --- | --- |
| `Consumable` | The resource can be released linearly and purely (Rust's `Drop`). | Release everything the value uniquely owns: consume linear fields, free off-heap memory. |
| `Dupable` | The resource can be duplicated any finite number of times in the middle of a linear computation. | The copies must be independent: deep-copy any mutable state, or purity and determinism break. |
| `Movable` | The value can be moved onto the GC heap, where it may have many owners. | Never for a type that is mutated purely or concurrently. |

Useful derived functions: `lseq :: Consumable a => a %1 -> b %1 -> b`, and `dup`, `dup2`, `dup3` from `Prelude.Linear` (`dup4` … `dup7` are only exported by `Data.Unrestricted.Linear`).

## Designing a data type

Decide the multiplicity of each field:

| The field holds … | Bind it … |
| --- | --- |
| mutable state (arrays, references, hash tables, handles, pools) | linearly |
| a value that needs a unique owner for any other reason | linearly |
| an immutable value | nonlinearly |
| a value that is already `Movable` | nonlinearly (or linearly, if you want `deriveGeneric`; see below) |

Ordinary `data` syntax makes every field linear, so a type with only linear fields needs nothing special.
To mix multiplicities, use GADT syntax, even for a plain algebraic data type:

```haskell
data Histogram where
  Histogram :: !Text -> !(Array Int) %1 -> Histogram
```

On GHC ≥ 9.14 only, record syntax can say the same: `data Histogram = Histogram { label %'Many :: Text, counts :: Array Int }`, which needs `DataKinds` and `import GHC.Exts (Multiplicity (..))`.

Consuming a value means consuming its linear fields: with plain syntax even an `Int` field must be consumed (`i \`lseq\` …`), not discarded with `_`.

## Instances for mutable types

A mutable type should be `Consumable`, may be `Dupable` (only with a real deep copy), and must ban `Movable`:

```haskell
{-# LANGUAGE DataKinds, UndecidableInstances #-}
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)

instance Consumable Histogram where
  consume (Histogram _label arr) = consume arr   -- _label is nonlinear: dropping it is fine

instance Dupable Histogram where
  dup2 (Histogram label arr) = case dup2 arr of   -- Array's Dupable copies the buffer
    (arr1, arr2) -> (Histogram label arr1, Histogram label arr2)

instance
  (Unsatisfiable ('Text "Histogram owns a mutable Array; it cannot be moved into Ur")) =>
  Movable Histogram
  where
  move = unsatisfiable
```

Any use of `move` at `Histogram` now fails with the custom message.
Prefer `GHC.TypeError.Unsatisfiable` (GHC ≥ 9.8): its given can discharge any constraint, including `Movable`'s `Dupable` superclass, so the ban compiles even when the type is not `Dupable`.
linear-base's `Prelude.Linear.Unsatisfiable` is an ordinary class and needs a real `Dupable` instance to exist.
Apply the same pattern to any other instance that could conjure an unrestricted value of a mutable type, above all Prelude's `Monoid` (`mempty :: a`); a Prelude `Semigroup` is merely useless for such a type, and linear-base ≥ 0.7 bans it for its mutable `HashMap` and `Set`.

## Generic deriving

linear-generics provides a multiplicity-aware `Generic`, and linear-base's classes have `Generically` instances:

```haskell
{-# LANGUAGE DataKinds, DerivingVia, TemplateHaskell, TypeFamilies #-}
import Data.Unrestricted.Linear (AsMovable (..))
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear.Generically (Generically (..))

data Summary = Summary !Int !Int

deriveGeneric ''Summary   -- TemplateHaskell, TypeFamilies and DataKinds are all required

deriving via Generically Summary instance Movable Summary

-- Once Movable exists, Consumable and Dupable follow from it.
deriving via AsMovable Summary instance Consumable Summary
deriving via AsMovable Summary instance Dupable Summary
```

- `deriving via Generically T instance Consumable T` (and `Dupable`, `Movable`) works directly when all fields have the corresponding instances.
- `AsMovable` derives `Consumable` and `Dupable` from `Movable`; this is the cheapest route for immutable data.
- `deriveGenericAnd1 ''F` also derives `Generic1`, for classes over type constructors (`deriving via Generically1 F instance Data.Functor F`).
- Place the Template Haskell splice after the data declaration and before any instance that uses it.
- GHC's own `deriving Generic` does **not** produce the linear-generics class; use the Template Haskell splice.

**Limitation.**
linear-generics' Template Haskell cannot derive `Generic` for a type with any nonlinear field, so in particular not for a type that mixes nonlinear and linear fields (`deriveGeneric` generates code that fails with `Couldn't match type 'm' with 'Many'`).
For such types, choose one of:

- Hand-roll the class instances, as for `Histogram` above (the usual choice).
- Use plain syntax with the immutable field wrapped in `Ur` (`data T = T !(Ur Text) !(Array Int)`), if generic deriving matters more than the field multiplicity; `Ur` itself has the linear-base instances.
- Write the `Generic` instance by hand, representing each nonlinear field with `MP1 'Many` (see the `Generics.Linear` documentation); linear-base's generic instances understand it.

## Allocation of composite linear values

A constructor applied to linear values is itself linear, so building a record of several resources needs each resource first.
With scope-passing allocators this nests continuations; with a `Linearly` token (pure-borrow) it is sequential:

```haskell
-- pure-borrow style: allocate beside a duplicated linearity witness.
import Control.Monad.Borrow.Pure (Linearly)
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref

data Pair a = Pair a a

newPair :: Linearly %1 -> Pair (Ref Int)
newPair l = case dup l of
  (l1, l2) -> Pair (Ref.new 1 l1) (Ref.new 10 l2)
```
