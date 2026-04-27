{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
An experimental module for splitting a borrow of a record gradually and partially.
To be used when you need to access more than one fields of a borrowed record, and borrow-out each field as you go.
If you can just pattern match on the record borrow once, consider using "Data.Record.Linear.Borrow.Experimental.PatternMatch".

The API is subject to future change.
-}
module Data.Record.Linear.Borrow.Experimental.Split (
  -- * Label Type
  RecordLabel (),

  -- * Single Field Accessor
  (.#),
  -- $record-splitting
  splitRecord,
  SplitRecord (),
  SplittableRecord (),
  (-#),
  (+#),
  (!#),
) where

import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Data.Kind (Constraint)
import GHC.Base (Multiplicity (..), TYPE, Type)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeError (ErrorMessage (..), Unsatisfiable)
import GHC.TypeLits (Symbol, TypeError)
import Generics.Linear.TH
import Prelude.Linear hiding (All)
import Prelude.Linear.Generically qualified as GL
import Unsafe.Linear qualified as Unsafe

{- |
@'RecordLabel' r field a@ witnesses that the record type @r@ has a field named @field@ of type @a@.
To be used as a label argument for '(.#)', '(-#)', '(+#)', and '(!#)'.

The record label is usually constructed by overloaded labels as `#field` under `OverloadedLabels` extension.
-}
type RecordLabel :: TYPE rep -> Symbol -> Type -> Type
data RecordLabel r field a where
  RecLab :: (HasField field r a) => RecordLabel r field a

instance (HasField field r a, field ~ field') => IsLabel field (RecordLabel r field' a) where
  fromLabel = RecLab
  {-# INLINE fromLabel #-}

{- |
Borrow-level field accessor.
@record '.#' #field@ returns a borrow of the @field@ of the @record@ of the same kind.

@
{\-# LANGUAGE OverloadedLabels #-\}
data MyRecord = MyRecord {field :: Ref Int, otherField :: Vector String}

recordBor :: 'Borrow' bk α MyRecord
recordBor = ...

fieldOfRecordBor :: 'Borrow' bk α (Ref Int)
fieldOfRecordBor = recordBor '.#' #field

otherFieldOfRecordBor :: 'Borrow' bk α (Vector String)
otherFieldOfRecordBor = recordBor '.#' #otherField
@

In above example, we annotate type of the divided field borows for clarity, but the type can be inferred by the record type and labels.

For more complex, partial splitting of a record, see [Splitting a record borrow into pieces]("Data.Record.Linear.Borrow.Experimental#split") for more detail.
-}
(.#) ::
  forall field r a k α.
  Borrow k α r %1 ->
  RecordLabel r field a ->
  Borrow k α a
UnsafeAlias !r .# RecLab = UnsafeAlias $! Unsafe.toLinear (getField @field @r @a) r

infixl 9 .#

type family Lookup l ls where
  Lookup l '[] = 'Nothing
  Lookup l ('(l, v) ': xs) = 'Just v
  Lookup l ('(l', v) ': xs) = Lookup l xs

type family Delete l ls where
  Delete _ '[] = '[]
  Delete l ('(l, v) ': ls) = ls
  Delete l ('(l', v) ': ls) = '(l', v) ': Delete l ls

{- $record-splitting
= #split# Splitting a record borrow into pieces

'(.#)' is handy when you need only one field of a borrowed record, but not applicable when you need to access more than one fields.
For that purpose, we provide 'SplitRecord' machinery and associated combinators '(-#)', '(+#)', and '(!#)' for splitting a borrow of a record into borrows of its fields.

In such cases, however, we must ensure that each field of a record borrow is split out @at most once@.
Here, @'SplitRecord' a bk α fs@ comes int play: it is representationally same as @'Borrow' bk α a@, but only the fields in @fs@ remains unsplit.
That is, a field is borrowed by splitting combinator only if it remains in @fs@ type parameter, and the field is removed from @fs@ after splitting.

A record type can be converted into a 'SplitRecord' by 'splitRecord' function, which requires 'SplittableRecord' instance for the record type.
This can be derived generically by deriving 'GL.Generic' and then 'SplittableRecord' for the record type, as follows:

@
{\-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies, LinearTypes #-\}
import Generics.Linear.TH ('deriveGeneric')

data MyRecord = MyRecord {field :: 'Data.Ref.Linear.Ref' 'Int', otherField :: 'Data.Vector.Mutable.Linear.Borrow.Vector' 'String'}

'deriveGeneric' ''MyRecord

deriving anyclass instance 'SplittableRecord' MyRecord
@

Once we have 'SplittableRecord' instance derived, we can now split a record borrow partially, step-by-step using '(-#)', '(+#)', and '(!#)' combinators.
Through out this documentation, suppose we have the following record borrow in scope:

@
recordBor :: 'Mut' α MyRecord

splitRec ::
  'SplitRecord' MyRecord v'Mut' α
      '[ '("field", '( 'One, Ref Int)), '("otherField", '( 'One, Vector String))]
splitRec = splitRecord recordBor
@

== Borrows-out a linear field

When you want to borrow-out a single linear field from the record, you can use '(-#)' combinator, as follows:

@
fieldBor :: 'Mut' α (Ref Int)
restBor :: 'SplitRecord' MyRecord Mut α '[ '(otherField, '( 'One, Vector String)))]
(fieldBor, restSplit) = splitRec '-#' #field
@

Here, the borrow to the @field@ of @splitRec@ is borrowed out as @fieldBor@, and the remaining borrow is represented by @restSplit@, where only @otherField@ remain unsplit.
We can no longer borrow-out @field@ from @restSplit@ by the type constraints.

== Consuming a split record

When you no longer need to borrow any field of a split record, you can just 'consume' a split record, or call '(!#)' to borrow out a single field from the split record and discard the rest of the record borrow, as follows:

@
otherFieldBor :: Mut α (Vector String)
otherFieldBor = restSplit '!#' #otherField
@

'(!#)' is analogous to '(.#)', but it acts on 'SplitRecord' instead of borrow of a record.

== APIs
-}

{- |
@'SplitRecord' a bk α fs@ represents a borrow of a value of type @a@ of borrow kind @bk@ (i.e. 'Share' or 'Mut') for lifetime @α@ with @fs@ remains unsplit.
That is, if the field @f@ is removed by some combinators like '(-#)', then the resulting 'SplitRecord' will have @f@ removed from the @fs@ type-level list.

At any time, you can 'consume' 'SplitRecord' when the remaining fields are no longer of interest.
-}
type SplitRecord :: Type -> BorrowKind -> Lifetime -> [(Symbol, (Multiplicity, Type))] -> Type
newtype SplitRecord a bk α s = SplitRecord (Borrow bk α a)

instance Consumable (SplitRecord a bk α fs) where
  consume (SplitRecord a) = consume a
  {-# INLINE consume #-}

class (Lookup l xs ~ 'Just v) => Member l xs v | l xs -> v

instance (Lookup l xs ~ 'Just v) => Member l xs v

type All_ :: (k -> Constraint) -> [k] -> Constraint
type family All_ c xs where
  All_ c '[] = ()
  All_ c (x ': xs) = (c x, All c xs)

type All :: (k -> Constraint) -> [k] -> Constraint
class (All_ c xs) => All c xs

instance All c '[]

instance (c x, All c xs) => All c (x ': xs)

type family IsFieldOf_ a xs where
  IsFieldOf_ a '(l, '(_, v)) = HasField l a v

class (IsFieldOf_ a x) => IsFieldOf a x

instance (HasField l a v) => IsFieldOf a '(l, '(m, v))

type SplittableRecord :: Type -> Constraint
class (All (IsFieldOf a) (Fields a)) => SplittableRecord a where
  type Fields a :: [(Symbol, (Multiplicity, Type))]
  type Fields a = GFields (GL.Rep a)

type GSplittableRecord :: (Type -> Type) -> Constraint
class GSplittableRecord f where
  type GFields f :: [(Symbol, (Multiplicity, Type))]

type family ls ++ rs where
  '[] ++ rs = rs
  (x ': xs) ++ rs = x ': (xs ++ rs)

instance
  (Unsatisfiable ('Text "A union type cannot be a splittable record")) =>
  GSplittableRecord (f GL.:+: g)
  where
  type GFields (f GL.:+: g) = TypeError ('Text "A union type cannot be a splittable record")

instance (GSplittableRecord f) => GSplittableRecord (GL.D1 i f) where
  type GFields (GL.D1 i f) = GFields f

instance (GSplittableRecord f) => GSplittableRecord (GL.C1 i f) where
  type GFields (GL.C1 i f) = GFields f

instance
  (Unsatisfiable ('Text "A record field must have a name")) =>
  GSplittableRecord (GL.S1 ('GL.MetaSel 'Nothing unp str str') (GL.K1 i c))
  where
  type GFields (GL.S1 ('GL.MetaSel 'Nothing unp str str') (GL.K1 i c)) = TypeError ('Text "A record field must have a name")

type MultOf :: Type -> Multiplicity
type family MultOf c where
  MultOf (Ur x) = 'Many
  MultOf x = 'One

instance
  (GSplittableRecord f) =>
  GSplittableRecord (GL.S1 ('GL.MetaSel ('Just name) unp str str') (GL.K1 i c))
  where
  type GFields (GL.S1 ('GL.MetaSel ('Just name) unp str str') (GL.K1 i c)) = '[ '(name, '((MultOf c), c))]

instance (GSplittableRecord f, GSplittableRecord g) => GSplittableRecord (f GL.:*: g) where
  type GFields (f GL.:*: g) = GFields f ++ GFields g

-- | Start subdividing a borrow of a record.
splitRecord :: (SplittableRecord a) => Borrow bk α a %m -> SplitRecord a bk α (Fields a)
splitRecord !bor = SplitRecord bor
{-# INLINE splitRecord #-}

{- |
Splitting a linear field from a borrow of a record.
@record '-#' #field@  returns a pair of the borrow of a split field and remaining split record, where @field@ is removed from the type-level list of the remaining split record.

Mnemonic: '(-#)' _subtracts_ the field from the record.
-}
(-#) ::
  (SplittableRecord a, Lookup field fs ~ 'Just '( 'One, x)) =>
  SplitRecord a bk α fs %m ->
  RecordLabel a field x ->
  (Borrow bk α x, SplitRecord a bk α (Delete field fs))
(-#) = Unsafe.toLinear \(SplitRecord !bor) lab ->
  let !fieldBor = bor .# lab
      !restBor = SplitRecord bor
   in (fieldBor, restBor)
{-# INLINE (-#) #-}

{- |
Extracting the borrow to the single linear field form a borrow of a record.
@record '!#' #field@ returns a borrow of the @field@ of the @record@, discarding the borrow to the rest of the record.

Mnemonic: '(!#)' _destructs_ a borrow of a record to that of a single field.
-}
(!#) ::
  (SplittableRecord a, Lookup field fs ~ 'Just '( 'One, x)) =>
  SplitRecord a bk α fs %m ->
  RecordLabel a field x ->
  Borrow bk α x
(!#) = Unsafe.toLinear \(SplitRecord !bor) lab ->
  let !fieldBor = bor .# lab
   in bor `lseq` fieldBor
{-# INLINE (!#) #-}

{- |
Skimming the value of a nonlinear (unrestricted) field.
@record '+#' #field@ returns a pair of the _value_ of the @field@ of the @record@ and the original split record.
The returned value of @field@ is wrapped by 'Ur' and can be used more than once.

Mnenonic: '(+#)' you can use nonlinear field _more_ (@+@) than once.
-}
(+#) ::
  (SplittableRecord a, Lookup field fs ~ 'Just '( 'Many, Ur x)) =>
  SplitRecord a bk α fs %m ->
  RecordLabel a field (Ur x) ->
  (Ur x, SplitRecord a bk α fs)
(+#) = Unsafe.toLinear \recd@(SplitRecord !bor) lab ->
  let UnsafeAlias !field = bor .# lab
   in (field, recd)
{-# INLINE (+#) #-}

infix 9 -#, +#, !#

data Hoge = Hoge {foo :: Int, bar :: Ur String, buz :: Bool}

deriveGeneric ''Hoge

deriving anyclass instance SplittableRecord Hoge
