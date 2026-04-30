{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
An experimental module for splitting a borrow of a record by pattern matching on it.
If you want to split out a field gradually and partially, see also "Data.Record.Linear.Borrow.Experimental.Split".

The API is subject to future change.
-}
module Data.Record.Linear.Borrow.Experimental.PatternMatch (
  -- * Label Type
  RecordLabel,

  -- * Single Field Accessor
  (.#),

  -- * #split# Splitting a record borrow into pieces
  -- $record-splitting

  -- ** APIs
  (.@),
  RecordLabels,
  FieldBorrows,
  LabelsOrBorrows (..),

  -- ** Internal APIs
  RecordEliminator (..),
  RecordLabel' (..),
) where

import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine (Affine (..), AsAffine (..))
import Control.Monad.Borrow.Pure.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.Pure.BO.Unsafe (unsafeMapAlias)
import Data.Kind (Constraint)
import GHC.Base (TYPE, Type, proxy#)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeError (ErrorMessage (..), Unsatisfiable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal')
import Prelude.Linear hiding (All)
import Unsafe.Linear qualified as Unsafe

{- $setup

>>> import Control.Monad.Borrow.Pure.BO.Internal (BorrowKind(..))
-}

{- |
@'RecordLabel' r f a@ witnesses that the record type @r@ has a field named @f@ of type @a@.
Intended to be constructed with @OverloadedLabels@ extension, so that you can construct it by @#f@ syntax when the field @f@ of @a@ is imported in the current scope.

You can also expose 'RecordLabel' only, so that you can allow users to access such fields while internal implementation unexposed.
-}
type RecordLabel a f v = RecordLabel' a '(f, v)

-- | The actual definition of 'RecordLabel' for type-level hacks.
type RecordLabel' :: TYPE rep -> (Symbol, Type) -> Type
data RecordLabel' r fldVal where
  RecLab :: forall field r a. (HasField field r a) => RecordLabel' r '(field, a)

{- $record-splitting
== Overview

'(.#)' is handy when you need only one field of a borrowed record, but not applicable when you need to access more than one fields.
For that purpose, we provide '(.@)' operator for splitting a borrow of a record into FieldBorrows of its fields.
Consider the following:

>>> import Data.Ref.Linear (Ref)
>>> import Data.Vector.Mutable.Linear.Borrow (Vector)
>>> import Control.Monad.Borrow.Pure.BO
>>> data MyRecord = MyRecord { int :: Ref Int, strs :: Vector String, bool :: Ref Bool }

Suppose we have a mutable borrow of some @MyRecord@:

>>> :{
mutRec :: Mut α (MyRecord)
mutRec = undefined
:}

So, let's divide the mutable borrow into several pieces with '(.@)'.
First, we need to enable @OverloadedLabels@ extension to construct 'RecordLabel's:

>>> :set -XOverloadedLabels

First, we just want to divide into all the fields, in arbitrary order:

>>> (mutStrs, mutBool, mutInt) = mutRec .@ (#strs, #bool, #int)
>>> :t mutStrs
mutStrs :: Borrow 'Mut α (Vector String)

>>> :t mutBool
mutBool :: Borrow 'Mut α (Ref Bool)

>>> :t mutInt
mutInt :: Borrow 'Mut α (Ref Int)

Or, we can just divide into some of the fields (say, @bool@ and @strs@):

>>> (mutStrs, mutBool) = mutRec .@ (#strs, #bool)
>>> :t mutStrs
mutStrs :: Borrow 'Mut α (Vector String)

>>> :t mutBool
mutBool :: Borrow 'Mut α (Ref Bool)

Specifying the same field more than once results in a type error:

@
mutRec .@ (#strs, #bool, #strs)
-- error: Split record fields must be distinct, but got duplicate field: "strs"
@

In genral, '(.@)' accepts any /eliminator/ of a record borrow, which is typically one of the following:

    1. A tuple of 'RecordLabel's without duplcations (currently 2 to 5 components), or
    2. A heterogeneous list 'RecordLabels' of 'RecordLabel's, constructed with '(:#-)' and 'RNil', without duplcations on fields.

And fields not listed within the eliminator are not accessible after the split.
The examples so far uses tuples as eliminators, but you can also use 'RecordLabels'  as follows:

>>> mutStrs :#- mutBool :#- RNil = mutRec .@ #strs :#- #bool :#- RNil

'RecordLabels' will be mapped to 'FieldBorrows' after the split, and you can also use '(:#-)' and 'RNil' for pattern-matching.

Indeed, 'RecordLabel' itself is also a 'RecordEliminator', but if you are using `#f` syntax for constructing 'RecordLabel', you cannot use it with `.@` operator without type annotation because of the ambiguity.
If you just want to access one field, you can use '(.#)' operator.
-}

instance (KnownSymbol field) => Show (RecordLabel' r '(field, a)) where
  showsPrec d _ = showsPrec d $ symbolVal' @field proxy#

-- | This allows users to use @#f@ for constructing @'RecordLabel' a f v@.
instance
  (HasField field r a, fldVal ~ '(field, a)) =>
  IsLabel field (RecordLabel' r fldVal)
  where
  fromLabel = RecLab
  {-# INLINE fromLabel #-}

type Fst :: (k, v) -> k
type family Fst kv where
  Fst '(k, v) = k

type Snd :: (k, v) -> v
type family Snd kv where
  Snd '(k, v) = v

class
  ( lab ~ RecordLabel' a '(f, v)
  , RecordOf lab ~ a
  , SelectorOf lab ~ f
  , ValueOf lab ~ v
  ) =>
  IsRecordLabel' a lab f v
    | lab -> f v
  where
  type RecordOf lab :: Type
  type SelectorOf lab :: Symbol
  type ValueOf lab :: Type

instance IsRecordLabel' a (RecordLabel' a '(f, v)) f v where
  type RecordOf (RecordLabel' a '(f, v)) = a
  type SelectorOf (RecordLabel' a '(f, v)) = f
  type ValueOf (RecordLabel' a '(f, v)) = v

{- |
A class for *eliminators* of record, which can split a borrow of the whole record into FieldBorrows of its fields.
Typically, an eliminator is a tuple of 'RecordLabel's or heterogeneous 'RecordLabels'.
-}
class RecordEliminator elim a where
  type SplitBorrow elim (bk :: BorrowKind) (α :: Lifetime) a :: Type
  splitRecord :: elim %1 -> Borrow bk α a %1 -> SplitBorrow elim bk α a

{- |
Divides a borrow of a record into multiple FieldBorrows of its fields, according to the given @elim@inator.

Typically, @elim@ is one of the following:

* A tuple of 'RecordLabel's without duplcations (currently 2 to 5 components):

    @
    (.@) ::
      a %1 ->
      ('RecordLabel' a f1 v1, 'RecordLabel' a f2 v2, 'RecordLabel' a f3 v3) %1 ->
      ('Borrow' bk α v1, 'Borrow' bk α v2, 'Borrow' bk α v3)
    @

* A heterogeneous list of 'RecordLabel's without duplications:

    @
    (.@) ::
      a %1 ->
      'RecordLabels' '[ '(f1, v1), '(f2, v2), '(f3, v3), .. ] %1 ->
      'FieldBorrows' '[ '(f1, v1), '(f2, v2), '(f3, v3) ]
    @
-}
(.@) :: (RecordEliminator elim a) => Borrow bk α a %1 -> elim %1 -> SplitBorrow elim bk α a
(.@) = flip splitRecord
{-# INLINE (.@) #-}

{- |
@recBor '.#' #f@ divides a record borrow @recBor@ into a borrow of the field @f@.

This is '(.@)' specialised to 'RecordLabel' for better type inference.
To access multiple fields, you can use '(.@)' with a tuple of 'RecordLabel's or 'RecordLabels'.
See [Splitting a record borrow into pieces](#split) for more details.
-}
(.#) :: Borrow bk α a %1 -> RecordLabel a field val %1 -> Borrow bk α val
{-# INLINE (.#) #-}
(.#) = (.@)

infixl 4 .@

instance (a ~ r) => RecordEliminator (RecordLabel' r '(field, val)) a where
  type SplitBorrow (RecordLabel' r '(field, val)) bk α a = Borrow bk α val
  splitRecord RecLab = unsafeMapAlias (Unsafe.toLinear $ getField @field)
  {-# INLINE splitRecord #-}

type Distinct' :: Symbol -> Symbol -> Constraint
type family Distinct' l r :: Constraint where
  Distinct' l l = Unsatisfiable ('Text "Split record fields must be distinct, but got duplicate field: " ':<>: 'ShowType l)
  Distinct' l r = ()

class (Distinct' l r) => Distinct l r

instance (Distinct' l r) => Distinct l r

instance
  ( IsRecordLabel' a l f1 v1
  , IsRecordLabel' a r f2 v2
  , Distinct f1 f2
  ) =>
  RecordEliminator (l, r) a
  where
  type
    SplitBorrow (l, r) bk α a =
      (Borrow bk α (ValueOf l), Borrow bk α (ValueOf r))
  splitRecord (RecLab, RecLab) = Unsafe.toLinear \r ->
    ( unsafeMapAlias (Unsafe.toLinear (getField @f1)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f2)) r
    )

instance
  ( IsRecordLabel' a l1 f1 v1
  , IsRecordLabel' a l2 f2 v2
  , IsRecordLabel' a l3 f3 v3
  , Distinct f1 f2
  , Distinct f1 f3
  , Distinct f2 f3
  ) =>
  RecordEliminator (l1, l2, l3) a
  where
  type
    SplitBorrow (l1, l2, l3) bk α a =
      (Borrow bk α (ValueOf l1), Borrow bk α (ValueOf l2), Borrow bk α (ValueOf l3))
  splitRecord (RecLab, RecLab, RecLab) = Unsafe.toLinear \r ->
    ( unsafeMapAlias (Unsafe.toLinear (getField @f1)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f2)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f3)) r
    )

instance
  ( IsRecordLabel' a l1 f1 v1
  , IsRecordLabel' a l2 f2 v2
  , IsRecordLabel' a l3 f3 v3
  , IsRecordLabel' a l4 f4 v4
  , Distinct f1 f2
  , Distinct f1 f3
  , Distinct f1 f4
  , Distinct f2 f3
  , Distinct f2 f4
  , Distinct f3 f4
  ) =>
  RecordEliminator (l1, l2, l3, l4) a
  where
  type
    SplitBorrow (l1, l2, l3, l4) bk α a =
      (Borrow bk α (ValueOf l1), Borrow bk α (ValueOf l2), Borrow bk α (ValueOf l3), Borrow bk α (ValueOf l4))
  splitRecord (RecLab, RecLab, RecLab, RecLab) = Unsafe.toLinear \r ->
    ( unsafeMapAlias (Unsafe.toLinear (getField @f1)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f2)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f3)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f4)) r
    )

instance
  ( IsRecordLabel' a l1 f1 v1
  , IsRecordLabel' a l2 f2 v2
  , IsRecordLabel' a l3 f3 v3
  , IsRecordLabel' a l4 f4 v4
  , IsRecordLabel' a l5 f5 v5
  , Distinct f1 f2
  , Distinct f1 f3
  , Distinct f1 f4
  , Distinct f1 f5
  , Distinct f2 f3
  , Distinct f2 f4
  , Distinct f2 f5
  , Distinct f3 f4
  , Distinct f3 f5
  , Distinct f4 f5
  ) =>
  RecordEliminator (l1, l2, l3, l4, l5) a
  where
  type
    SplitBorrow (l1, l2, l3, l4, l5) bk α a =
      ( Borrow bk α (ValueOf l1)
      , Borrow bk α (ValueOf l2)
      , Borrow bk α (ValueOf l3)
      , Borrow bk α (ValueOf l4)
      , Borrow bk α (ValueOf l5)
      )
  splitRecord (RecLab, RecLab, RecLab, RecLab, RecLab) = Unsafe.toLinear \r ->
    ( unsafeMapAlias (Unsafe.toLinear (getField @f1)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f2)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f3)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f4)) r
    , unsafeMapAlias (Unsafe.toLinear (getField @f5)) r
    )

type data Fun
  = BorrowOf BorrowKind Lifetime
  | RecordLabelOf Type

type Apply :: Fun -> (Symbol, Type) -> Type
type family Apply fun a where
  Apply (BorrowOf bk α) kv = Borrow bk α (Snd kv)
  Apply (RecordLabelOf r) kv = RecordLabel' r '(Fst kv, Snd kv)

type LabelsOrBorrows :: Fun -> [(Symbol, Type)] -> Type
data LabelsOrBorrows h xs where
  RNil :: LabelsOrBorrows h '[]
  (:#-) :: Apply h '(k, v) %1 -> LabelsOrBorrows h xs %1 -> LabelsOrBorrows h ('(k, v) ': xs)

{- |
Heterogeneous record labels. If the record type @a@ is clear from the context, you can construct it with '(':#-')' and 'RNil' with @OverloadedLabels@ extension:

@
  data MyRecord = MyRecord { foo :: Int, bar :: String, buz :: Bool }
  myLabels :: 'RecordLabels' MyRecord _
  myLabels = #foo ':#-' #bar ':#-' #buz ':#-' 'RNil'
@
-}
type RecordLabels a fs = LabelsOrBorrows (RecordLabelOf a) fs

{- |
Heterogeneous FieldBorrows. If the record type @a@ is clear from the context, you can construct it with '(':#-')' and 'RNil' with @OverloadedLabels@ extension:

@
data MyRecord = MyRecord { foo :: Int, bar :: String, buz :: Bool }

mutRec :: t'Control.Monad.Pure.Mut' α MyRecord
mutRec = ...

buzMut ':#-' fooMut ':#-' 'RNil' = mutRec '.@' #buz ':#-' #foo ':#-' 'RNil'
@
-}
type FieldBorrows bk α fs = LabelsOrBorrows (BorrowOf bk α) fs

instance Affine (LabelsOrBorrows h xs) where
  aff = unsafeAff
  {-# INLINE aff #-}

deriving via
  AsAffine (LabelsOrBorrows h xs)
  instance
    Consumable (LabelsOrBorrows h xs)

infixr 5 :#-

instance
  (IsUnique fvs, label ~ RecordLabelOf a) =>
  RecordEliminator (LabelsOrBorrows label fvs) a
  where
  type SplitBorrow (LabelsOrBorrows label fvs) bk α a = LabelsOrBorrows (BorrowOf bk α) fvs
  splitRecord = Unsafe.toLinear \case
    RNil -> (`lseq` RNil)
    lab :#- xs -> Unsafe.toLinear \r ->
      splitRecord lab r :#- splitRecord xs r
  {-# INLINE splitRecord #-}

type family All_ c xs :: Constraint where
  All_ c '[] = ()
  All_ c (x ': xs) = (c x, All c xs)

class (All_ c xs) => All c xs

instance All c '[]

instance (c x, All c xs) => All c (x ': xs)

type IsUnique :: [(Symbol, Type)] -> Constraint

type family IsUnique_ xs :: Constraint where
  IsUnique_ '[] = ()
  IsUnique_ (kv ': xs) = (All (Distinct (Fst kv)) (MapFst xs), IsUnique xs)

class (IsUnique_ xs) => IsUnique xs

instance IsUnique '[]

instance (All (Distinct (Fst kv)) (MapFst xs), IsUnique xs) => IsUnique (kv ': xs)

type family MapFst xs where
  MapFst '[] = '[]
  MapFst ('(k, v) ': xs) = k ': MapFst xs
