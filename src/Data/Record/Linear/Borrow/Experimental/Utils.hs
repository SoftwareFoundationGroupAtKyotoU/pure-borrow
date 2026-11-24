{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Record.Linear.Borrow.Experimental.Utils (
  HasDeclaredField (..),
) where

import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (~))
import Data.Void (Void)
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)
import GHC.TypeLits (Symbol)
import Generics.Linear qualified as GL
import Prelude.Linear (Maybe (..))

{-
Note [Record labels project through Generic]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both "Data.Record.Linear.Borrow.Experimental.Split" and "Data.Record.Linear.Borrow.Experimental.PatternMatch" once built each field borrow by calling 'GHC.Records.getField' under a 'GHC.Records.HasField' constraint.
That accepted any user-written 'HasField' instance, including virtual fields that alias another field or pair two references to the same field, and it also accepted a @newtype N = N R@ with @deriving newtype instance Generic N@ and hand-written virtual 'HasField' instances on @N@, because @N@ declares no selectors of its own.

Instead, 'HasDeclaredField' checks linear-generics' 'GL.Generic' representation directly and projects the field through 'GL.from' along the path 'FindField' computes in the single constructor's product tree:

\* Distinct field names in the same constructor select distinct leaves of that product tree, and a lawful 'GL.Generic' instance is an isomorphism, so two distinct labels always denote disjoint parts of the record regardless of any 'HasField' instance in scope.
  A hand-written 'GL.Generic' instance with duplicate @'GL.MetaSel'@ names in one constructor is a user-side contract violation, since GHC forbids duplicate field names in a single constructor.
\* Projecting through 'GL.from' also closes the @newtype N = N R@ route, because @deriving newtype instance Generic N@ gives @N@ the representation and 'GL.from' of @R@, so @#x@ and @#y@ project the corresponding fields of @R@ rather than any virtual 'HasField' on @N@.
\* The check runs only when constructing a 'RecordLabel' or 'RecordLabel'' via 'GHC.OverloadedLabels.IsLabel', which stores the resulting projection function @r -> a@ inside the label rather than carrying the dictionary witness.
  Both label types keep @RecLab@ unexported and carry @nominal@ role annotations, so callers cannot forge or coerce a label to another record, field name, or type, and large lists of labels do not inflate module interface files.
  The projection class methods bind their record or representation argument in a lambda on the right-hand side (@sat-args=0@), so GHC inlines them into @RecLab@ without waiting for a record argument and without emitting @SPECIALISE@ rules into @.hi@ files.

Dispatch goes through 'HasDeclaredFieldRep' with an @INCOHERENT@ fallback when @GL.Rep r@ is stuck on a type without a 'GL.Generic' instance or a polymorphic type variable.
That @INCOHERENT@ instance only ever discharges 'Unsatisfiable', so it cannot make an ill-typed program compile or forge an instance for a concrete type whose 'GL.Rep' already reduces to 'GL.D1'.
-}

data FieldPath
  = Here
  | GoL FieldPath
  | GoR FieldPath

data FoundField
  = NotFound
  | Found FieldPath Type

type CombineFind :: FoundField -> FoundField -> FoundField
type family CombineFind l r where
  CombineFind ('Found p c) _ = 'Found ('GoL p) c
  CombineFind 'NotFound ('Found p c) = 'Found ('GoR p) c
  CombineFind 'NotFound 'NotFound = 'NotFound

type FindField :: Symbol -> (Type -> Type) -> FoundField
type family FindField field f where
  FindField field (GL.S1 ('GL.MetaSel ('Just field) unp str dstr) (GL.K1 i c)) = 'Found 'Here c
  FindField field (GL.S1 ('GL.MetaSel ('Just field) unp str dstr) (GL.MP1 m (GL.K1 i c))) = 'Found 'Here c
  FindField field (l GL.:*: r) = CombineFind (FindField field l) (FindField field r)
  FindField _ _ = 'NotFound

type AppendSymbols :: [Symbol] -> [Symbol] -> [Symbol]
type family AppendSymbols xs ys where
  AppendSymbols '[] ys = ys
  AppendSymbols (x ': xs) ys = x ': AppendSymbols xs ys

type DeclaredFieldNames :: (Type -> Type) -> [Symbol]
type family DeclaredFieldNames f where
  DeclaredFieldNames (GL.S1 ('GL.MetaSel ('Just name) unp str dstr) f) = '[name]
  DeclaredFieldNames (l GL.:*: r) = AppendSymbols (DeclaredFieldNames l) (DeclaredFieldNames r)
  DeclaredFieldNames _ = '[]

type GProjectPath :: FieldPath -> (Type -> Type) -> Type -> Constraint
class GProjectPath path f a where
  gprojectPath :: f p -> a

instance (GProjectPath path l a) => GProjectPath ('GoL path) (l GL.:*: r) a where
  {-# INLINE gprojectPath #-}
  gprojectPath = \(l GL.:*: _) -> gprojectPath @path l

instance (GProjectPath path r a) => GProjectPath ('GoR path) (l GL.:*: r) a where
  {-# INLINE gprojectPath #-}
  gprojectPath = \(_ GL.:*: r) -> gprojectPath @path r

instance GProjectPath 'Here (GL.S1 i (GL.K1 j a)) a where
  {-# INLINE gprojectPath #-}
  gprojectPath = \(GL.M1 (GL.K1 x)) -> x

instance GProjectPath 'Here (GL.S1 i (GL.MP1 m (GL.K1 j a))) a where
  {-# INLINE gprojectPath #-}
  gprojectPath = \(GL.M1 (GL.MP1 (GL.K1 x))) -> x

type ProjectFound :: Symbol -> Type -> (Type -> Type) -> FoundField -> Type -> Constraint
class ProjectFound field r body found a where
  projectFound :: body p -> a

instance
  (a ~ c, GProjectPath path body c) =>
  ProjectFound field r body ('Found path c) a
  where
  {-# INLINE projectFound #-}
  projectFound = \x -> gprojectPath @path @body @c x

instance
  ( Unsatisfiable
      ( 'ShowType r
          ':<>: 'Text " declares no field "
          ':<>: 'ShowType field
          ':<>: 'Text "."
          ':$$: 'Text "The fields that "
          ':<>: 'ShowType r
          ':<>: 'Text " declares are "
          ':<>: 'ShowType (DeclaredFieldNames body)
          ':<>: 'Text "."
          ':$$: 'Text "A HasField instance written by hand does not declare a field."
      )
  ) =>
  ProjectFound field r body 'NotFound a
  where
  {-# INLINE projectFound #-}
  projectFound = \_ -> unsatisfiable

type ProjectCtor :: Symbol -> Type -> (Type -> Type) -> Type -> Constraint
class ProjectCtor field r f a where
  projectCtor :: f p -> a

instance
  (Unsatisfiable ('Text "A borrow of a type with more than one constructor cannot be split by a field.")) =>
  ProjectCtor field r (f GL.:+: g) a
  where
  {-# INLINE projectCtor #-}
  projectCtor = \_ -> unsatisfiable

instance
  (ProjectFound field r body (FindField field body) a) =>
  ProjectCtor field r (GL.C1 i body) a
  where
  {-# INLINE projectCtor #-}
  projectCtor = \(GL.M1 x) -> projectFound @field @r @body @(FindField field body) @a x

instance
  ( Unsatisfiable
      ( 'ShowType r
          ':<>: 'Text " declares no field "
          ':<>: 'ShowType field
          ':<>: 'Text "."
          ':$$: 'Text "The fields that "
          ':<>: 'ShowType r
          ':<>: 'Text " declares are "
          ':<>: 'ShowType ('[] :: [Symbol])
          ':<>: 'Text "."
          ':$$: 'Text "A HasField instance written by hand does not declare a field."
      )
  ) =>
  ProjectCtor field r GL.V1 a
  where
  {-# INLINE projectCtor #-}
  projectCtor = \_ -> unsatisfiable

type HasDeclaredFieldRep :: Symbol -> Type -> (Type -> Type) -> Type -> Constraint
class HasDeclaredFieldRep field r rep a where
  projectDeclaredFieldRep :: r -> a

instance
  (GL.Generic r, GL.Rep r ~ GL.D1 i f, ProjectCtor field r f a) =>
  HasDeclaredFieldRep field r (GL.D1 i f) a
  where
  {-# INLINE projectDeclaredFieldRep #-}
  projectDeclaredFieldRep = \r -> projectCtor @field @r @f @a (GL.unM1 (GL.from @r @Void r))

instance
  {-# INCOHERENT #-}
  ( Unsatisfiable
      ( 'Text "The fields of "
          ':<>: 'ShowType r
          ':<>: 'Text " are unknown, since it has no instance of linear-generics' Generic."
          ':$$: 'Text "Derive one with deriveGeneric from Generics.Linear.TH, or, in a function polymorphic in the record, take the label as an argument."
      )
  ) =>
  HasDeclaredFieldRep field r rep a
  where
  {-# INLINE projectDeclaredFieldRep #-}
  projectDeclaredFieldRep = \_ -> unsatisfiable

-- | Witness that @r@ is a single-constructor type with a 'GL.Generic' instance declaring a field @field@ of type @a@.
type HasDeclaredField :: Symbol -> Type -> Type -> Constraint
class HasDeclaredField field r a where
  projectDeclaredField :: r -> a

instance
  (HasDeclaredFieldRep field r (GL.Rep r) a) =>
  HasDeclaredField field r a
  where
  {-# INLINE projectDeclaredField #-}
  projectDeclaredField = \r -> projectDeclaredFieldRep @field @r @(GL.Rep r) @a r
