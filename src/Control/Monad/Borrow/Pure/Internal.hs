{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Monad.Borrow.Pure.Internal (
  module Control.Monad.Borrow.Pure.Internal,
) where

import Control.Exception qualified as SystemIO
import Control.Functor.Linear qualified as Control
import Control.Monad qualified as NonLinear
import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Control.Monad.ST.Strict (ST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Array.Mutable.Linear (Array)
import Data.Coerce (Coercible)
import Data.Coerce qualified
import Data.Coerce.Directed
import Data.Functor.Identity (Identity)
import Data.Functor.Linear qualified as Data
import Data.Int
import Data.Kind (Constraint, Type)
import Data.Monoid qualified as Mon
import Data.Ord qualified as Ord
import Data.Ref.Linear (Ref)
import Data.Semigroup qualified as Sem
import Data.Tuple (Solo (..))
import Data.Type.Coercion (Coercion (..))
import Data.Vector.Mutable.Linear (Vector)
import Data.Word
import GHC.Base (TYPE)
import GHC.Base qualified as GHC
import GHC.Exts (State#, realWorld#)
import GHC.ST qualified as ST
import GHC.TypeError (ErrorMessage (..))
import Generics.Linear
import Numeric.Natural (Natural)
import Prelude.Linear
import Prelude.Linear qualified as PL
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import System.IO.Linear qualified as L
import Unsafe.Linear qualified as Unsafe

-- NOTE: We want to use `TypeData` extension for 'ForBO', but it makes Haddock panic!

type ForBO :: Lifetime -> Type
data ForBO α

-- Morally an ST Monad, but linear!
newtype BO α a = BO (State# (ForBO α) %1 -> (# State# (ForBO α), a #))

instance Data.Functor (BO α) where
  fmap f (BO g) = BO \s -> case g s of
    (# s', a #) -> (# s', f a #)
  {-# INLINE fmap #-}

instance Control.Functor (BO α) where
  fmap f (BO g) = BO \s -> case g s of
    (# s', a #) -> (# s', f a #)
  {-# INLINE fmap #-}

instance Data.Applicative (BO α) where
  pure a = Control.pure a
  {-# INLINE pure #-}

  (<*>) = \f g -> f Control.<*> g
  {-# INLINE (<*>) #-}

instance Control.Applicative (BO α) where
  pure a = BO \s -> (# s, a #)
  {-# INLINE pure #-}

  BO f <*> BO g = BO \s -> case f s of
    (# s', h #) -> case g s' of
      (# s'', a #) -> (# s'', h a #)
  {-# INLINE (<*>) #-}

instance Control.Monad (BO α) where
  BO fa >>= f = BO \s -> case fa s of
    (# s', a #) -> (f a) PL.& \(BO g) -> g s'
  {-# INLINE (>>=) #-}

unsafeBOToLinIO :: BO α a %1 -> L.IO a
{-# INLINE unsafeBOToLinIO #-}
unsafeBOToLinIO (BO f) = L.IO (Unsafe.coerce f)

runBO# :: forall {rep} α (o :: TYPE rep). (State# (ForBO α) %1 -> o) %1 -> o
{-# NOINLINE runBO# #-}
runBO# f = Unsafe.coerce f realWorld#

execBO :: BO α a %1 -> Now α %1 -> (Now α, a)
{-# INLINE execBO #-}
execBO (BO f) !now =
  case runBO# f of
    (# s, !a #) -> dropState# s `PL.lseq` (now, a)

dropState# :: State# a %1 -> ()
{-# INLINE dropState# #-}
dropState# = Unsafe.toLinear \_ -> ()

-- | See also 'scope'.
sexecBO :: BO (α /\ β) a %1 -> Now α %1 -> BO β (Now α, a)
{-# INLINE sexecBO #-}
sexecBO f now = unsafeCastBO ((now,) PL.. Unsafe.toLinear (\ !a -> a) Control.<$> f)

unsafeCastBO :: BO α a %1 -> BO β a
{-# INLINE unsafeCastBO #-}
unsafeCastBO = Unsafe.coerce

unsafeSTToBO :: ST s a %1 -> BO α a
{-# INLINE unsafeSTToBO #-}
unsafeSTToBO (ST.ST f) = BO (Unsafe.coerce f)

unsafeBOToST :: BO α a %1 -> ST s a
{-# INLINE unsafeBOToST #-}
unsafeBOToST (BO f) = ST.ST (Unsafe.coerce f)

unsafeIOToBO :: L.IO a %1 -> BO α a
{-# INLINE unsafeIOToBO #-}
unsafeIOToBO (L.IO f) = BO (Unsafe.coerce f)

unsafeSystemIOToBO :: IO a %1 -> BO α a
{-# INLINE unsafeSystemIOToBO #-}
unsafeSystemIOToBO (GHC.IO a) = BO (Unsafe.coerce a)

unsafeBOToSystemIO :: BO α a %1 -> IO a
{-# INLINE unsafeBOToSystemIO #-}
unsafeBOToSystemIO (BO f) = GHC.IO (Unsafe.coerce f)

unsafePerformEvaluateUndupableBO :: BO α a %1 -> a
unsafePerformEvaluateUndupableBO (BO f) = runBO# \s ->
  case Unsafe.toLinear GHC.noDuplicate# s of
    s -> case f s of
      (# s, !a #) -> dropState# s `PL.lseq` a

-- | Run two computations in parallel, returning their results as a tuple.
parBO :: BO α a %1 -> BO α b %1 -> BO α (a, b)
{-# NOINLINE parBO #-}
parBO a b = GHC.noinline
  -- TODO: define explicit rules to when to invoke noDuplicate#
  BO
  \s -> case Unsafe.toLinear GHC.noDuplicate# s of
    s -> case Unsafe.toLinear2 GHC.spark# (unsafePerformEvaluateUndupableBO a) s of
      (# s, a #) -> case Unsafe.toLinear2 GHC.spark# (unsafePerformEvaluateUndupableBO b) s of
        (# s, b #) -> case Unsafe.toLinear2 GHC.seq# a s of
          (# s, !a #) -> case Unsafe.toLinear2 GHC.seq# b s of
            (# s, !b #) -> (# s, (a, b) #)

evaluate :: a %1 -> BO α a
{-# INLINE evaluate #-}
evaluate a = unsafeSystemIOToBO (Unsafe.toLinear SystemIO.evaluate a)

-- | Mutable borrow to some resource 'a'
type Mut :: Lifetime -> Type -> Type
newtype Mut α a = UnsafeMut a

instance LinearOnly (Mut α a) where
  unsafeWithLinear = unsafeLinearOnly

type role Mut nominal nominal

instance Affine (Mut α a) where
  aff = UnsafeAff
  {-# INLINE aff #-}

deriving via AsAffine (Mut α a) instance Consumable (Mut α a)

instance (β <= α, a <: b, b <: a) => Mut α a <: Mut β b where
  upcast (UnsafeMut a) = UnsafeMut (upcast a)
  {-# INLINE upcast #-}

-- | Immutable shared borrow to some resource 'a'
type Share :: Lifetime -> Type -> Type
newtype Share α a = UnsafeShare a

type role Share nominal representational

instance Affine (Share α a) where
  aff = UnsafeAff
  {-# INLINE aff #-}

deriving via AsAffine (Share α a) instance Consumable (Share α a)

unsafeWrapAlias :: (Alias_ alias) => a %1 -> alias a
{-# INLINE unsafeWrapAlias #-}
unsafeWrapAlias = coerceLin

unsafeUnwrapAlias :: (Alias_ alias) => alias a %1 -> a
{-# INLINE unsafeUnwrapAlias #-}
unsafeUnwrapAlias = coerceLin

instance Dupable (Share α a) where
  dup2 = Unsafe.toLinear $ NonLinear.join (,)
  {-# INLINE dup2 #-}

instance Movable (Share α a) where
  move = Unsafe.toLinear Ur
  {-# INLINE move #-}

instance (β <= α, a <: b) => Share α a <: Share β b where
  upcast (UnsafeShare a) = UnsafeShare (upcast a)
  {-# INLINE upcast #-}

{- | A (mutable) lent resource to 'a', which
will only be available at the 'End' of the lifetime 'α'.
-}
type Lend :: Lifetime -> Type -> Type
newtype Lend α a = UnsafeLend a

type role Lend nominal nominal

instance (α <= β, a <: b) => Lend α a <: Lend β b where
  upcast (UnsafeLend a) = UnsafeLend (upcast a)
  {-# INLINE upcast #-}

-- | Borrow a resource linearly and obtain the mutable borrow to it and 'Lend' witness to 'reclaim' the resource to lend at the 'End' of the lifetime.
borrow :: a %1 -> Linearly %1 -> (Mut α a, Lend α a)
borrow = Unsafe.toLinear \a lin ->
  lin `lseq` (UnsafeMut a, UnsafeLend a)

-- | Analogous to 'borrow', but does not return the original 'Lend' to be reclaimed
borrow_ :: a %1 -> Linearly %1 -> Mut α a
{-# INLINE borrow_ #-}
borrow_ = Unsafe.toLinear \(a :: a) lin ->
  lin `lseq` UnsafeMut a

-- | Shares a mutable borrow, invalidating the original one.
share :: Mut α a %1 -> Ur (Share α a)
{-# INLINE share #-}
share = Unsafe.toLinear \(UnsafeMut a) -> Ur (UnsafeShare a)

-- | Reclaims a 'borrow'ed resource at the 'End' of lifetime @α'.
reclaim :: End α %1 -> Lend α a %1 -> a
reclaim end = end `lseq` \(UnsafeLend !a) -> a

-- | Reborrow a mutable borrow into a sublifetime
reborrow :: (β <= α) => Mut α a %1 -> (Mut β a, Lend β (Mut α a))
reborrow = Unsafe.toLinear \mutA ->
  (Data.Coerce.coerce mutA, Data.Coerce.coerce mutA)

-- | Collapse a nested mutable borrow
joinMut :: Mut α (Mut β a) %1 -> Mut (α /\ β) a
joinMut = coerceLin

-- | Collapse a shared borrow over a mutable borrow
joinShareMut :: Share α (Mut β a) %1 -> Share (α /\ β) a
joinShareMut = coerceLin

type Alias_ :: (Type -> Type) -> Constraint
class
  (forall x. Coercible x (alias x)) =>
  Alias_ alias
  where
  type AliasLifetime alias :: Lifetime
  coercionWit :: Coercion x (alias x)

instance Alias_ (Mut α) where
  type AliasLifetime (Mut α) = α
  coercionWit = Coercion

instance Alias_ (Share α) where
  type AliasLifetime (Share α) = α
  coercionWit = Coercion

instance Alias_ (Lend α) where
  type AliasLifetime (Lend α) = α
  coercionWit = Coercion

-- | An abstraction over a type that can be
class (Alias_ alias) => Alias alias

instance (Alias_ alias) => Alias alias

type AliasAt α alias = (Alias alias, AliasLifetime alias ~ α)

data CaseBorrow alias where
  IsMut :: CaseBorrow (Mut α)
  IsShare :: CaseBorrow (Share α)

-- | A constraint that requires @alias@ to be either a 'Share' or a 'Mut' borrow, which is accessible in 'BO' regions.
class (Alias alias) => Borrow alias where
  caseBorrow :: CaseBorrow alias

instance Borrow (Mut α) where
  caseBorrow = IsMut

instance Borrow (Share α) where
  caseBorrow = IsShare

type BorrowAt α alias =
  ( Borrow alias
  , AliasLifetime alias ~ α
  )

splitList :: (Alias f) => f [x] %1 -> [f x]
splitList = split

splitPair :: (Alias alias) => alias (a, b) %1 -> (alias a, alias b)
{-# INLINE splitPair #-}
splitPair = coerceLin . unsafeUnwrapAlias

splitEither :: (Alias alias) => alias (Either a b) %1 -> Either (alias a) (alias b)
{-# INLINE splitEither #-}
splitEither = coerceLin . unsafeUnwrapAlias

-- | A dual to 'Alias', which allows us to distribute a borrow over a functor.
class DistributesAlias f where
  split_ :: (Alias alias) => alias (f x) %1 -> f (alias x)
  default split_ ::
    (GenericDistributesAlias f, Alias alias) =>
    alias (f x) %1 -> f (alias x)
  split_ = genericSplit

split ::
  forall alias f x.
  ( DistributesAlias f
  , Alias alias
  ) =>
  alias (f x) %1 -> f (alias x)
{-# INLINE [1] split #-}
split = split_

deriving anyclass instance DistributesAlias Identity

deriving anyclass instance DistributesAlias []

deriving anyclass instance DistributesAlias Maybe

deriving anyclass instance DistributesAlias Solo

deriving anyclass instance DistributesAlias Ord.Down

deriving anyclass instance DistributesAlias Sem.Dual

deriving anyclass instance DistributesAlias Sem.Max

deriving anyclass instance DistributesAlias Sem.Min

deriving anyclass instance DistributesAlias Sem.First

deriving anyclass instance DistributesAlias Sem.Last

deriving anyclass instance DistributesAlias Mon.First

deriving anyclass instance DistributesAlias Mon.Last

instance (Unsatisfiable ('Text "Use splitEither directly!")) => DistributesAlias (Either e) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

instance (Unsatisfiable ('Text "Use splitPair instead!")) => DistributesAlias ((,) a) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

type GenericDistributesAlias f = (Generic1 f, GDistributeAlias (Rep1 f))

genericSplit ::
  forall alias f x.
  ( GenericDistributesAlias f
  , Alias alias
  ) =>
  alias (f x) %1 -> f (alias x)
{-# INLINE genericSplit #-}
genericSplit =
  to1
    . gdistributeAlias @(Rep1 f) @alias
    . unsafeMapAlias from1

unsafeMapAlias :: (Alias alias) => (a %1 -> b) -> alias a %1 -> alias b
{-# INLINE unsafeMapAlias #-}
unsafeMapAlias f = coerceLin f

instance (GenericDistributesAlias f) => DistributesAlias (Generically1 f) where
  {-# INLINE split_ #-}
  split_ = Generically1 . genericSplit . unsafeMapAlias \(Generically1 f) -> f

class GDistributeAlias f where
  gdistributeAlias :: (Alias alias) => alias (f x) %1 -> f (alias x)

instance
  ( GDistributeAlias f
  , GDistributeAlias g
  ) =>
  GDistributeAlias (f :*: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (alias :: alias a) =
    case unsafeUnwrapAlias alias of
      f :*: g -> DataFlow.do
        f <- gdistributeAlias $ unsafeWrapAlias f
        g <- gdistributeAlias $ unsafeWrapAlias g
        f :*: g

instance
  ( GDistributeAlias f
  , GDistributeAlias g
  ) =>
  GDistributeAlias (f :+: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias alias = case unsafeUnwrapAlias alias of
    L1 l -> L1 (gdistributeAlias (unsafeWrapAlias l))
    R1 r -> R1 (gdistributeAlias (unsafeWrapAlias r))

instance
  (Unsatisfiable (Text "Nonlinear fields cannot distribute borrows!")) =>
  GDistributeAlias (MP1 GHC.Many f)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = unsatisfiable

instance (GDistributeAlias f) => GDistributeAlias (MP1 GHC.One f) where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias =
    MP1 . gdistributeAlias . unsafeWrapAlias . unMP1 . unsafeUnwrapAlias

instance (GDistributeAlias f) => GDistributeAlias (M1 i c f) where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = \x ->
    case unsafeUnwrapAlias x of
      M1 x -> M1 $ gdistributeAlias $ unsafeWrapAlias x

instance DistributesAlias Par1 where
  {-# INLINE split_ #-}
  split_ = \x -> case unsafeUnwrapAlias x of
    Par1 a -> Par1 (unsafeWrapAlias a)

instance
  ( DistributesAlias f
  , DistributesAlias g
  , Data.Functor f
  ) =>
  GDistributeAlias (f :.: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = \(x :: alias _) -> case unsafeUnwrapAlias x of
    Comp1 fg -> Comp1 $ Data.fmap split_ $ split_ $ unsafeWrapAlias @alias fg

instance GDistributeAlias Par1 where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = \x -> case unsafeUnwrapAlias x of
    Par1 a -> Par1 (unsafeWrapAlias a)

instance
  (Unsatisfiable (Text "A type containing non-parametric field with type `" :<>: ShowType c :<>: Text "', which cannot be safely splitted!")) =>
  GDistributeAlias (K1 i c)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = unsatisfiable

instance GDistributeAlias U1 where
  gdistributeAlias = coerceLin . unsafeUnwrapAlias
  {-# INLINE gdistributeAlias #-}

class Copyable a where
  unsafeCopy :: Share α a %1 -> a

instance
  (Unsatisfiable (ShowType (Ref a) :<>: Text " cannot be copied!")) =>
  Copyable (Ref a)
  where
  unsafeCopy = unsatisfiable

instance
  (Unsatisfiable (ShowType (Array a) :<>: Text " cannot be copied!")) =>
  Copyable (Array a)
  where
  unsafeCopy = unsatisfiable

instance
  (Unsatisfiable (ShowType (Vector a) :<>: Text " cannot be copied!")) =>
  Copyable (Vector a)
  where
  unsafeCopy = unsatisfiable

copy :: (Copyable a) => Share α a %1 -> a
{-# INLINE [1] copy #-}
copy = unsafeCopy

copyMut :: (Copyable a) => Mut α a %1 -> a
copyMut mut = let !(Ur shr) = share mut in copy shr

{-# RULES
"copy/unsafeCoerce" [~1]
  copy =
    Unsafe.coerce
  #-}

newtype UnsafeAssumeNoVar a = UnsafeAssumeNoVar a

instance Copyable (UnsafeAssumeNoVar a) where
  unsafeCopy = coerceLin
  {-# INLINE unsafeCopy #-}

deriving via UnsafeAssumeNoVar Int instance Copyable Int

deriving via UnsafeAssumeNoVar Int8 instance Copyable Int8

deriving via UnsafeAssumeNoVar Int16 instance Copyable Int16

deriving via UnsafeAssumeNoVar Int32 instance Copyable Int32

deriving via UnsafeAssumeNoVar Int64 instance Copyable Int64

deriving via UnsafeAssumeNoVar Word instance Copyable Word

deriving via UnsafeAssumeNoVar Word8 instance Copyable Word8

deriving via UnsafeAssumeNoVar Word16 instance Copyable Word16

deriving via UnsafeAssumeNoVar Word32 instance Copyable Word32

deriving via UnsafeAssumeNoVar Word64 instance Copyable Word64

deriving via UnsafeAssumeNoVar Integer instance Copyable Integer

deriving via UnsafeAssumeNoVar Natural instance Copyable Natural

deriving via UnsafeAssumeNoVar Float instance Copyable Float

deriving via UnsafeAssumeNoVar Double instance Copyable Double

deriving via UnsafeAssumeNoVar Char instance Copyable Char

deriving via UnsafeAssumeNoVar Bool instance Copyable Bool

type GenericCopyable a = (Generic a, GCopyable (Rep a))

genericCopyShare :: (GenericCopyable a) => Share α a %1 -> a
{-# INLINE genericCopyShare #-}
genericCopyShare (UnsafeShare x) = to (gcopy (UnsafeShare (from x)))

type GCopyable :: forall {k}. (k -> Type) -> Constraint
class GCopyable f where
  gcopy :: Share α (f x) %1 -> f x

instance (Copyable a) => GCopyable (K1 i a) where
  gcopy = coerceLin . unsafeUnwrapAlias

instance (GCopyable f, GCopyable g) => GCopyable (f :*: g) where
  gcopy (UnsafeShare (f :*: g)) =
    gcopy (UnsafeShare f) :*: gcopy (UnsafeShare g)

instance (GCopyable f) => GCopyable (M1 i c f) where
  gcopy = \case
    UnsafeShare (M1 x) -> M1 (gcopy (UnsafeShare x))

instance (GCopyable f) => GCopyable (MP1 m f) where
  gcopy = \case
    UnsafeShare (MP1 x) -> MP1 (gcopy (UnsafeShare x))

instance (GCopyable f, GCopyable g) => GCopyable (f :+: g) where
  gcopy = \case
    UnsafeShare (L1 x) -> L1 (gcopy (UnsafeShare x))
    UnsafeShare (R1 x) -> R1 (gcopy (UnsafeShare x))

instance GCopyable U1 where
  gcopy = coerceLin . unsafeUnwrapAlias

instance GCopyable V1 where
  gcopy = \case {} . unsafeUnwrapAlias

instance (GenericCopyable a) => Copyable (Generically a) where
  unsafeCopy = Generically . genericCopyShare . unsafeMapAlias (\(Generically x) -> x)

deriving via
  Generically (Sum a)
  instance
    (Copyable a) => Copyable (Sum a)

deriving via
  Generically (Product a)
  instance
    (Copyable a) => Copyable (Product a)

deriving via
  Generically (Sem.Max a)
  instance
    (Copyable a) => Copyable (Sem.Max a)

deriving via
  Generically (Maybe a)
  instance
    (Copyable a) => Copyable (Maybe a)

deriving via
  Generically (Sem.Min a)
  instance
    (Copyable a) => Copyable (Sem.Min a)

deriving via
  Generically (a, b)
  instance
    (Copyable a, Copyable b) =>
    Copyable (a, b)

deriving via
  Generically (a, b, c)
  instance
    (Copyable a, Copyable b, Copyable c) =>
    Copyable (a, b, c)

deriving via
  Generically (a, b, c, d)
  instance
    (Copyable a, Copyable b, Copyable c, Copyable d) =>
    Copyable (a, b, c, d)

deriving via
  Generically (Either a b)
  instance
    (Copyable a, Copyable b) => Copyable (Either a b)
