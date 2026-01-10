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
import Data.Type.Equality ((:~:) (Refl))
import Data.Vector.Mutable.Linear (Vector)
import Data.Word
import GHC.Base (TYPE)
import GHC.Base qualified as GHC
import GHC.Exts (State#, runRW#)
import GHC.ST qualified as ST
import GHC.TypeError (ErrorMessage (..))
import Generics.Linear
import Numeric.Natural (Natural)
import Prelude.Linear
import Prelude.Linear qualified as PL
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import System.IO.Linear qualified as L
import Unsafe.Coerce (unsafeCoerce#)
import Unsafe.Linear qualified as Unsafe

askLinearly :: BO α Linearly
{-# NOINLINE askLinearly #-}
askLinearly = GHC.noinline $ Control.pure UnsafeLinearly

-- NOTE: We want to use `TypeData` extension for 'ForBO', but it makes Haddock panic!

type ForBO :: Lifetime -> Type
data ForBO α

-- Morally an ST Monad, but linear!
newtype BO α a = BO (State# (ForBO α) %1 -> (# State# (ForBO α), a #))

assocRBO :: BO ((α /\ β) /\ γ) a %1 -> BO (α /\ (β /\ γ)) a
{-# INLINE assocRBO #-}
assocRBO = unsafeCastBO

assocLBO :: BO (α /\ (β /\ γ)) a %1 -> BO ((α /\ β) /\ γ) a
{-# INLINE assocLBO #-}
assocLBO = unsafeCastBO

assocBOEq :: forall α β γ a. BO ((α /\ β) /\ γ) a :~: BO (α /\ (β /\ γ)) a
{-# INLINE assocBOEq #-}
assocBOEq = Unsafe.coerce $ Refl @(BO (α /\ β /\ γ) a)

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
{-# INLINE runBO# #-}
runBO# = Unsafe.toLinear \f -> runRW# \s ->
  f (unsafeCoerce# s)

execBO :: BO α a %1 -> Now α %1 -> (Now α, a)
{-# INLINE execBO #-}
execBO (BO f) !now =
  case runBO# f of
    (# s, !a #) -> dropState# s `PL.lseq` (now, a)

dropState# :: State# a %1 -> ()
{-# INLINE dropState# #-}
dropState# = Unsafe.toLinear \ !_ -> ()

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

-- | Alias of kind 'ak' to a resource of type 'a'
type Alias :: AliasKind -> Type -> Type
newtype Alias ak a = UnsafeAlias a

unsafeUnalias :: Alias ak a %1 -> a
unsafeUnalias (UnsafeAlias x) = x

type role Alias nominal representational

-- | Alias kind
data AliasKind
  = -- | Borrower
    Borrow BorrowKind Lifetime
  | -- | Lender
    Lend Lifetime

-- | Borrower kind
data BorrowKind
  = -- | Mutable
    Mut
  | -- | Shared
    Share

-- | Borrower of kind `bk` that is active during the lifetime 'α'
type Borrow :: BorrowKind -> Lifetime -> Type -> Type
type Borrow bk α = Alias ('Borrow bk α)

-- | Mutable borrower, which is affine and can update the data
type Mut :: Lifetime -> Type -> Type
type Mut α = Borrow 'Mut α

assocBorrowR ::
  Borrow bk ((α /\ β) /\ γ) a %1 ->
  Borrow bk (α /\ (β /\ γ)) a
{-# INLINE assocBorrowR #-}
assocBorrowR = coerceLin

assocBorrowL ::
  Borrow bk (α /\ (β /\ γ)) a %1 ->
  Borrow bk ((α /\ β) /\ γ) a
{-# INLINE assocBorrowL #-}
assocBorrowL = coerceLin

assocBorrowEq ::
  forall bk α β γ a.
  (Borrow bk ((α /\ β) /\ γ) a) :~: (Borrow bk (α /\ (β /\ γ)) a)
{-# INLINE assocBorrowEq #-}
assocBorrowEq = Unsafe.coerce $ Refl @(Borrow bk (α /\ β /\ γ) a)

assocLendR ::
  Lend ((α /\ β) /\ γ) a %1 ->
  Lend (α /\ (β /\ γ)) a
{-# INLINE assocLendR #-}
assocLendR = coerceLin

assocLendL ::
  Lend (α /\ (β /\ γ)) a %1 ->
  Lend ((α /\ β) /\ γ) a
{-# INLINE assocLendL #-}
assocLendL = coerceLin

assocLendEq :: forall α β γ a. (Lend ((α /\ β) /\ γ) a) :~: (Lend (α /\ (β /\ γ)) a)
{-# INLINE assocLendEq #-}
assocLendEq = Unsafe.coerce $ Refl @(Lend (α /\ β /\ γ) a)

instance (bk ~ 'Mut) => LinearOnly (Borrow bk α a) where
  linearOnly = UnsafeLinearOnly

deriving via AsAffine (Borrow bk α a) instance Consumable (Borrow bk α a)

-- | Shared borrower, which is unrestricted but usually can only read from the data
type Share :: Lifetime -> Type -> Type
type Share α = Borrow 'Share α

instance Affine (Borrow bk α a) where
  aff = UnsafeAff
  {-# INLINE aff #-}

instance (k ~ 'Borrow 'Share α) => Dupable (Alias k a) where
  dup2 = Unsafe.toLinear $ NonLinear.join (,)
  {-# INLINE dup2 #-}

instance (k ~ 'Borrow 'Share α) => Movable (Alias k a) where
  move = Unsafe.toLinear Ur
  {-# INLINE move #-}

instance (β <= α, a <: b, b <: a) => Mut α a <: Mut β b where
  subtype = UnsafeSubtype

instance (β <= α, a <: b) => Share α a <: Share β b where
  subtype = UnsafeSubtype

-- | Lender, which can retrieve the lifetime at the lifetime 'α'
type Lend :: Lifetime -> Type -> Type
type Lend α = Alias ('Lend α)

instance (α <= β, a <: b) => Lend α a <: Lend β b where
  subtype = UnsafeSubtype

-- | Borrow a resource linearly and obtain the mutable borrow to it and 'Lend' witness to 'reclaim' the resource to lend at the 'End' of the lifetime.
borrow :: a %1 -> Linearly %1 -> (Mut α a, Lend α a)
borrow = Unsafe.toLinear2 \ !a !_ ->
  (UnsafeAlias a, UnsafeAlias a)

-- | Analogous to 'borrow', but does not return the original 'Lend' to be reclaimed
borrow_ :: a %1 -> Linearly %1 -> Mut α a
borrow_ = Unsafe.toLinear2 \ !a !_ ->
  UnsafeAlias a

-- | Shares a mutable borrow, invalidating the original one.
share :: Borrow k α a %1 -> Ur (Share α a)
share = Unsafe.toLinear \(UnsafeAlias !a) -> Ur (UnsafeAlias a)

-- | Reclaims a 'borrow'ed resource at the 'End' of lifetime @α'.
reclaim :: Lend α a %1 -> End α -> a
reclaim = \(UnsafeAlias !a) !_ -> a

-- | Reborrow a mutable borrow into a sublifetime
reborrow :: (β <= α) => Mut α a %1 -> (Mut β a, Lend β (Mut α a))
reborrow = Unsafe.toLinear \ !mutA ->
  (Data.Coerce.coerce mutA, Data.Coerce.coerce mutA)

-- | Collapse a borrower to a mutable borrower
joinMut :: Borrow bk α (Mut β a) %1 -> Borrow bk (α /\ β) a
joinMut = coerceLin

-- | Distribute an alias over a functor.
class DistributesAlias f where
  split_ :: Alias ak (f x) %1 -> f (Alias ak x)
  default split_ ::
    (GenericDistributesAlias f) =>
    Alias ak (f x) %1 -> f (Alias ak x)
  split_ = genericSplit

split ::
  forall f x ak.
  (DistributesAlias f) =>
  Alias ak (f x) %1 -> f (Alias ak x)
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

splitPair :: Alias ak (a, b) %1 -> (Alias ak a, Alias ak b)
{-# INLINE splitPair #-}
splitPair = coerceLin

splitEither :: Alias ak (Either a b) %1 -> Either (Alias ak a) (Alias ak b)
{-# INLINE splitEither #-}
splitEither = coerceLin

instance (Unsatisfiable ('Text "Use splitEither directly!")) => DistributesAlias (Either e) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

instance (Unsatisfiable ('Text "Use splitPair instead!")) => DistributesAlias ((,) a) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

type GenericDistributesAlias f = (Generic1 f, GDistributeAlias (Rep1 f))

genericSplit ::
  forall f x ak.
  (GenericDistributesAlias f) =>
  Alias ak (f x) %1 -> f (Alias ak x)
{-# INLINE genericSplit #-}
genericSplit =
  to1
    . gdistributeAlias @(Rep1 f)
    . unsafeMapAlias from1

unsafeMapAlias :: (a %1 -> b) -> Alias ak a %1 -> Alias ak b
{-# INLINE unsafeMapAlias #-}
unsafeMapAlias f = coerceLin f

instance (GenericDistributesAlias f) => DistributesAlias (Generically1 f) where
  {-# INLINE split_ #-}
  split_ = Generically1 . genericSplit . unsafeMapAlias \(Generically1 f) -> f

class GDistributeAlias f where
  gdistributeAlias :: Alias ak (f x) %1 -> f (Alias ak x)

instance
  ( GDistributeAlias f
  , GDistributeAlias g
  ) =>
  GDistributeAlias (f :*: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias !(UnsafeAlias !(f :*: g)) =
    DataFlow.do
      !f <- gdistributeAlias $ UnsafeAlias f
      !g <- gdistributeAlias $ UnsafeAlias g
      f :*: g

instance
  ( GDistributeAlias f
  , GDistributeAlias g
  ) =>
  GDistributeAlias (f :+: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (UnsafeAlias x) = case x of
    L1 !l -> L1 (gdistributeAlias (UnsafeAlias l))
    R1 !r -> R1 (gdistributeAlias (UnsafeAlias r))

instance
  (Unsatisfiable (Text "Nonlinear fields cannot distribute borrows!")) =>
  GDistributeAlias (MP1 GHC.Many f)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = unsatisfiable

instance (GDistributeAlias f) => GDistributeAlias (MP1 GHC.One f) where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias =
    MP1 . gdistributeAlias . UnsafeAlias . unMP1 . unsafeUnalias

instance (GDistributeAlias f) => GDistributeAlias (M1 i c f) where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (UnsafeAlias (M1 x)) =
    M1 $ gdistributeAlias $ UnsafeAlias x

instance DistributesAlias Par1 where
  {-# INLINE split_ #-}
  split_ (UnsafeAlias (Par1 a)) = Par1 (UnsafeAlias a)

instance
  ( DistributesAlias f
  , DistributesAlias g
  , Data.Functor f
  ) =>
  GDistributeAlias (f :.: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (UnsafeAlias (Comp1 !fg)) =
    Comp1 $ Data.fmap split_ $ split_ $ UnsafeAlias fg

instance GDistributeAlias Par1 where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (UnsafeAlias (Par1 !a)) = Par1 (UnsafeAlias a)

instance
  (Unsatisfiable (Text "A type containing non-parametric field with type `" :<>: ShowType c :<>: Text "', which cannot be safely splitted!")) =>
  GDistributeAlias (K1 i c)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = unsatisfiable

instance GDistributeAlias U1 where
  gdistributeAlias = coerceLin
  {-# INLINE gdistributeAlias #-}

class Copyable a where
  copy :: Share α a %1 -> a

instance (Copyable a) => Copyable (Ur a) where
  copy (UnsafeAlias (Ur a)) = Ur $ copy $ UnsafeAlias a
  {-# INLINE copy #-}

instance
  (Unsatisfiable (ShowType (Ref a) :<>: Text " cannot be copied!")) =>
  Copyable (Ref a)
  where
  copy = unsatisfiable

instance
  (Unsatisfiable (ShowType (Array a) :<>: Text " cannot be copied!")) =>
  Copyable (Array a)
  where
  copy = unsatisfiable

instance
  (Unsatisfiable (ShowType (Vector a) :<>: Text " cannot be copied!")) =>
  Copyable (Vector a)
  where
  copy = unsatisfiable

copyMut :: (Copyable a) => Mut α a %1 -> Ur a
copyMut mut = let !(Ur shr) = share mut in Ur (copy shr)

newtype UnsafeAssumeNoVar a = UnsafeAssumeNoVar a

instance Copyable (UnsafeAssumeNoVar a) where
  copy = coerceLin
  {-# INLINE copy #-}

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
genericCopyShare (UnsafeAlias x) = to (gcopy (UnsafeAlias (from x)))

type GCopyable :: forall {k}. (k -> Type) -> Constraint
class GCopyable f where
  gcopy :: Share α (f x) %1 -> f x

instance (Copyable a) => GCopyable (K1 i a) where
  gcopy = coerceLin . unsafeUnalias

instance (GCopyable f, GCopyable g) => GCopyable (f :*: g) where
  gcopy (UnsafeAlias (f :*: g)) =
    gcopy (UnsafeAlias f) :*: gcopy (UnsafeAlias g)

instance (GCopyable f) => GCopyable (M1 i c f) where
  gcopy = \case
    UnsafeAlias (M1 x) -> M1 (gcopy (UnsafeAlias x))

instance (GCopyable f) => GCopyable (MP1 m f) where
  gcopy = \case
    UnsafeAlias (MP1 x) -> MP1 (gcopy (UnsafeAlias x))

instance (GCopyable f, GCopyable g) => GCopyable (f :+: g) where
  gcopy = \case
    UnsafeAlias (L1 x) -> L1 (gcopy (UnsafeAlias x))
    UnsafeAlias (R1 x) -> R1 (gcopy (UnsafeAlias x))

instance GCopyable U1 where
  gcopy = coerceLin . unsafeUnalias

instance GCopyable V1 where
  gcopy = \case {} . unsafeUnalias

instance (GenericCopyable a) => Copyable (Generically a) where
  copy = Generically . genericCopyShare . unsafeMapAlias (\(Generically x) -> x)

deriving via Generically () instance Copyable ()

deriving via
  Generically (Sum a)
  instance
    (Copyable a) => Copyable (Sum a)

deriving via
  Generically (Product a)
  instance
    (Copyable a) => Copyable (Product a)

deriving via
  Generically [a]
  instance
    (Copyable a) => Copyable [a]

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

deriving via
  Generically (Sem.Arg a b)
  instance
    (Copyable a, Copyable b) => Copyable (Sem.Arg a b)
