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
import Data.Semigroup qualified as Sem
import Data.Tuple (Solo (..))
import Data.Type.Coercion (Coercion (..))
import Data.Var.Linear (Var)
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

-- | See also 'within'.
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

-- | Mutable reference to some resource 'a'
type Mut :: Lifetime -> Type -> Type
newtype Mut α a = UnsafeMut a

instance LinearOnly (Mut α a) where
  unsafeWithLinear = unsafeLinearOnly

type role Mut nominal nominal

instance Affable (Mut α a) where
  aff = UnsafeAff
  {-# INLINE aff #-}

deriving via AsAffable (Mut α a) instance Consumable (Mut α a)

instance (β <= α, a <: b, b <: a) => Mut α a <: Mut β b where
  upcast (UnsafeMut a) = UnsafeMut (upcast a)
  {-# INLINE upcast #-}

-- | Immutable shared reference to some resource 'a'
type Share :: Lifetime -> Type -> Type
newtype Share α a = UnsafeShare a

type role Share nominal representational

instance Affable (Share α a) where
  aff = UnsafeAff
  {-# INLINE aff #-}

deriving via AsAffable (Share α a) instance Consumable (Share α a)

unsafeWrapRef :: (SplittableRef_ ref) => a %1 -> ref a
{-# INLINE unsafeWrapRef #-}
unsafeWrapRef = coerceLin

unsafeUnwrapRef :: (SplittableRef_ ref) => ref a %1 -> a
{-# INLINE unsafeUnwrapRef #-}
unsafeUnwrapRef = coerceLin

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

-- | Borrow a resource linearly and obtain the mutable reference to it and 'Lend' witness to 'reclaim' the resource to lend at the 'End' of the lifetime.
borrow :: a %1 -> Linearly %1 -> (Mut α a, Lend α a)
borrow = Unsafe.toLinear \a lin ->
  lin `lseq` (UnsafeMut a, UnsafeLend a)

-- | Analogous to 'borrow', but does not return the original 'Lend' to be reclaimed
borrow_ :: a %1 -> Linearly %1 -> Mut α a
{-# INLINE borrow_ #-}
borrow_ = Unsafe.toLinear \(a :: a) lin ->
  lin `lseq` UnsafeMut a

-- | Shares a mutable reference, invalidating the original mutable reference.
share :: Mut α a %1 -> Ur (Share α a)
{-# INLINE share #-}
share = Unsafe.toLinear \(UnsafeMut a) -> Ur (UnsafeShare a)

-- | Reclaims a 'borrow'ed resource at the 'End' of lifetime @α'.
reclaim :: End α %1 -> Lend α a %1 -> a
reclaim end = end `lseq` \(UnsafeLend !a) -> a

-- | Reborrow a mutable reference from sublifetime
reborrow :: (β <= α) => Mut α a %1 -> (Mut β a, Lend β (Mut α a))
reborrow = Unsafe.toLinear \mutA ->
  (Data.Coerce.coerce mutA, Data.Coerce.coerce mutA)

unMutMut :: (α <= β) => Mut α (Mut β a) %1 -> Mut α a
unMutMut = coerceLin

unShrMut :: (α <= β) => Share α (Mut β a) %1 -> Share α a
unShrMut = coerceLin

unMutShr :: Mut α (Share β a) %1 -> Share β a
unMutShr = coerceLin

unShrShr :: Share α (Share β a) %1 -> Share β a
unShrShr = coerceLin

type SplittableRef_ :: (Type -> Type) -> Constraint
class
  (forall x. Coercible x (ref x)) =>
  SplittableRef_ ref
  where
  type RefLifetime ref :: Lifetime
  coercionWit :: Coercion x (ref x)

instance SplittableRef_ (Mut α) where
  type RefLifetime (Mut α) = α
  coercionWit = Coercion

instance SplittableRef_ (Share α) where
  type RefLifetime (Share α) = α
  coercionWit = Coercion

instance SplittableRef_ (Lend α) where
  type RefLifetime (Lend α) = α
  coercionWit = Coercion

-- | An abstraction over a type that can be
class (SplittableRef_ ref) => SplittableRef ref

instance (SplittableRef_ ref) => SplittableRef ref

type SplittableRefAt α ref = (SplittableRef ref, RefLifetime ref ~ α)

data CaseRef ref where
  IsMut :: CaseRef (Mut α)
  IsShare :: CaseRef (Share α)

-- | A constraint that requires @ref@ to be either a 'Share' or a 'Mut' reference, which is accessible in 'BO' regions.
class (SplittableRef ref) => AccessibleRef ref where
  caseRef :: CaseRef ref

instance AccessibleRef (Mut α) where
  caseRef = IsMut

instance AccessibleRef (Share α) where
  caseRef = IsShare

type AccessibleRefAt α ref =
  ( AccessibleRef ref
  , RefLifetime ref ~ α
  )

splitList :: (SplittableRef f) => f [x] %1 -> [f x]
splitList = split

splitPair :: (SplittableRef ref) => ref (a, b) %1 -> (ref a, ref b)
{-# INLINE splitPair #-}
splitPair = coerceLin . unsafeUnwrapRef

splitEither :: (SplittableRef ref) => ref (Either a b) %1 -> Either (ref a) (ref b)
{-# INLINE splitEither #-}
splitEither = coerceLin . unsafeUnwrapRef

-- | A dual to 'SplittableRef', which allows us to distribute a reference over a functor.
class DistributesRef f where
  split_ :: (SplittableRef ref) => ref (f x) %1 -> f (ref x)
  default split_ ::
    (GenericDistributesRef f, SplittableRef ref) =>
    ref (f x) %1 -> f (ref x)
  split_ = genericSplit

split ::
  forall ref f x.
  ( DistributesRef f
  , SplittableRef ref
  ) =>
  ref (f x) %1 -> f (ref x)
{-# INLINE [1] split #-}
split = split_

deriving anyclass instance DistributesRef Identity

deriving anyclass instance DistributesRef []

deriving anyclass instance DistributesRef Maybe

deriving anyclass instance DistributesRef Solo

deriving anyclass instance DistributesRef Ord.Down

deriving anyclass instance DistributesRef Sem.Dual

deriving anyclass instance DistributesRef Sem.Max

deriving anyclass instance DistributesRef Sem.Min

deriving anyclass instance DistributesRef Sem.First

deriving anyclass instance DistributesRef Sem.Last

deriving anyclass instance DistributesRef Mon.First

deriving anyclass instance DistributesRef Mon.Last

instance (Unsatisfiable ('Text "Use splitEither directly!")) => DistributesRef (Either e) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

instance (Unsatisfiable ('Text "Use splitPair instead!")) => DistributesRef ((,) a) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

type GenericDistributesRef f = (Generic1 f, GDistributeRef (Rep1 f))

genericSplit ::
  forall ref f x.
  ( GenericDistributesRef f
  , SplittableRef ref
  ) =>
  ref (f x) %1 -> f (ref x)
{-# INLINE genericSplit #-}
genericSplit =
  to1
    . gdistributeRef @(Rep1 f) @ref
    . unsafeMapRef from1

unsafeMapRef :: (SplittableRef ref) => (a %1 -> b) -> ref a %1 -> ref b
{-# INLINE unsafeMapRef #-}
unsafeMapRef f = coerceLin f

instance (GenericDistributesRef f) => DistributesRef (Generically1 f) where
  {-# INLINE split_ #-}
  split_ = Generically1 . genericSplit . unsafeMapRef \(Generically1 f) -> f

class GDistributeRef f where
  gdistributeRef :: (SplittableRef ref) => ref (f x) %1 -> f (ref x)

instance
  ( GDistributeRef f
  , GDistributeRef g
  ) =>
  GDistributeRef (f :*: g)
  where
  {-# INLINE gdistributeRef #-}
  gdistributeRef (ref :: ref a) =
    case unsafeUnwrapRef ref of
      f :*: g -> DataFlow.do
        f <- gdistributeRef $ unsafeWrapRef f
        g <- gdistributeRef $ unsafeWrapRef g
        f :*: g

instance
  ( GDistributeRef f
  , GDistributeRef g
  ) =>
  GDistributeRef (f :+: g)
  where
  {-# INLINE gdistributeRef #-}
  gdistributeRef ref = case unsafeUnwrapRef ref of
    L1 l -> L1 (gdistributeRef (unsafeWrapRef l))
    R1 r -> R1 (gdistributeRef (unsafeWrapRef r))

instance
  (Unsatisfiable (Text "Nonlinear fields cannot distribute references!")) =>
  GDistributeRef (MP1 GHC.Many f)
  where
  {-# INLINE gdistributeRef #-}
  gdistributeRef = unsatisfiable

instance (GDistributeRef f) => GDistributeRef (MP1 GHC.One f) where
  {-# INLINE gdistributeRef #-}
  gdistributeRef =
    MP1 . gdistributeRef . unsafeWrapRef . unMP1 . unsafeUnwrapRef

instance (GDistributeRef f) => GDistributeRef (M1 i c f) where
  {-# INLINE gdistributeRef #-}
  gdistributeRef = \x ->
    case unsafeUnwrapRef x of
      M1 x -> M1 $ gdistributeRef $ unsafeWrapRef x

instance DistributesRef Par1 where
  {-# INLINE split_ #-}
  split_ = \x -> case unsafeUnwrapRef x of
    Par1 a -> Par1 (unsafeWrapRef a)

instance
  ( DistributesRef f
  , DistributesRef g
  , Data.Functor f
  ) =>
  GDistributeRef (f :.: g)
  where
  {-# INLINE gdistributeRef #-}
  gdistributeRef = \(x :: ref _) -> case unsafeUnwrapRef x of
    Comp1 fg -> Comp1 $ Data.fmap split_ $ split_ $ unsafeWrapRef @ref fg

instance GDistributeRef Par1 where
  {-# INLINE gdistributeRef #-}
  gdistributeRef = \x -> case unsafeUnwrapRef x of
    Par1 a -> Par1 (unsafeWrapRef a)

instance
  (Unsatisfiable (Text "A type containing non-parametric field with type `" :<>: ShowType c :<>: Text "', which cannot be safely referenced!")) =>
  GDistributeRef (K1 i c)
  where
  {-# INLINE gdistributeRef #-}
  gdistributeRef = unsatisfiable

instance GDistributeRef U1 where
  gdistributeRef = coerceLin . unsafeUnwrapRef
  {-# INLINE gdistributeRef #-}

class Derefable a where
  unsafeDeref :: Share α a %1 -> a

instance
  (Unsatisfiable (ShowType (Var a) :<>: Text " cannot be dereferenced!")) =>
  Derefable (Var a)
  where
  unsafeDeref = unsatisfiable

instance
  (Unsatisfiable (ShowType (Array a) :<>: Text " cannot be dereferenced!")) =>
  Derefable (Array a)
  where
  unsafeDeref = unsatisfiable

instance
  (Unsatisfiable (ShowType (Vector a) :<>: Text " cannot be dereferenced!")) =>
  Derefable (Vector a)
  where
  unsafeDeref = unsatisfiable

derefShare :: (Derefable a) => Share α a %1 -> a
{-# INLINE [1] derefShare #-}
derefShare = unsafeDeref

{-# RULES
"derefShare/unsafeCoerce" [~1]
  derefShare =
    Unsafe.coerce
  #-}

newtype UnsafeAssumeNoVar a = UnsafeAssumeNoVar a

instance Derefable (UnsafeAssumeNoVar a) where
  unsafeDeref = coerceLin
  {-# INLINE unsafeDeref #-}

deriving via UnsafeAssumeNoVar Int instance Derefable Int

deriving via UnsafeAssumeNoVar Int8 instance Derefable Int8

deriving via UnsafeAssumeNoVar Int16 instance Derefable Int16

deriving via UnsafeAssumeNoVar Int32 instance Derefable Int32

deriving via UnsafeAssumeNoVar Int64 instance Derefable Int64

deriving via UnsafeAssumeNoVar Word instance Derefable Word

deriving via UnsafeAssumeNoVar Word8 instance Derefable Word8

deriving via UnsafeAssumeNoVar Word16 instance Derefable Word16

deriving via UnsafeAssumeNoVar Word32 instance Derefable Word32

deriving via UnsafeAssumeNoVar Word64 instance Derefable Word64

deriving via UnsafeAssumeNoVar Integer instance Derefable Integer

deriving via UnsafeAssumeNoVar Natural instance Derefable Natural

deriving via UnsafeAssumeNoVar Float instance Derefable Float

deriving via UnsafeAssumeNoVar Double instance Derefable Double

deriving via UnsafeAssumeNoVar Char instance Derefable Char

deriving via UnsafeAssumeNoVar Bool instance Derefable Bool

type GenericDerefable a = (Generic a, GDerefable (Rep a))

genericDerefShare :: (GenericDerefable a) => Share α a %1 -> a
{-# INLINE genericDerefShare #-}
genericDerefShare (UnsafeShare x) = to (gderef (UnsafeShare (from x)))

type GDerefable :: forall {k}. (k -> Type) -> Constraint
class GDerefable f where
  gderef :: Share α (f x) %1 -> f x

instance (Derefable a) => GDerefable (K1 i a) where
  gderef = coerceLin . unsafeUnwrapRef

instance (GDerefable f, GDerefable g) => GDerefable (f :*: g) where
  gderef (UnsafeShare (f :*: g)) =
    gderef (UnsafeShare f) :*: gderef (UnsafeShare g)

instance (GDerefable f) => GDerefable (M1 i c f) where
  gderef = \case
    UnsafeShare (M1 x) -> M1 (gderef (UnsafeShare x))

instance (GDerefable f) => GDerefable (MP1 m f) where
  gderef = \case
    UnsafeShare (MP1 x) -> MP1 (gderef (UnsafeShare x))

instance (GDerefable f, GDerefable g) => GDerefable (f :+: g) where
  gderef = \case
    UnsafeShare (L1 x) -> L1 (gderef (UnsafeShare x))
    UnsafeShare (R1 x) -> R1 (gderef (UnsafeShare x))

instance GDerefable U1 where
  gderef = coerceLin . unsafeUnwrapRef

instance GDerefable V1 where
  gderef = \case {} . unsafeUnwrapRef

instance (GenericDerefable a) => Derefable (Generically a) where
  unsafeDeref = Generically . genericDerefShare . unsafeMapRef (\(Generically x) -> x)

deriving via
  Generically (Sum a)
  instance
    (Derefable a) => Derefable (Sum a)

deriving via
  Generically (Product a)
  instance
    (Derefable a) => Derefable (Product a)

deriving via
  Generically (Sem.Max a)
  instance
    (Derefable a) => Derefable (Sem.Max a)

deriving via
  Generically (Maybe a)
  instance
    (Derefable a) => Derefable (Maybe a)

deriving via
  Generically (Sem.Min a)
  instance
    (Derefable a) => Derefable (Sem.Min a)

deriving via
  Generically (a, b)
  instance
    (Derefable a, Derefable b) =>
    Derefable (a, b)

deriving via
  Generically (a, b, c)
  instance
    (Derefable a, Derefable b, Derefable c) =>
    Derefable (a, b, c)

deriving via
  Generically (a, b, c, d)
  instance
    (Derefable a, Derefable b, Derefable c, Derefable d) =>
    Derefable (a, b, c, d)

deriving via
  Generically (Either a b)
  instance
    (Derefable a, Derefable b) => Derefable (Either a b)
