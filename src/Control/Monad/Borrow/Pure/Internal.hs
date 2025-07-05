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

instance Affable (Mut α a) where
  aff = UnsafeAff
  {-# INLINE aff #-}

deriving via AsAffable (Mut α a) instance Consumable (Mut α a)

instance (β <= α, a <: b, b <: a) => Mut α a <: Mut β b where
  upcast (UnsafeMut a) = UnsafeMut (upcast a)
  {-# INLINE upcast #-}

-- | Immutable shared borrow to some resource 'a'
type Share :: Lifetime -> Type -> Type
newtype Share α a = UnsafeShare a

type role Share nominal representational

instance Affable (Share α a) where
  aff = UnsafeAff
  {-# INLINE aff #-}

deriving via AsAffable (Share α a) instance Consumable (Share α a)

unsafeWrapView :: (View_ view) => a %1 -> view a
{-# INLINE unsafeWrapView #-}
unsafeWrapView = coerceLin

unsafeUnwrapView :: (View_ view) => view a %1 -> a
{-# INLINE unsafeUnwrapView #-}
unsafeUnwrapView = coerceLin

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

type View_ :: (Type -> Type) -> Constraint
class
  (forall x. Coercible x (view x)) =>
  View_ view
  where
  type ViewLifetime view :: Lifetime
  coercionWit :: Coercion x (view x)

instance View_ (Mut α) where
  type ViewLifetime (Mut α) = α
  coercionWit = Coercion

instance View_ (Share α) where
  type ViewLifetime (Share α) = α
  coercionWit = Coercion

instance View_ (Lend α) where
  type ViewLifetime (Lend α) = α
  coercionWit = Coercion

-- | An abstraction over a type that can be
class (View_ view) => View view

instance (View_ view) => View view

type ViewAt α view = (View view, ViewLifetime view ~ α)

data CaseBorrow view where
  IsMut :: CaseBorrow (Mut α)
  IsShare :: CaseBorrow (Share α)

-- | A constraint that requires @view@ to be either a 'Share' or a 'Mut' borrow, which is accessible in 'BO' regions.
class (View view) => Borrow view where
  caseBorrow :: CaseBorrow view

instance Borrow (Mut α) where
  caseBorrow = IsMut

instance Borrow (Share α) where
  caseBorrow = IsShare

type BorrowAt α view =
  ( Borrow view
  , ViewLifetime view ~ α
  )

splitList :: (View f) => f [x] %1 -> [f x]
splitList = split

splitPair :: (View view) => view (a, b) %1 -> (view a, view b)
{-# INLINE splitPair #-}
splitPair = coerceLin . unsafeUnwrapView

splitEither :: (View view) => view (Either a b) %1 -> Either (view a) (view b)
{-# INLINE splitEither #-}
splitEither = coerceLin . unsafeUnwrapView

-- | A dual to 'View', which allows us to distribute a borrow over a functor.
class DistributesView f where
  split_ :: (View view) => view (f x) %1 -> f (view x)
  default split_ ::
    (GenericDistributesView f, View view) =>
    view (f x) %1 -> f (view x)
  split_ = genericSplit

split ::
  forall view f x.
  ( DistributesView f
  , View view
  ) =>
  view (f x) %1 -> f (view x)
{-# INLINE [1] split #-}
split = split_

deriving anyclass instance DistributesView Identity

deriving anyclass instance DistributesView []

deriving anyclass instance DistributesView Maybe

deriving anyclass instance DistributesView Solo

deriving anyclass instance DistributesView Ord.Down

deriving anyclass instance DistributesView Sem.Dual

deriving anyclass instance DistributesView Sem.Max

deriving anyclass instance DistributesView Sem.Min

deriving anyclass instance DistributesView Sem.First

deriving anyclass instance DistributesView Sem.Last

deriving anyclass instance DistributesView Mon.First

deriving anyclass instance DistributesView Mon.Last

instance (Unsatisfiable ('Text "Use splitEither directly!")) => DistributesView (Either e) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

instance (Unsatisfiable ('Text "Use splitPair instead!")) => DistributesView ((,) a) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

type GenericDistributesView f = (Generic1 f, GDistributeView (Rep1 f))

genericSplit ::
  forall view f x.
  ( GenericDistributesView f
  , View view
  ) =>
  view (f x) %1 -> f (view x)
{-# INLINE genericSplit #-}
genericSplit =
  to1
    . gdistributeView @(Rep1 f) @view
    . unsafeMapView from1

unsafeMapView :: (View view) => (a %1 -> b) -> view a %1 -> view b
{-# INLINE unsafeMapView #-}
unsafeMapView f = coerceLin f

instance (GenericDistributesView f) => DistributesView (Generically1 f) where
  {-# INLINE split_ #-}
  split_ = Generically1 . genericSplit . unsafeMapView \(Generically1 f) -> f

class GDistributeView f where
  gdistributeView :: (View view) => view (f x) %1 -> f (view x)

instance
  ( GDistributeView f
  , GDistributeView g
  ) =>
  GDistributeView (f :*: g)
  where
  {-# INLINE gdistributeView #-}
  gdistributeView (view :: view a) =
    case unsafeUnwrapView view of
      f :*: g -> DataFlow.do
        f <- gdistributeView $ unsafeWrapView f
        g <- gdistributeView $ unsafeWrapView g
        f :*: g

instance
  ( GDistributeView f
  , GDistributeView g
  ) =>
  GDistributeView (f :+: g)
  where
  {-# INLINE gdistributeView #-}
  gdistributeView view = case unsafeUnwrapView view of
    L1 l -> L1 (gdistributeView (unsafeWrapView l))
    R1 r -> R1 (gdistributeView (unsafeWrapView r))

instance
  (Unsatisfiable (Text "Nonlinear fields cannot distribute borrows!")) =>
  GDistributeView (MP1 GHC.Many f)
  where
  {-# INLINE gdistributeView #-}
  gdistributeView = unsatisfiable

instance (GDistributeView f) => GDistributeView (MP1 GHC.One f) where
  {-# INLINE gdistributeView #-}
  gdistributeView =
    MP1 . gdistributeView . unsafeWrapView . unMP1 . unsafeUnwrapView

instance (GDistributeView f) => GDistributeView (M1 i c f) where
  {-# INLINE gdistributeView #-}
  gdistributeView = \x ->
    case unsafeUnwrapView x of
      M1 x -> M1 $ gdistributeView $ unsafeWrapView x

instance DistributesView Par1 where
  {-# INLINE split_ #-}
  split_ = \x -> case unsafeUnwrapView x of
    Par1 a -> Par1 (unsafeWrapView a)

instance
  ( DistributesView f
  , DistributesView g
  , Data.Functor f
  ) =>
  GDistributeView (f :.: g)
  where
  {-# INLINE gdistributeView #-}
  gdistributeView = \(x :: view _) -> case unsafeUnwrapView x of
    Comp1 fg -> Comp1 $ Data.fmap split_ $ split_ $ unsafeWrapView @view fg

instance GDistributeView Par1 where
  {-# INLINE gdistributeView #-}
  gdistributeView = \x -> case unsafeUnwrapView x of
    Par1 a -> Par1 (unsafeWrapView a)

instance
  (Unsatisfiable (Text "A type containing non-parametric field with type `" :<>: ShowType c :<>: Text "', which cannot be safely splitted!")) =>
  GDistributeView (K1 i c)
  where
  {-# INLINE gdistributeView #-}
  gdistributeView = unsatisfiable

instance GDistributeView U1 where
  gdistributeView = coerceLin . unsafeUnwrapView
  {-# INLINE gdistributeView #-}

class Deborrowable a where
  unsafeDeborrow :: Share α a %1 -> a

instance
  (Unsatisfiable (ShowType (Ref a) :<>: Text " cannot be deborrowed!")) =>
  Deborrowable (Ref a)
  where
  unsafeDeborrow = unsatisfiable

instance
  (Unsatisfiable (ShowType (Array a) :<>: Text " cannot be deborrowed!")) =>
  Deborrowable (Array a)
  where
  unsafeDeborrow = unsatisfiable

instance
  (Unsatisfiable (ShowType (Vector a) :<>: Text " cannot be deborrowed!")) =>
  Deborrowable (Vector a)
  where
  unsafeDeborrow = unsatisfiable

deborrow :: (Deborrowable a) => Share α a %1 -> a
{-# INLINE [1] deborrow #-}
deborrow = unsafeDeborrow

{-# RULES
"deborrow/unsafeCoerce" [~1]
  deborrow =
    Unsafe.coerce
  #-}

newtype UnsafeAssumeNoVar a = UnsafeAssumeNoVar a

instance Deborrowable (UnsafeAssumeNoVar a) where
  unsafeDeborrow = coerceLin
  {-# INLINE unsafeDeborrow #-}

deriving via UnsafeAssumeNoVar Int instance Deborrowable Int

deriving via UnsafeAssumeNoVar Int8 instance Deborrowable Int8

deriving via UnsafeAssumeNoVar Int16 instance Deborrowable Int16

deriving via UnsafeAssumeNoVar Int32 instance Deborrowable Int32

deriving via UnsafeAssumeNoVar Int64 instance Deborrowable Int64

deriving via UnsafeAssumeNoVar Word instance Deborrowable Word

deriving via UnsafeAssumeNoVar Word8 instance Deborrowable Word8

deriving via UnsafeAssumeNoVar Word16 instance Deborrowable Word16

deriving via UnsafeAssumeNoVar Word32 instance Deborrowable Word32

deriving via UnsafeAssumeNoVar Word64 instance Deborrowable Word64

deriving via UnsafeAssumeNoVar Integer instance Deborrowable Integer

deriving via UnsafeAssumeNoVar Natural instance Deborrowable Natural

deriving via UnsafeAssumeNoVar Float instance Deborrowable Float

deriving via UnsafeAssumeNoVar Double instance Deborrowable Double

deriving via UnsafeAssumeNoVar Char instance Deborrowable Char

deriving via UnsafeAssumeNoVar Bool instance Deborrowable Bool

type GenericDeborrowable a = (Generic a, GDeborrowable (Rep a))

genericDeborrowShare :: (GenericDeborrowable a) => Share α a %1 -> a
{-# INLINE genericDeborrowShare #-}
genericDeborrowShare (UnsafeShare x) = to (gdeborrow (UnsafeShare (from x)))

type GDeborrowable :: forall {k}. (k -> Type) -> Constraint
class GDeborrowable f where
  gdeborrow :: Share α (f x) %1 -> f x

instance (Deborrowable a) => GDeborrowable (K1 i a) where
  gdeborrow = coerceLin . unsafeUnwrapView

instance (GDeborrowable f, GDeborrowable g) => GDeborrowable (f :*: g) where
  gdeborrow (UnsafeShare (f :*: g)) =
    gdeborrow (UnsafeShare f) :*: gdeborrow (UnsafeShare g)

instance (GDeborrowable f) => GDeborrowable (M1 i c f) where
  gdeborrow = \case
    UnsafeShare (M1 x) -> M1 (gdeborrow (UnsafeShare x))

instance (GDeborrowable f) => GDeborrowable (MP1 m f) where
  gdeborrow = \case
    UnsafeShare (MP1 x) -> MP1 (gdeborrow (UnsafeShare x))

instance (GDeborrowable f, GDeborrowable g) => GDeborrowable (f :+: g) where
  gdeborrow = \case
    UnsafeShare (L1 x) -> L1 (gdeborrow (UnsafeShare x))
    UnsafeShare (R1 x) -> R1 (gdeborrow (UnsafeShare x))

instance GDeborrowable U1 where
  gdeborrow = coerceLin . unsafeUnwrapView

instance GDeborrowable V1 where
  gdeborrow = \case {} . unsafeUnwrapView

instance (GenericDeborrowable a) => Deborrowable (Generically a) where
  unsafeDeborrow = Generically . genericDeborrowShare . unsafeMapView (\(Generically x) -> x)

deriving via
  Generically (Sum a)
  instance
    (Deborrowable a) => Deborrowable (Sum a)

deriving via
  Generically (Product a)
  instance
    (Deborrowable a) => Deborrowable (Product a)

deriving via
  Generically (Sem.Max a)
  instance
    (Deborrowable a) => Deborrowable (Sem.Max a)

deriving via
  Generically (Maybe a)
  instance
    (Deborrowable a) => Deborrowable (Maybe a)

deriving via
  Generically (Sem.Min a)
  instance
    (Deborrowable a) => Deborrowable (Sem.Min a)

deriving via
  Generically (a, b)
  instance
    (Deborrowable a, Deborrowable b) =>
    Deborrowable (a, b)

deriving via
  Generically (a, b, c)
  instance
    (Deborrowable a, Deborrowable b, Deborrowable c) =>
    Deborrowable (a, b, c)

deriving via
  Generically (a, b, c, d)
  instance
    (Deborrowable a, Deborrowable b, Deborrowable c, Deborrowable d) =>
    Deborrowable (a, b, c, d)

deriving via
  Generically (Either a b)
  instance
    (Deborrowable a, Deborrowable b) => Deborrowable (Either a b)
