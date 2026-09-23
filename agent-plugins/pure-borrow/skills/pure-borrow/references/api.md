# Core API by task

Signatures are those of pure-borrow 0.1 (`Control.Monad.Borrow.Pure`, plus `Control.Monad.Borrow.Pure.BO` for the lower-level ones marked *BO*).
Always check the Haddock of the installed version when in doubt.

## Types

```haskell
-- Lifetime is a kind; Static is the lifetime that never ends.
type (/\) :: Lifetime -> Lifetime -> Lifetime   -- meet: the longest lifetime shorter than both
class α <= β                                    -- α is a sublifetime of β (β outlives α)
type α >= β = β <= α

type BO :: Lifetime -> Type -> Type             -- computations during α; a control monad (abstract)
newtype After α a = After ((End α) => a)        -- post-processing once α has ended
class End α                                     -- evidence that α has ended (only inside After)

type Alias :: AliasKind -> Type -> Type         -- common zero-cost representation
type Borrow bk α a = Alias ('Borrow bk α) a     -- either Mut or Share
type Mut α a       = Borrow 'Mut α a
type Share α a     = Borrow 'Share α a
type Lend α a      = Alias ('Lend α) a

data Linearly           -- linearity witness: Dupable, Consumable, never Movable (abstract)
class LinearOnly a      -- values that only exist in a linear context: owners, Mut, Linearly
```

## Running `BO`

```haskell
linearly     :: Movable a => (Linearly %1 -> a) %1 -> a
runBO        :: Linearly %1 -> (forall α. BO α (After α a)) %1 -> a
runBO_       :: Linearly %1 -> (forall α. BO α a) %1 -> a
runBOLend    :: Linearly %1 -> (forall α. BO α (Lend α a)) %1 -> a
pureAfter    :: ((End α) => a) %1 -> BO α (After α a)
modifyBO     :: a %1 -> Linearly %1 -> (forall α. Mut α a %1 -> BO α r) %1 -> (r, a)
modifyBO_    :: a %1 -> Linearly %1 -> (forall α. Mut α a %1 -> BO α ()) %1 -> a
modifyLinearOnlyBO  :: LinearOnly a => a %1 -> (forall α. Mut α a %1 -> BO α r) %1 -> (r, a)
modifyLinearOnlyBO_ :: LinearOnly a => a %1 -> (forall α. Mut α a %1 -> BO α ()) %1 -> a
evaluateBO   :: a %1 -> BO α a                                     -- BO
```

## Linearity witnesses

```haskell
dup           :: Dupable a => a %1 -> (a, a)          -- works on Linearly
consume       :: Consumable a => a %1 -> ()
withLinearly  :: LinearOnly a => a %1 -> (Linearly, a)
askLinearly   :: BO α Linearly
asksLinearly  :: (Linearly %1 -> r) %1 -> BO α r
asksLinearlyM :: (Linearly %1 -> BO α r) %1 -> BO α r
```

## Borrowing and reclaiming

```haskell
borrowM         :: a %1 -> BO α (Mut α a, Lend α a)
borrowLinearlyM :: (Linearly %1 -> a) %1 -> BO α (Mut α a, Lend α a)
borrow          :: a %1 -> Linearly %1 -> (Mut α a, Lend α a)     -- BO; lifetime chosen by the caller
share           :: Borrow bk α a %1 -> Ur (Share α a)
reclaim         :: (End α) => Lend α a %1 -> a
reclaim'        :: Lend α a %1 -> After α a
```

## Sublifetime scopes

```haskell
reborrowing  :: Mut α a %1 -> (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 -> BO α' (r, Mut α a)
reborrowing_ :: Consumable r => Mut α a %1 -> (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 -> BO α' (Mut α a)
reborrowing' :: Mut α a %1 -> (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') (After β r)) %1 -> BO α' (r, Mut α a)
(<%~)        :: (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 -> Mut α a %1 -> BO α' (r, Mut α a)
(<%=)        :: (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') ()) %1 -> Mut α a %1 -> BO α' (Mut α a)

sharing      :: Mut α a %1 -> (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 -> BO α' (r, Mut α a)
sharing_     :: Consumable r => Mut α a %1 -> (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 -> BO α' (Mut α a)
sharing'     :: Mut α a %1 -> (forall β. Share (β /\ α) a -> BO (β /\ α') (After β r)) %1 -> BO α' (r, Mut α a)
(<$~)        :: (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 -> Mut α a %1 -> BO α' (r, Mut α a)
(<$=)        :: (forall β. Share (β /\ α) a -> BO (β /\ α') ()) %1 -> Mut α a %1 -> BO α' (Mut α a)

srunBO       :: (forall α. BO (α /\ β) (After α a)) %1 -> BO β a    -- run in an ephemeral sublifetime
srunBO_      :: (forall α. BO (α /\ β) a) %1 -> BO β a
reborrow     :: (α >= β) => Mut α a %1 -> (Mut β a, Lend β (Mut α a))   -- BO; manual form
```

Note that the `sharing` continuation receives the `Share` unrestricted (plain `->`), while the `reborrowing` continuation receives the `Mut` linearly.
`Control.Monad.Borrow.Pure.Experimental.Reborrowable` generalises both as `locally`, `locally_`, `locally'`.

## Subtyping and lifetimes

```haskell
upcast     :: (a <: b) => a %1 -> b      -- e.g. Mut α a <: Mut β a when α >= β
subShare   :: (α >= β) => Share α a -> Share β a
joinMut    :: Borrow bk α (Mut β a) %1 -> Borrow bk (α /\ β) a
joinLend   :: Lend α (Lend α a) %1 -> Lend α a
coerceShare :: Coercible a b => Share α a %1 -> Share α b
neverEnds  :: (HasCallStack, End Static) => a
```

## Copying and cloning

```haskell
class Copyable a where copy :: Borrow bk α a %1 -> a
copyMut :: Copyable a => Mut α a %1 -> Ur a
class Clone a where clone :: Share α a %1 -> BO α a
```

## Splitting

```haskell
splitPair   :: Alias ak (a, b) %1 -> (Alias ak a, Alias ak b)
splitEither :: Alias ak (Either a b) %1 -> Either (Alias ak a) (Alias ak b)
split       :: DistributesAlias f => Alias ak (f x) %1 -> f (Alias ak x)
```

`split` works for `Maybe`, lists, `Identity` and other single-parameter functors, and for your own types via `deriveGenericAnd1` plus `deriving anyclass instance DistributesAlias F`.
Use `splitPair`/`splitEither` for tuples and `Either`.

## Parallelism

```haskell
parBO   :: BO α a %1 -> BO α b %1 -> BO α (a, b)
newtype Par α a = Par (BO α a)     -- Data/Control Applicative whose (<*>) runs both sides in parallel
runPar  :: Par α a %1 -> BO α a
mapConcurrentlyOf :: Traversal s t a b -> (a %1 -> BO α b) -> s %1 -> BO α t
forConcurrentlyOf :: Traversal s t a b -> s %1 -> (a %1 -> BO α b) -> BO α t
```

## Re-exported linear-base classes

`Consumable (..)`, `Dupable (..)`, `dup`, `dup3`, `Movable (..)`, `Ur (..)`.
