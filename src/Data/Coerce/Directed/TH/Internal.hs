{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

-- | The Template Haskell deriver of '(<:)', exported by "Data.Coerce.Directed.Unsafe".
module Data.Coerce.Directed.TH.Internal (deriveSubtype, fieldSubtype, fieldMultiplicity) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Coerce.Directed.Internal (MultiplicityLe, SubtypeWitness (..), type (<:) (..))
import Data.Data (Data, cast, gmapQ, gmapT)
import Data.List (intercalate, nub)
import GHC.Exts (Multiplicity (..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (NameSpace (TcClsName))
import Prelude

{- | Declare '(<:)' between two instantiations of a data type of your own, field by field.

> data Env α a = Env (Share α Int) (Mut α a) (a -> Bool)
>
> deriveSubtype ''Env

declares

> instance (Share α Int <: Share α' Int, Mut α a <: Mut α' a', (a -> Bool) <: (a' -> Bool)) => Env α a <: Env α' a'

so that @upcast :: (α >= β) => Env α X %1 -> Env β X@ holds.
Each field is covariant: its type in the source must be a subtype of its type in the target.
Every field that mentions a parameter, also inside a kind, becomes one constraint at its declared type, so a parameter varies the way its fields allow: covariantly under a t'Control.Monad.Borrow.Pure.Share', invariantly under a t'Control.Monad.Borrow.Pure.Mut', contravariantly as the argument of a function.
A field stored at a multiplicity that mentions a parameter may become linear where it was unrestricted, never the other way round.
A parameter that no field mentions stays fixed in the instance, but 'Data.Coerce.Directed.upcast' still converts whatever 'Data.Coerce.coerce' converts: give such a parameter a nominal role, with a @type role@ declaration, if it must stay fixed.
A field whose type has no instance of '(<:)', such as @Ur@ or a type of your own not yet derived, relates only what 'Data.Coerce.coerce' relates; derive the instance for each type of a recursive group, with a splice of its own.
Where an 'Data.Coerce.Directed.upcast' does not hold, GHC reports the constraint left once the instances of the field types have reduced, not the field it came from, as in "Couldn't match type ‘α’ with ‘β’ arising from a use of ‘upcast’".

__Caveat:__ do not use this macro when defining a /mutable/ data structure.
The macro treats each field as covariant.
If your data type includes your own pure but mutable data, this variance can violate soundness.

Splice it after the declaration of the type and of every type in its recursive group, and before any use, in a module with @TemplateHaskell@, @UndecidableInstances@ and @MonoLocalBinds@ (which @LinearTypes@ implies), and with @DataKinds@ if a field type has a promoted constructor or a type-level literal, besides the extensions of @GHC2021@ it relies on.
It refuses, with a message saying why:

* a name that is not a @data@ or @newtype@ declaration, such as a type synonym or a data family, and a type without parameters;
* a type defined in pure-borrow, which declares every instance of '(<:)' its own types have: one without an instance stays invariant;
* a type whose constructors are not in scope, unqualified and unambiguous, where it is spliced: import them, as @import M (T (..))@ does, hide an import that clashes with one, or splice in the module that defines @T@;
* a datatype context, an existential or constrained constructor, a GADT constructor whose result is not the type applied to its parameters, and a field that mentions a kind variable that is not a parameter;
* a field whose type has a @forall@ or a constraint, also behind a type synonym;
* a field that mentions the type at other arguments than its parameters, as in @data Nested a = Flat a | Nest (Nested [a])@.

For a type it refuses, write the conversion by hand: match the constructor, 'Data.Coerce.Directed.upcast' each field, and rebuild.
-}
deriveSubtype :: Name -> Q [Dec]
deriveSubtype tyName = do
  info <- reify tyName
  (binders, cons) <- case info of
    TyConI (DataD cx _ bs _ cs _) -> noContext cx >> pure (bs, cs)
    TyConI (NewtypeD cx _ bs _ c _) -> noContext cx >> pure (bs, [c])
    TyConI TySynD {} -> refuse "is a type synonym; derive the instance for the data type it stands for, if that type is your own"
    FamilyI {} -> refuse "is a type or data family; deriveSubtype takes a data or newtype declaration"
    DataConI _ _ parent -> refuse ("is a data constructor; pass the name of its type, as in deriveSubtype ''" <> nameBase parent)
    _ -> refuse "is not a data or newtype declaration"
  when (namePackage tyName == namePackage ''SubtypeWitness) $
    refuse "is defined in pure-borrow, which declares every instance of (<:) its own types have: one without an instance stays invariant"
  let params = [n | b <- binders, Just n <- [visibleName b]]
  when (null params) $ refuse "has no parameters, so there are no two instantiations to relate"
  constructors <- forM (concatMap conNames cons) (constructorFields params)
  let varying = [i | (i, _) <- zip [0 :: Int ..] params, any (mentions i) constructors]
  sources <- mapM (newName . nameBase) params
  targets <- forM (zip [0 ..] sources) \(i, s) ->
    if i `elem` varying then newName (nameBase s <> "'") else pure s
  let applied ns = foldl AppT (ConT tyName) (map VarT ns)
      varyingVar c v = maybe False (`elem` varying) (lookup v (zip (conVars c) [0 ..]))
      pairsOf c = do
        let toSource = substTy (zip (conVars c) (map VarT sources))
            toTarget = substTy (zip (conVars c) (map VarT targets))
        Field m t <- conFields c
        [Left (toSource t, toTarget t) | any (varyingVar c) (typeVars t)]
          <> [Right (toTarget m, toSource m) | any (varyingVar c) (typeVars m)]
      pairs = nub (concatMap pairsOf constructors)
      typePairs = [p | Left p <- pairs]
      multiplicityPairs = [p | Right p <- pairs]
      context =
        [ConT ''(<:) `AppT` s `AppT` t | (s, t) <- typePairs]
          <> [ConT ''MultiplicityLe `AppT` t `AppT` s | (t, s) <- multiplicityPairs]
      -- The body names each constraint, so that GHC does not report the context redundant.
      body =
        foldr
          (\(s, t) e -> VarE 'fieldSubtype `AppTypeE` s `AppTypeE` t `AppE` e)
          (foldr (\(t, s) e -> VarE 'fieldMultiplicity `AppTypeE` t `AppTypeE` s `AppE` e) (ConE 'UnsafeSubtype) multiplicityPairs)
          typePairs
  requireExtensions ([DataKinds | any promotes context])
  pure
    [ InstanceD
        Nothing
        context
        (ConT ''(<:) `AppT` applied sources `AppT` applied targets)
        [ValD (VarP 'subtype) (NormalB body) []]
    ]
  where
    -- The splice as written, with one quote for a value and two for a type.
    call = "deriveSubtype " <> (if nameSpace tyName == Just TcClsName then "''" else "'") <> nameBase tyName

    refuse :: String -> Q a
    refuse reason = fail (call <> ": " <> nameBase tyName <> " " <> reason <> ".")

    byHand = "; convert it by hand instead: match the constructor, upcast each field, and rebuild"

    noContext cx = unless (null cx) (refuse ("has a datatype context" <> byHand))

    requireExtensions extra = do
      missing <-
        filterM
          (fmap not . isExtEnabled)
          ( [ UndecidableInstances
            , MonoLocalBinds
            , MultiParamTypeClasses
            , FlexibleContexts
            , FlexibleInstances
            , TypeApplications
            , ScopedTypeVariables
            , TypeOperators
            ]
              <> extra
          )
      unless (null missing) $
        fail (call <> ": enable " <> intercalate ", " (map show missing) <> " in this module, for the instance it declares.")

    -- A constructor's fields: 'reifyType' keeps their multiplicities, which 'reify' drops.
    constructorFields params c = do
      found <- recover (pure Nothing) (lookupValueName (nameBase c))
      here <- loc_module <$> location
      unless (found == Just c) $
        refuse
          if nameModule c == Just here
            then "has a constructor, " <> nameBase c <> ", whose name is ambiguous where it is spliced: hide the imported " <> nameBase c <> ", as import Prelude hiding (" <> nameBase c <> ") does for the Prelude's"
            else
              "needs its constructor "
                <> nameBase c
                <> " in scope, unqualified and unambiguous, where it is spliced: import it, as import M ("
                <> nameBase tyName
                <> " (..)) does from a module M that exports it, or derive the instance in the module that defines "
                <> nameBase tyName
                <> "; a type whose constructors are hidden keeps its invariants out of reach of deriveSubtype"
      ty <- reifyType c
      let (bound, cx, rhs) = case ty of
            ForallT bs cx' t -> (map bndrName bs, cx', t)
            t -> ([], [], t)
          (fields, result) = splitArrows rhs
      unless (null cx) $ refuse ("has a constructor, " <> nameBase c <> ", with a constraint context" <> byHand)
      vars <- case spine result of
        (ConT n, args)
          | n == tyName
          , Just vs <- mapM asVar args
          , length vs == length params
          , length (nub vs) == length vs ->
              pure vs
        _ -> refuse ("has a constructor, " <> nameBase c <> ", whose result is not " <> nameBase tyName <> " applied to its parameters" <> byHand)
      let free = nub (concatMap (\(Field m t) -> typeVars m <> typeVars t) fields)
      case filter (`notElem` vars) (filter (`elem` bound) free) of
        [] -> pure ()
        v : _ -> refuse ("has a constructor, " <> nameBase c <> ", whose fields mention " <> nameBase v <> ", which is not a parameter of " <> nameBase tyName <> ": an existential type variable, or a kind variable" <> byHand)
      forM_ (zip [1 :: Int ..] fields) \(i, Field _ t) -> do
        q <- quantified t
        when q $ refuse ("has a forall or a constraint in the type of field " <> show i <> " of " <> nameBase c <> byHand)
        unless (regular vars t) $
          refuse ("mentions itself at other arguments than its parameters in field " <> show i <> " of " <> nameBase c <> byHand)
      pure (Constructor vars fields)

    -- Whether every occurrence of the type in a field is at the constructor's own parameters.
    regular vars t = case spine t of
      (ConT n, args) | n == tyName -> args == map VarT vars
      (h, args@(_ : _)) -> regular vars h && all (regular vars) args
      (h, []) -> all (regular vars) (childTypes h)

-- | Names a field's evidence in a derived instance, so that GHC does not report the context redundant.
fieldSubtype :: forall a b r. (a <: b) => r -> r
fieldSubtype r = r

-- | As 'fieldSubtype', for the multiplicity a field is stored at.
fieldMultiplicity :: forall (p :: Multiplicity) (q :: Multiplicity) r. (MultiplicityLe p q) => r -> r
fieldMultiplicity r = r

data Field = Field Type Type

data Constructor = Constructor {conVars :: [Name], conFields :: [Field]}

-- | Whether parameter @i@ occurs in a field of the constructor, in its type, a kind in it, or its multiplicity.
mentions :: Int -> Constructor -> Bool
mentions i (Constructor vars fields) = case drop i vars of
  v : _ -> any (\(Field m t) -> v `elem` typeVars m || v `elem` typeVars t) fields
  [] -> False

conNames :: Con -> [Name]
conNames = \case
  NormalC n _ -> [n]
  RecC n _ -> [n]
  InfixC _ n _ -> [n]
  ForallC _ _ c -> conNames c
  GadtC ns _ _ -> ns
  RecGadtC ns _ _ -> ns

visibleName :: TyVarBndr BndrVis -> Maybe Name
visibleName = \case
  PlainTV n BndrReq -> Just n
  KindedTV n BndrReq _ -> Just n
  _ -> Nothing

bndrName :: TyVarBndr flag -> Name
bndrName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n

asVar :: Type -> Maybe Name
asVar = \case
  VarT n -> Just n
  SigT t _ -> asVar t
  ParensT t -> asVar t
  _ -> Nothing

-- | The fields of a constructor type, each with the multiplicity it is stored at, and its result.
splitArrows :: Type -> ([Field], Type)
splitArrows = \case
  AppT (AppT ArrowT a) r -> let (fs, res) = splitArrows r in (Field (PromotedT 'Many) a : fs, res)
  AppT (AppT (AppT MulArrowT m) a) r -> let (fs, res) = splitArrows r in (Field m a : fs, res)
  t -> ([], t)

-- | The head of a type application and its arguments, without kind arguments.
spine :: Type -> (Type, [Type])
spine = go []
  where
    go acc = \case
      AppT f x -> go (x : acc) f
      AppKindT f _ -> go acc f
      ParensT t -> go acc t
      SigT t _ -> go acc t
      h -> (h, acc)

-- | The free type variables of a type, kinds included, since a type family can dispatch on a kind.
typeVars :: Type -> [Name]
typeVars = \case
  VarT n -> [n]
  ForallT bs cx t -> bound bs (concatMap typeVars cx <> typeVars t)
  ForallVisT bs t -> bound bs (typeVars t)
  t -> concatMap typeVars (childTypes t)
  where
    bound bs vs = filter (`notElem` map bndrName bs) (concatMap bndrKindVars bs <> vs)
    bndrKindVars = \case
      KindedTV _ _ k -> typeVars k
      PlainTV {} -> []

-- | The types directly under a type, also inside lists and other structure.
childTypes :: Type -> [Type]
childTypes = concat . gmapQ collect
  where
    collect :: (Data d) => d -> [Type]
    collect d = case cast d of
      Just t -> [t]
      Nothing -> concat (gmapQ collect d)

substTy :: [(Name, Type)] -> Type -> Type
substTy s = go
  where
    go :: (Data d) => d -> d
    go d = case cast d of
      Just (VarT n) | Just t <- lookup n s, Just d' <- cast t -> d'
      _ -> gmapT go d

-- | Whether a type has a @forall@ or a constraint anywhere, looking through type synonyms.
quantified :: Type -> Q Bool
quantified = \case
  ForallT {} -> pure True
  ForallVisT {} -> pure True
  t -> do
    let (h, args) = spine t
    expanded <- case h of
      ConT n -> do
        info <- recover (pure Nothing) (Just <$> reify n)
        pure case info of
          Just (TyConI (TySynD _ bs rhs))
            | length args >= length bs ->
                let (now, rest) = splitAt (length bs) args
                 in Just (foldl AppT (substTy (zip (map bndrName bs) now) rhs) rest)
          _ -> Nothing
      _ -> pure Nothing
    case expanded of
      Just t' -> quantified t'
      Nothing -> or <$> mapM quantified (childTypes t)

-- | Whether a type needs @DataKinds@ where it is spliced; an arrow's multiplicity does not.
promotes :: Type -> Bool
promotes = \case
  PromotedT {} -> True
  PromotedTupleT {} -> True
  PromotedNilT -> True
  PromotedConsT -> True
  PromotedInfixT {} -> True
  PromotedUInfixT {} -> True
  LitT {} -> True
  AppT (AppT (AppT MulArrowT _) a) r -> promotes a || promotes r
  t -> any promotes (childTypes t)
