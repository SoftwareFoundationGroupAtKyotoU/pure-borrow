{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types with '(<:)' instances from 'deriveSubtype', for "Data.Coerce.DirectedSpec"; test/typing-fail/DeriveSubtype*.hs holds the upcasts they must refuse.
module Data.Coerce.Directed.DeriveSubtype.Types (
  Env (..),
  MBox (..),
  List (..),
  Rose (..),
  Fn (..),
  Ph (..),
  M (..),
  PK (..),
) where

import Control.Monad.Borrow.Pure (Mut, Share)
import Data.Coerce.Directed.Unsafe (deriveSubtype)
import Data.Proxy (Proxy)

-- | A record of borrows: covariant in the lifetime, and invariant in what the 'Mut' points to.
data Env α a = Env (Share α Int) (Mut α a)

deriveSubtype ''Env

-- | Only a 'Mut', whose contents stay invariant.
newtype MBox α a = MBox (Mut α a)

deriveSubtype ''MBox

data List a = Nil | Cons a (List a)

deriveSubtype ''List

-- | Recursive through a list.
data Rose a = Rose a [Rose a]

deriveSubtype ''Rose

-- | Contravariant: the parameter is the argument of a function.
newtype Fn a = Fn (a -> Int)

deriveSubtype ''Fn

-- | The first parameter occurs in no field, so it stays fixed; its nominal role keeps 'Data.Coerce.Coercible' from relating two instantiations instead.
data Ph x a = Ph a

type role Ph nominal representational

deriveSubtype ''Ph

-- | A field stored at the multiplicity @p@.
data M p a where
  M :: a %p -> M p a

deriveSubtype ''M

-- | Poly-kinded: no field mentions the kind of @x@.
data PK (x :: k) a = PK (Proxy x) a

deriveSubtype ''PK
