{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Monad.Borrow.Pure.Var (Var ()) where

import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Data.MutVar.Linear
import Prelude.Linear

atomicModify :: (β <= α) => Mut α (Var a) %1 -> (a %1 -> BO β (a, b)) %1 -> BO β (Mut α (Var a), b)
atomicModify = undefined
