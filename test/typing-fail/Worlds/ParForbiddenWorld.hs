{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ParForbiddenWorld where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.BO
import GHC.TypeError (ErrorMessage (Text), Unsatisfiable)

data ThreadBound

instance (Unsatisfiable (Text "ThreadBound actions must remain on their original thread")) => Forkable ThreadBound

bad :: BO' ThreadBound Static ((), ())
bad = parBO (Control.pure ()) (Control.pure ())
