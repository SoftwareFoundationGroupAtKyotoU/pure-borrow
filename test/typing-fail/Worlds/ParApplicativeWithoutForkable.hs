{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}

module ParApplicativeWithoutForkable where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.BO

data ThreadBound

bad :: Par ThreadBound Static ()
bad = Control.pure ()
