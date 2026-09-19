{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}

module ParWithoutForkable where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.BO

data ThreadBound

-- The marker dictionary can be erased, so observe this rejection at compile time.
bad :: BO' ThreadBound Static ((), ())
bad = parBO (Control.pure ()) (Control.pure ())
