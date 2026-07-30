{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Concurrent.DivideConquer.Linear.Types.Internal (
  module Control.Concurrent.DivideConquer.Linear.Types.Internal,
) where

import Control.Monad.Borrow.Pure.BO
import Prelude.Linear

data Result c β t a r
  = Done !r
  | Continue !(t (Ur c, Mut β a))
