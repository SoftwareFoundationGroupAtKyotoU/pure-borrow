{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foo (foo) where

import Prelude.Linear

-- Just a placeholder
foo :: a %1 -> (a, Int)
foo a = a & \b -> (b, 42)
