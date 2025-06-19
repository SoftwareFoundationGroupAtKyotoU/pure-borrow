{-# LANGUAGE NoImplicitPrelude #-}

module Control.Syntax.DataFlow ((>>=), (>>), (*>), pure, return, (<*>), (<*)) where

import Prelude.Linear qualified as PL

(>>=) :: a %1 -> (a %1 -> b) %1 -> b
a >>= b = b a

(>>) :: (PL.Consumable a) => a %1 -> b %1 -> b
a >> b = PL.consume a PL.& \() -> b

(*>) :: (PL.Consumable a) => a %1 -> b %1 -> b
(*>) = (>>)

(<*) :: (PL.Consumable b) => a %1 -> b %1 -> a
a <* b = PL.consume b PL.& \() -> a

pure :: a %1 -> a
pure = PL.id

return :: a %1 -> a
return = PL.id

(<*>) :: (a %1 -> b) %1 -> a %1 -> b
f <*> a = f a
