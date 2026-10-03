{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeDataConstructor where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: 'MkBox names the constructor, and the message names the type to pass instead.
-- EXPECT: deriveSubtype 'MkBox: MkBox is a data constructor; pass the name of its type, as in deriveSubtype ''Box.
data Box a = MkBox a

deriveSubtype 'MkBox
