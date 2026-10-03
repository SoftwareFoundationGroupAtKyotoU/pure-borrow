{-# LANGUAGE ImportQualifiedPost #-}

module ModifyBoxedMVectorRenamed where

import Data.Vector.Mutable.Linear.Borrow qualified as VL

-- This fixture must not typecheck: the old name survives only to say what
-- replaced it and which obligation came with the rename.
-- EXPECT: modifyBoxedMVector was renamed to unsafeModifyBoxedMVector
-- EXPECT: do not read or reuse the MVector afterwards
oldName :: ()
oldName = VL.modifyBoxedMVector
