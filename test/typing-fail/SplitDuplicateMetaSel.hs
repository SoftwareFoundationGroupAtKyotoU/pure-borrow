{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SplitDuplicateMetaSel where

import Data.Record.Linear.Borrow.Experimental.Split (SplittableRecord)
import Data.Ref.Linear (Ref)
import GHC.Exts qualified as GHC
import Generics.Linear qualified as GL
import Prelude.Linear

data DupMeta = DupMeta (Ref Int) (Ref Int)

instance GL.Generic DupMeta where
  type
    Rep DupMeta =
      GL.D1
        GHC.Any
        ( GL.C1
            GHC.Any
            ( GL.S1 ('GL.MetaSel ('Just "x") 'GL.NoSourceUnpackedness 'GL.NoSourceStrictness 'GL.DecidedLazy) (GL.K1 GL.R (Ref Int))
                GL.:*: GL.S1 ('GL.MetaSel ('Just "x") 'GL.NoSourceUnpackedness 'GL.NoSourceStrictness 'GL.DecidedLazy) (GL.K1 GL.R (Ref Int))
            )
        )
  from r = fromD r
    where
      fromD :: DupMeta %1 -> GL.Rep DupMeta p
      fromD (DupMeta a b) = GL.M1 (GL.M1 (GL.M1 (GL.K1 a) GL.:*: GL.M1 (GL.K1 b)))
  to rep = toD rep
    where
      toD :: GL.Rep DupMeta p %1 -> DupMeta
      toD (GL.M1 (GL.M1 (GL.M1 (GL.K1 a) GL.:*: GL.M1 (GL.K1 b)))) = DupMeta a b

-- EXPECT: Split record fields must be distinct, but got duplicate field
-- EXPECT: x
deriving anyclass instance SplittableRecord DupMeta
