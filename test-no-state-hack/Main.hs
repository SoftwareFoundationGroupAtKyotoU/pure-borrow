{-
Owners handed back after a scope, with GHC's state hack switched off.

With the state hack, GHC does not float code out of a state-threaded lambda, so an owner read in a scope's 'Control.Monad.Borrow.Pure.After' stays after the scope's writes by its position alone.
Without it, only a data dependency on the end of the scope keeps it there, which is what Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal" provides.
A user may compile with @-fno-state-hack@, and may force the token that 'Control.Monad.Borrow.Pure.Lifetime.Token.endLifetime' hands out unrestricted; the library must stay correct either way.
"PureBorrow.NoStateHack.ForcedLinearlySpec" forces 'Control.Monad.Borrow.Pure.Linearly' tokens before allocating with them, which one of its kernels got wrong only under this flag, and "PureBorrow.NoStateHack.ClosedRunSpec" runs 'Control.Monad.Borrow.Pure.BO' over actions with no free variables (Note [Tokens are opaque values] in "Control.Monad.Borrow.Pure.Lifetime.Token.Internal").

This is a component of its own, built with @-fno-state-hack@ for every module, because the flag did not take effect when set in one module's @OPTIONS_GHC@ of the main test suite: there the same kernels stayed green against a library that the kernels fail here.
GHC does not recompile a module when only this flag changes, so a build directory that predates the flag must be cleaned before these tests can be trusted.

This file is only the tasty-discover driver: each kernel module has a Spec module beside it with its tests.
-}
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}
