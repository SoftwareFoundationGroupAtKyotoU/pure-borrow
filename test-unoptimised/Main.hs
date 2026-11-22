{-
Reads and writes through borrows, ordered by the state token, in code compiled at @-O0@.

A user's module compiled at @-O0@, or loaded into GHCi, calls the library's compiled code: @-O0@ implies @-fignore-interface-pragmas@, so it sees none of the library's unfoldings.
This is a component of its own, with every test module at @-O0@, so that its kernels run that way.
In the main test suite they could not: there, a plain @-O0@ module hides the library's unfoldings from the @-O2@ modules compiled after it in the same session, and one that passes @-fno-ignore-interface-pragmas@ inlines the library's @INLINE@ functions, which a user's @-O0@ module does not.

This file is only the tasty-discover driver, which holds no kernel: the tests are in "PureBorrow.Unoptimised.OrderingSpec".
-}
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}
