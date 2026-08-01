-- The two @--ingredient@ flags rebuild 'Test.Tasty.Bench.benchIngredients':
-- @tasty-discover@ prepends each ingredient to 'Test.Tasty.defaultIngredients'
-- in reverse flag order, so @listingTests@ ends up first (keeping
-- @--list-tests@ working) and the composed bench reporter second.
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display -optF --ingredient=PureBorrow.Bench.Ingredients.benchReporter -optF --ingredient=Test.Tasty.Ingredients.Basic.listingTests #-}
