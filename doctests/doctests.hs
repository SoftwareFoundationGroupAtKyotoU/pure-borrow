{-# LANGUAGE CPP #-}
module Main (main) where

#if defined(RUN_DOCTESTS)
import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "pure-borrow" =<< getArgs
#else
main :: IO ()
main = putStrLn "Doctests are disabled for GHC <9.12.3."
#endif
