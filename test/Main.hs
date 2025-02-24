{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Foo (foo)
import Prelude.Linear

-- Just a placeholder
main :: IO ()
main = foo "The answer is: " & \(s, n) -> putStrLn (s ++ show n)
