{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import PureBorrow.Internal.Bench.QSort (defaultMain)

main :: IO ()
main = defaultMain