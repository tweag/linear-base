module Main where

import qualified Data.Mutable.Array as Array
import qualified Data.Mutable.HashMap as HashMap
import qualified Data.Mutable.Quicksort as Quicksort
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = do
  defaultMain
    [ Array.benchmarks,
      HashMap.benchmarks,
      Quicksort.benchmarks
    ]
