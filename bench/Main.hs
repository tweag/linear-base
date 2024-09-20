module Main where

import qualified Bench.Data.Array.Mutable as Array
import qualified Bench.Data.Array.Mutable.Quicksort as Quicksort
import qualified Bench.Data.HashMap.Mutable as HashMap
import qualified Bench.Compact as Compact
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = do
  defaultMain
    [ Array.benchmarks,
      Quicksort.benchmarks,
      HashMap.benchmarks,
      Compact.benchmarks
    ]
