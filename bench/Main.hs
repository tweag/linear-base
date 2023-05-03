module Main where

import qualified Data.Mutable.Array as Array
import qualified Data.Mutable.HashMap as HashMap
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = do
  defaultMain
    [ Array.benchmarks,
      HashMap.benchmarks
    ]
