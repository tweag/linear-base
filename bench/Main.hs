module Main where

import qualified Data.Mutable.Array as Array
import Data.Mutable.HashMap (hmbench)
import Gauge

main :: IO ()
main = do
  defaultMain
    [ hmbench,
      Array.benchmarks
    ]
