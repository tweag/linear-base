module Main where

import Gauge
import Data.Mutable.HashMap (hmbench)
import qualified Data.Mutable.Array as Array

main :: IO ()
main = do
  defaultMain
    [ hmbench
    , Array.benchmarks
    ]

