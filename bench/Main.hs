module Main where

import Gauge
import Data.Mutable.HashMap (hmbench, getHMInput)
import qualified Data.Mutable.Array as Array

main :: IO ()
main = do
  hmInput <- getHMInput
  defaultMain
    [ hmbench hmInput
    , Array.benchmarks
    ]

