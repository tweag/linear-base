module Main where

import Gauge
import Data.Mutable.HashMap (hmbench)

main :: IO ()
main = defaultMain [hmbench]

