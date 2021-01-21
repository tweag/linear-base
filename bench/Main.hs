module Main where

import Gauge
import Data.Mutable.HashMap (hmbench, getHMInput)

main :: IO ()
main = do
  hmInput <- getHMInput
  defaultMain
    [ hmbench hmInput
    ]

