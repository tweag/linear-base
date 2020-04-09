{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Hedgehog

import Test.Data.Mutable.Array (mutArrTests)
import Test.Data.Mutable.Vector (mutVecTests)
import Test.Data.Mutable.HashMap (mutHMTests)

main :: IO ()
main = sequence_ allTests

allTests :: [IO Bool]
allTests =
  [ mutArrTests
  , mutVecTests
  , mutHMTests
  ]

