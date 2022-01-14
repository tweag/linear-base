{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Data.Destination (destArrayTests)
import Test.Data.Mutable.Array (mutArrTests)
import Test.Data.Mutable.HashMap (mutHMTests)
import Test.Data.Mutable.Set (mutSetTests)
import Test.Data.Mutable.Vector (mutVecTests)
import Test.Data.Polarized (polarizedArrayTests)
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "All tests"
    [ mutArrTests,
      mutVecTests,
      mutHMTests,
      mutSetTests,
      destArrayTests,
      polarizedArrayTests
    ]
