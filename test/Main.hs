{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Compact.Destination (destinationTests)
import Test.Data.Array.Destination (destArrayTests)
import Test.Data.Array.Mutable (mutArrTests)
import Test.Data.Array.Polarized (polarizedArrayTests)
import Test.Data.Functor.Linear (genericTests)
import Test.Data.HashMap.Mutable (mutHMTests)
import Test.Data.Replicator (replicatorInspectionTests)
import Test.Data.Set.Mutable (mutSetTests)
import Test.Data.V (vInspectionTests)
import Test.Data.Vector.Mutable (mutVecTests)
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "All tests"
    [ testGroup
        "Functional tests"
        [ mutArrTests,
          mutVecTests,
          mutHMTests,
          mutSetTests,
          destArrayTests,
          polarizedArrayTests,
          genericTests,
          destinationTests
        ],
      testGroup
        "Inspection tests"
        [ vInspectionTests,
          replicatorInspectionTests
        ]
    ]
