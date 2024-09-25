module Main where

import Test.Compact (compactTests)
import Test.Data.Array.Mutable.Quicksort (quicksortTests)
import Test.Foreign (foreignGCTests)
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "All tests"
    [ foreignGCTests,
      quicksortTests,
      compactTests
    ]
