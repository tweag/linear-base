module Main where

import Test.Foreign (foreignGCTests)
import Test.Quicksort (quickSortTests)
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "All tests"
    [ foreignGCTests,
      quickSortTests
    ]
