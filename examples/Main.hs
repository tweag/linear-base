module Main where

import Test.Tasty
import Test.Foreign (foreignGCTests)
import Test.Quicksort (quickSortTests)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests"
  [ foreignGCTests
  , quickSortTests
  ]

