module Main where

import Test.Foreign (foreignGCTests)
import Test.Simple.Quicksort (quicksortTests)
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "All tests"
    [ foreignGCTests,
      quicksortTests
    ]
