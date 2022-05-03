{-# LANGUAGE OverloadedStrings #-}

module Test.Quicksort (quickSortTests) where

import Data.List (sort)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Simple.Quicksort (quickSort)
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

quickSortTests :: TestTree
quickSortTests = testPropertyNamed "quicksort sorts" "testQuicksort" testQuicksort

testQuicksort :: Property
testQuicksort = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 100)
  sort xs === quickSort xs
