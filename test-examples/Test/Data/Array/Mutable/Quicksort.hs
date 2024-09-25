{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Array.Mutable.Quicksort (quicksortTests) where

import Data.Array.Mutable.Quicksort (quicksortUsingArray, quicksortUsingList)
import Data.List (sort)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

quicksortTests :: TestTree
quicksortTests =
  testGroup
    "quicksort tests"
    [ testPropertyNamed "sort xs === quicksortUsingArray xs" "testQuicksortUsingArray" testQuicksortUsingArray,
      testPropertyNamed "sort xs === quicksortUsingList xs" "testQuicksortUsingList" testQuicksortUsingList
    ]

testQuicksortUsingArray :: Property
testQuicksortUsingArray = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 100)
  sort xs === quicksortUsingArray xs

testQuicksortUsingList :: Property
testQuicksortUsingList = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 100)
  sort xs === quicksortUsingList xs
