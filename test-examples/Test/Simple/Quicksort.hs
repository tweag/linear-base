{-# LANGUAGE OverloadedStrings #-}

module Test.Simple.Quicksort (quicksortTests) where

import Data.List (sort)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Simple.Quicksort (quicksortUsingArray, quicksortUsingList)
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
