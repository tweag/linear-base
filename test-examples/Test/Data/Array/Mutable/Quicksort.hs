{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Array.Mutable.Quicksort (quicksortTests) where

import Data.Array.Mutable.Quicksort (qsortUsingArray, qsortUsingList)
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
    [ testPropertyNamed "sort xs === qsortUsingArray xs" "testQsortUsingArray" testQsortUsingArray,
      testPropertyNamed "sort xs === qsortUsingList xs" "testQsortUsingList" testQsortUsingList
    ]

testQsortUsingArray :: Property
testQsortUsingArray = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 100)
  sort xs === qsortUsingArray xs

testQsortUsingList :: Property
testQsortUsingList = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 100)
  sort xs === qsortUsingList xs
