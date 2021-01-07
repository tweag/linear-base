{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.Destination (destArrayTests) where

import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import qualified Data.Array.Destination as DArray
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Vector as Vector
import Prelude.Linear
import qualified Prelude


-- # Tests and Utlities
-------------------------------------------------------------------------------

destArrayTests :: TestTree
destArrayTests = testGroup "Destination array tests"
  [ testProperty "alloc . mirror = id" roundTrip
  , testProperty "alloc . replicate = V.replicate" replicateTest
  , testProperty "alloc . fill = V.singleton" fillTest
  , testProperty "alloc n . fromFunction (+s) = V.fromEnum n s" fromFuncEnum
  ]

list :: Gen [Int]
list = Gen.list (Range.linear 0 1000) (Gen.int (Range.linear 0 100))

randInt :: Gen Int
randInt = Gen.int (Range.linear (-500) 500)

randNonnegInt :: Gen Int
randNonnegInt = Gen.int (Range.linear 0 500)


-- # Properties
-------------------------------------------------------------------------------

roundTrip :: Property
roundTrip = property Prelude.$ do
  xs <- forAll list
  let v = Vector.fromList xs
  let n = Vector.length v
  v === DArray.alloc n (DArray.mirror v id)

replicateTest :: Property
replicateTest = property Prelude.$ do
  n <- forAll randNonnegInt
  x <- forAll randInt
  let v = Vector.replicate n x
  v === DArray.alloc n (DArray.replicate x)


fillTest :: Property
fillTest = property Prelude.$ do
  x <- forAll randInt
  let v = Vector.singleton x
  v === DArray.alloc 1 (DArray.fill x)

fromFuncEnum :: Property
fromFuncEnum = property Prelude.$ do
  n <- forAll randNonnegInt
  start <- forAll randInt
  let v = Vector.enumFromN start n
  v === DArray.alloc n (DArray.fromFunction (Prelude.+ start))

