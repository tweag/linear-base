{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.Polarized (polarizedArrayTests) where

import qualified Data.Array.Polarized as Polar
import qualified Data.Array.Polarized.Pull as Pull
import qualified Data.Array.Polarized.Push as Push
import qualified Data.Vector as Vector
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude.Linear
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)
import qualified Prelude

{- TODO:

 * test fmap on push arrays
 * test zip on different length pull arrays errors

-}

-- # Tests and Utlities
-------------------------------------------------------------------------------

polarizedArrayTests :: TestTree
polarizedArrayTests =
    testGroup
        "Polarized arrays"
        [ testPropertyNamed "Push.alloc . transfer . Pull.fromVector = id" "polarRoundTrip" polarRoundTrip
        , testPropertyNamed "Push.append ~ Vec.append" "pushAppend" pushAppend
        , testPropertyNamed "Push.make ~ Vec.replicate" "pushMake" pushMake
        , testPropertyNamed "Pull.append ~ Vec.append" "pullAppend" pullAppend
        , testPropertyNamed "Pull.asList . Pull.fromVector ~ id" "pullAsList" pullAsList
        , testPropertyNamed "Pull.singleton x = [x]" "pullSingleton" pullSingleton
        , testPropertyNamed "Pull.splitAt ~ splitAt" "pullSplitAt" pullSplitAt
        , testPropertyNamed "Pull.make ~ Vec.replicate" "pullMake" pullMake
        , testPropertyNamed "Pull.zip ~ zip" "pullZip" pullZip
        , testPropertyNamed "Pull.safeIndex" "safeIndex" safeIndex
        ]

list :: Gen [Int]
list = Gen.list (Range.linear 0 1000) randInt

randInt :: Gen Int
randInt = Gen.int (Range.linear (-500) 500)

randNonnegInt :: Gen Int
randNonnegInt = Gen.int (Range.linear 0 500)

-- # Properties
-------------------------------------------------------------------------------

polarRoundTrip :: Property
polarRoundTrip = property Prelude.$ do
  xs <- forAll list
  let v = Vector.fromList xs
  v === Push.alloc (Polar.transfer (Pull.fromVector v))

pushAppend :: Property
pushAppend = property Prelude.$ do
  xs <- forAll list
  ys <- forAll list
  let v1 = Vector.fromList xs
  let v2 = Vector.fromList ys
  let sumVecs = v1 Vector.++ v2
  sumVecs === Push.alloc (Polar.walk v1 <> Polar.walk v2)

pushMake :: Property
pushMake = property Prelude.$ do
  n <- forAll randNonnegInt
  x <- forAll randInt
  let v = Vector.replicate n x
  v === Push.alloc (Push.make x n)

pullAppend :: Property
pullAppend = property Prelude.$ do
  xs <- forAll list
  ys <- forAll list
  let v1 = Vector.fromList xs
  let v2 = Vector.fromList ys
  let sumVecs = v1 Vector.++ v2
  sumVecs === Pull.toVector (Pull.fromVector v1 <> Pull.fromVector v2)

pullAsList :: Property
pullAsList = property Prelude.$ do
  xs <- forAll list
  xs === Pull.asList (Pull.fromVector (Vector.fromList xs))

pullSingleton :: Property
pullSingleton = property Prelude.$ do
  x <- forAll randInt
  [x] === Pull.asList (Pull.singleton x)

pullSplitAt :: Property
pullSplitAt = property Prelude.$ do
  xs <- forAll list
  n <- forAll randNonnegInt
  let v = Vector.fromList xs
  let (l, r) = Pull.split n (Pull.fromVector v)
  (Pull.asList l, Pull.asList r) === splitAt n xs

pullMake :: Property
pullMake = property Prelude.$ do
  x <- forAll randInt
  n <- forAll randNonnegInt
  replicate n x === Pull.asList (Pull.make x n)

pullZip :: Property
pullZip = property Prelude.$ do
  let genPairs = (,) Prelude.<$> randInt Prelude.<*> randInt
  as <- forAll (Gen.list (Range.linear 0 1000) genPairs)
  let (xs, ys) = unzip as
  let xs' = Pull.fromVector (Vector.fromList xs)
  let ys' = Pull.fromVector (Vector.fromList ys)
  zip xs ys === Pull.asList (Pull.zip xs' ys')

safeIndex :: Property
safeIndex = property Prelude.$ do
  x <- forAll randInt
  n <- forAll (Prelude.fmap (Prelude.+ 1) randNonnegInt)
  let array = Pull.make x n
  let (x0, array0) = Pull.safeIndex array 0
  let (x1, array1) = Pull.safeIndex array0 (n Prelude.- 1)
  let (x2, array2) = Pull.safeIndex array1 n
  let (x3, _) = Pull.safeIndex array2 (-1)
  x0 === Just x
  x1 === Just x
  x2 === Nothing
  x3 === Nothing
