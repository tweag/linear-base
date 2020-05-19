{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable sets.

module Test.Data.Mutable.Set
  ( mutSetTests,
  )
where

import qualified Data.Set.Mutable.Linear as Set
import Data.Unrestricted.Linear
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- # Exported Tests
--------------------------------------------------------------------------------

mutSetTests :: TestTree
mutSetTests = testGroup "Mutable set tests" group

group :: [TestTree]
group =
  -- Tests of the form [accessor (mutator)]
  [ testProperty "∀ x. member (sing x) x = True" memberSing1
  , testProperty "∀ x,y/=x. member (sing x) y = False" memberSing2
  , testProperty "∀ x. member (insert s x) x = True" memberInsert1
  , testProperty "∀ x,y/=x. member (insert s x) y = member s y" memberInsert2
  , testProperty "∀ x. member (delete s x) x = False" memberDelete1
  , testProperty "∀ x,y/=x. member (delete s x) y = member s y" memberDelete2
  , testProperty "∀ x. size (sing x) = 1" sizeSing
  , testProperty "∀ s, x \\in s. size (insert s x) = size s" sizeInsert1
  , testProperty "∀ s, x \\notin s. size (insert s x) = size s + 1" sizeInsert2
  , testProperty "∀ s, x \\in s. size (delete s x) = size s - 1" sizeDelete1
  , testProperty "∀ s, x \\notin s. size (delete s x) = size s" sizeDelete2
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type SetTester = Set.Set Int #-> Unrestricted (TestT IO ())

-- | A random list
nonemptyList :: Gen [Int]
nonemptyList = do
  size <- Gen.int $ Range.linearFrom 1 1 1000
  let size' = Range.singleton size
  Gen.list size' $ Gen.int $ Range.linearFrom 0 (-100) 100

-- | A random value
value :: Gen Int
value = Gen.int (Range.linear (-100) 100)

testEqual :: (Show a, Eq a) =>
  Unrestricted a #->
  Unrestricted a #->
  Unrestricted (TestT IO ())
testEqual (Unrestricted x) (Unrestricted y) = Unrestricted (x === y)

-- XXX: This is a terrible name
getSnd :: (Consumable a, Movable b) => (a, b) #-> Unrestricted b
getSnd (a, b) = lseq a (move b)

-- # Tests
--------------------------------------------------------------------------------

memberSing1 :: Property
memberSing1 = property $ do
  val <- forAll value
  let tester = memberSingTest val
  test $ unUnrestricted Linear.$ Set.singleton val tester

memberSingTest :: Int -> SetTester
memberSingTest val set =
  testEqual (Unrestricted True) (getSnd Linear.$ Set.member set val)

memberSing2 :: Property
memberSing2 = property $ do
  val1 <- forAll value
  val2 <- forAll $ Gen.filter (/= val1) value
  let tester = memberSing2Test val2
  test $ unUnrestricted Linear.$ Set.singleton val1 tester

memberSing2Test :: Int -> SetTester
memberSing2Test val2 set =
  testEqual (Unrestricted False) (getSnd Linear.$ Set.member set val2)

memberInsert1 :: Property
memberInsert1 = property $ do
  val <- forAll value
  l <- forAll nonemptyList
  let tester = memberInsert1Test val
  test $ unUnrestricted Linear.$ Set.fromList l tester

memberInsert1Test :: Int -> SetTester
memberInsert1Test val set =
  testEqual
    (Unrestricted True)
    (getSnd (Set.member (Set.insert set val) val))

memberInsert2 :: Property
memberInsert2 = property $ do
  val1 <- forAll value
  val2 <- forAll $ Gen.filter (/= val1) value
  l <- forAll nonemptyList
  let tester = memberInsert2Test val1 val2
  test $ unUnrestricted Linear.$ Set.fromList l tester

memberInsert2Test :: Int -> Int -> SetTester
memberInsert2Test val1 val2 set = fromRead (Set.member set val2)
  where
    fromRead :: (Set.Set Int, Bool) #-> Unrestricted (TestT IO ())
    fromRead (set, memberVal2) =
      testEqual
        (move memberVal2)
        (getSnd (Set.member (Set.insert set val1) val2))

memberDelete1 :: Property
memberDelete1 = property $ do
  val <- forAll value
  l <- forAll nonemptyList
  let tester = memberDelete1Test val
  test $ unUnrestricted Linear.$ Set.fromList l tester

memberDelete1Test :: Int -> SetTester
memberDelete1Test val set =
  testEqual
    (Unrestricted False)
    (getSnd (Set.member (Set.delete set val) val))

memberDelete2 :: Property
memberDelete2 = property $ do
  val1 <- forAll value
  val2 <- forAll $ Gen.filter (/= val1) value
  l <- forAll nonemptyList
  let tester = memberDelete2Test val1 val2
  test $ unUnrestricted Linear.$ Set.fromList l tester

memberDelete2Test :: Int -> Int -> SetTester
memberDelete2Test val1 val2 set = fromRead (Set.member set val2)
  where
    fromRead :: (Set.Set Int, Bool) #-> Unrestricted (TestT IO ())
    fromRead (set, memberVal2) =
      testEqual
        (move memberVal2)
        (getSnd Linear.$ Set.member (Set.delete set val1) val2)

sizeSing :: Property
sizeSing = property $ do
  val <- forAll value
  test $ unUnrestricted Linear.$ Set.singleton val sizeSingTest

sizeSingTest :: SetTester
sizeSingTest set = testEqual (Unrestricted 1) (getSnd (Set.size set))

sizeInsert1 :: Property
sizeInsert1 = property $ do
  l <- forAll nonemptyList
  val <- forAll $ Gen.filter (`elem` l) value
  let tester = sizeInsert1Test val
  test $ unUnrestricted Linear.$ Set.fromList l tester

sizeInsert1Test :: Int -> SetTester
sizeInsert1Test val set = fromRead (Set.size set)
  where
    fromRead :: (Set.Set Int, Int) #-> Unrestricted (TestT IO ())
    fromRead (set, sizeOriginal) =
      testEqual
        (move sizeOriginal)
        (getSnd Linear.$ (Set.size (Set.insert set val)))

sizeInsert2 :: Property
sizeInsert2 = property $ do
  l <- forAll nonemptyList
  val <- forAll $ Gen.filter (not . (`elem` l)) value
  let tester = sizeInsert2Test val
  test $ unUnrestricted Linear.$ Set.fromList l tester

sizeInsert2Test :: Int -> SetTester
sizeInsert2Test val set = fromRead (Set.size set)
  where
    fromRead :: (Set.Set Int, Int) #-> Unrestricted (TestT IO ())
    fromRead (set, sizeOriginal) =
      testEqual
        (move (sizeOriginal Linear.+ 1))
        (getSnd Linear.$ (Set.size (Set.insert set val)))

sizeDelete1 :: Property
sizeDelete1 = property $ do
  l <- forAll nonemptyList
  val <- forAll $ Gen.filter (`elem` l) value
  let tester = sizeDelete1Test val
  test $ unUnrestricted Linear.$ Set.fromList l tester

sizeDelete1Test :: Int -> SetTester
sizeDelete1Test val set = fromRead (Set.size set)
  where
    fromRead :: (Set.Set Int, Int) #-> Unrestricted (TestT IO ())
    fromRead (set, sizeOriginal) =
      testEqual
        (move (sizeOriginal Linear.- 1))
        (getSnd Linear.$ (Set.size (Set.delete set val)))

sizeDelete2 :: Property
sizeDelete2 = property $ do
  l <- forAll nonemptyList
  val <- forAll $ Gen.filter (not . (`elem` l)) value
  let tester = sizeDelete2Test val
  test $ unUnrestricted Linear.$ Set.fromList l tester

sizeDelete2Test :: Int -> SetTester
sizeDelete2Test val set = fromRead (Set.size set)
  where
    fromRead :: (Set.Set Int, Int) #-> Unrestricted (TestT IO ())
    fromRead (set, sizeOriginal) =
      testEqual
        (move sizeOriginal)
        (getSnd Linear.$ (Set.size (Set.delete set val)))
