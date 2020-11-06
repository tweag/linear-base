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
import Data.Set.Mutable.Linear (Set)
import Data.Unrestricted.Linear
import Hedgehog
import qualified Data.List as List
import qualified Data.Functor.Linear as Data
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- # Exported Tests
--------------------------------------------------------------------------------

{- How we designed the tests.

We use hedgehog to test properties that are axioms that fully define the
behavior of sets. There is typically one axiom per each combination of
accessor and constructor/mutator. So one defintion for accessor @f@, and
constructor/mutator @g@, we have one axiom of the form @f (g (...)) = ...@.

If however, this is cumbersome, we can often just test against an existing
implementation. This is like a homomorphism test (but not an exact
homomorphism). For unions, this is like saying if we have two lists A and B,
and we take their union as lists, then sort and nub (remove duplicates)
this is the same as using @Set.fromList@ to make them into sets, taking
the union as sets, converting back with @Set.toList@ and then sorting
the output list. Note that this can replace an axiom (though seeing this is
not trivial).

See https://softwarefoundations.cis.upenn.edu/vfa-current/ADT.html
for more about how ADT axioms work.
-}


mutSetTests :: TestTree
mutSetTests = testGroup "Mutable set tests" group

group :: [TestTree]
group =
  -- Tests of the form [accessor (mutator)]
  [ testProperty "∀ x. member (insert s x) x = True" memberInsert1
  , testProperty "∀ x,y/=x. member (insert s x) y = member s y" memberInsert2
  , testProperty "∀ x. member (delete s x) x = False" memberDelete1
  , testProperty "∀ x,y/=x. member (delete s x) y = member s y" memberDelete2
  , testProperty "∀ s, x \\in s. size (insert s x) = size s" sizeInsert1
  , testProperty "∀ s, x \\notin s. size (insert s x) = size s + 1" sizeInsert2
  , testProperty "∀ s, x \\in s. size (delete s x) = size s - 1" sizeDelete1
  , testProperty "∀ s, x \\notin s. size (delete s x) = size s" sizeDelete2
  , testProperty "sort . nub = sort . toList . fromList" toListFromList
  , testProperty
      "sort . nub (l ∪ m) = sort . toList (fromList l ∪ fromList m)"
      unionHomomorphism
  , testProperty
      "sort . nub (l ∩ m) = sort . toList (fromList l ∩ fromList m)"
      intersectHomomorphism
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type SetTester = Set.Set Int %1-> Ur (TestT IO ())

-- | A random list
list :: Gen [Int]
list = do
  size <- Gen.int $ Range.linearFrom 0 0 1000
  let size' = Range.singleton size
  Gen.list size' $ Gen.int $ Range.linearFrom 0 (-100) 100

-- | A random value
value :: Gen Int
value = Gen.int (Range.linear (-100) 100)

testEqual :: (Show a, Eq a) =>
  Ur a %1->
  Ur a %1->
  Ur (TestT IO ())
testEqual (Ur x) (Ur y) = Ur (x === y)

-- XXX: This is a terrible name
getFst :: Consumable b => (a, b) %1-> a
getFst (a, b) = lseq b a

-- # Tests
--------------------------------------------------------------------------------

memberInsert1 :: Property
memberInsert1 = property $ do
  val <- forAll value
  l <- forAll list
  let tester = memberInsert1Test val
  test $ unur Linear.$ Set.fromList l tester

memberInsert1Test :: Int -> SetTester
memberInsert1Test val set =
  testEqual
    (Ur True)
    (getFst (Set.member val (Set.insert val set)))

memberInsert2 :: Property
memberInsert2 = property $ do
  val1 <- forAll value
  val2 <- forAll $ Gen.filter (/= val1) value
  l <- forAll list
  let tester = memberInsert2Test val1 val2
  test $ unur Linear.$ Set.fromList l tester

memberInsert2Test :: Int -> Int -> SetTester
memberInsert2Test val1 val2 set = fromRead (Set.member val2 set)
  where
    fromRead :: (Ur Bool, Set.Set Int) %1-> Ur (TestT IO ())
    fromRead (memberVal2, set) =
      testEqual
        memberVal2
        (getFst (Set.member val2 (Set.insert val1 set)))

memberDelete1 :: Property
memberDelete1 = property $ do
  val <- forAll value
  l <- forAll list
  let tester = memberDelete1Test val
  test $ unur Linear.$ Set.fromList l tester

memberDelete1Test :: Int -> SetTester
memberDelete1Test val set =
  testEqual
    (Ur False)
    (getFst (Set.member val (Set.delete val set)))

memberDelete2 :: Property
memberDelete2 = property $ do
  val1 <- forAll value
  val2 <- forAll $ Gen.filter (/= val1) value
  l <- forAll list
  let tester = memberDelete2Test val1 val2
  test $ unur Linear.$ Set.fromList l tester

memberDelete2Test :: Int -> Int -> SetTester
memberDelete2Test val1 val2 set = fromRead (Set.member val2 set)
  where
    fromRead :: (Ur Bool, Set.Set Int) %1-> Ur (TestT IO ())
    fromRead (memberVal2, set) =
      testEqual
        memberVal2
        (getFst Linear.$ Set.member val2 (Set.delete val1 set))

sizeInsert1 :: Property
sizeInsert1 = property $ do
  l <- forAll list
  val <- forAll $ Gen.filter (`elem` l) value
  let tester = sizeInsert1Test val
  test $ unur Linear.$ Set.fromList l tester

sizeInsert1Test :: Int -> SetTester
sizeInsert1Test val set = fromRead (Set.size set)
  where
    fromRead :: (Ur Int, Set.Set Int) %1-> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        sizeOriginal
        (getFst Linear.$ (Set.size (Set.insert val set)))

sizeInsert2 :: Property
sizeInsert2 = property $ do
  l <- forAll list
  val <- forAll $ Gen.filter (not . (`elem` l)) value
  let tester = sizeInsert2Test val
  test $ unur Linear.$ Set.fromList l tester

sizeInsert2Test :: Int -> SetTester
sizeInsert2Test val set = fromRead (Set.size set)
  where
    fromRead :: (Ur Int, Set.Set Int) %1-> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        ((Linear.+ 1) Data.<$> sizeOriginal)
        (getFst Linear.$ (Set.size (Set.insert val set)))

sizeDelete1 :: Property
sizeDelete1 = property $ do
  l <- forAll list
  val <- forAll $ Gen.filter (`elem` l) value
  let tester = sizeDelete1Test val
  test $ unur Linear.$ Set.fromList l tester

sizeDelete1Test :: Int -> SetTester
sizeDelete1Test val set = fromRead (Set.size set)
  where
    fromRead :: (Ur Int, Set.Set Int) %1-> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        ((Linear.- 1) Data.<$> sizeOriginal)
        (getFst Linear.$ (Set.size (Set.delete val set)))

sizeDelete2 :: Property
sizeDelete2 = property $ do
  l <- forAll list
  val <- forAll $ Gen.filter (not . (`elem` l)) value
  let tester = sizeDelete2Test val
  test $ unur Linear.$ Set.fromList l tester

sizeDelete2Test :: Int -> SetTester
sizeDelete2Test val set = fromRead (Set.size set)
  where
    fromRead :: (Ur Int, Set.Set Int) %1-> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        sizeOriginal
        (getFst Linear.$ (Set.size (Set.delete val set)))

toListFromList :: Property
toListFromList = property $ do
  l <- forAll list
  let outsideSet = List.nub . List.sort $ l
  List.sort (unur (Set.fromList l Set.toList)) === outsideSet

unionHomomorphism :: Property
unionHomomorphism = property $ do
  l <- forAll list
  l' <- forAll list
  let listUnion = List.nub $ List.sort $ List.union l l'
  let setUnion = List.sort $ unur (fromLists l l' doUnion)
  setUnion === listUnion
  where
    fromLists :: [Int] -> [Int] -> (Set Int %1-> Set Int %1-> Ur b) %1-> Ur b
    fromLists l l' f = Set.fromList l (\s -> Set.fromList l' (\s' -> f s s'))

    doUnion :: Set Int %1-> Set Int %1-> Ur [Int]
    doUnion s s' = Set.toList (Set.union s s')

intersectHomomorphism :: Property
intersectHomomorphism = property $ do
  l <- forAll list
  l' <- forAll list
  let listIntersect = List.nub $ List.sort $ List.intersect l l'
  let setIntersect = List.sort $ unur (fromLists l l' doIntersect)
  setIntersect === listIntersect
  where
    fromLists :: [Int] -> [Int] -> (Set Int %1-> Set Int %1-> Ur b) %1-> Ur b
    fromLists l l' f = Set.fromList l (\s -> Set.fromList l' (\s' -> f s s'))

    doIntersect :: Set Int %1-> Set Int %1-> Ur [Int]
    doIntersect s s' = Set.toList (Set.intersection s s')

