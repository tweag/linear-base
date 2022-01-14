{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable sets.
--
-- = How we designed the tests
--
-- We use hedgehog to test properties that are axioms that funtionally define
-- the behavior of sets. There is at least one axiom per each combination of
-- accessor and constructor/mutator. So for the accessor @f@, and
-- constructor/mutator @g@, we have one axiom of the form @f (g (...)) = ...@.
--
-- If however, this is cumbersome, we can often just test against an existing
-- implementation. This is like a homomorphism test (but it's not quite a
-- homomorphism). For unions, this is like saying if we have two lists A and B,
-- and we take their union as lists, then sort and nub (remove duplicates)
-- this is the same as using @Set.fromList@ to make them into sets, taking
-- the union as sets, converting back with @Set.toList@ and then sorting
-- the output list.
--
-- To have such a test replace an axiom, however, we need to have
-- "homomorphisms" for each accessor or modifier. In general, we want to be
-- able to say for any modifier or accessor @f@,
--
-- >  toList (f set) = f' (toList set)
--
-- where @f'@ is some reference implementation we know. The key idea is a kind
-- of "homomorphism" between @f@ and a reference implementation @f'@ that
-- holds across the conversion function @toList@.
--
-- Note: We could also formulate this in terms of @fromList@.
--
-- For example, we'd want to have @member x (fromList l) = elem x l@
-- for any @x@ and @l@.
--
-- With this we can prove
--
-- >  member x (intersect set1 set2) = member x set1 && member x set2
--
-- Thusly:
--
-- >  member x (intersect set1 set2) =
-- >  member x (intersect (fromList l1) (fromList l2)) =  -- for some l1, l2
-- >  member x (fromList (intersect l1 l2)) =
-- >  elem x (intersect l1 l2)) =
-- >  elem x l1 && elem x l2 =
-- >  member x (fromList l1) && member x (fromList l2) =
-- >  member x set1 && member x set2
--
-- See https://softwarefoundations.cis.upenn.edu/vfa-current/ADT.html
-- for more about how ADT axioms work.
--
-- Remark: we are not testing @empty@ since it is trivial.
module Test.Data.Mutable.Set
  ( mutSetTests,
  )
where

import Data.Containers.ListUtils (nubOrd)
import qualified Data.Functor.Linear as Data
import qualified Data.List as List
import Data.Set.Mutable.Linear (Set)
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
  [ testProperty "∀ x. member (insert s x) x = True" memberInsert1,
    testProperty "∀ x,y/=x. member (insert s x) y = member s y" memberInsert2,
    testProperty "∀ x. member (delete s x) x = False" memberDelete1,
    testProperty "∀ x,y/=x. member (delete s x) y = member s y" memberDelete2,
    testProperty "∀ s, x \\in s. size (insert s x) = size s" sizeInsert1,
    testProperty "∀ s, x \\notin s. size (insert s x) = size s + 1" sizeInsert2,
    testProperty "∀ s, x \\in s. size (delete s x) = size s - 1" sizeDelete1,
    testProperty "∀ s, x \\notin s. size (delete s x) = size s" sizeDelete2,
    -- Homomorphism tests
    testProperty "sort . nub = sort . toList" toListFromList,
    testProperty "member x s = elem x (toList s)" memberHomomorphism,
    testProperty "size = length . toList" sizeHomomorphism,
    testProperty
      "sort . nub ((toList s) ∪ (toList s')) = sort . toList (s ∪ s')"
      unionHomomorphism,
    testProperty
      "sort . nub ((toList s) ∩ (toList s')) = sort . toList (s ∩ s')"
      intersectHomomorphism
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type SetTester = Set.Set Int %1 -> Ur (TestT IO ())

-- | A random list
list :: Gen [Int]
list = do
  size <- Gen.int $ Range.linearFrom 0 0 1000
  let size' = Range.singleton size
  Gen.list size' $ Gen.int $ Range.linearFrom 0 (-100) 100

-- | A random value
value :: Gen Int
value = Gen.int (Range.linear (-100) 100)

testEqual ::
  (Show a, Eq a) =>
  Ur a %1 ->
  Ur a %1 ->
  Ur (TestT IO ())
testEqual (Ur x) (Ur y) = Ur (x === y)

-- XXX: This is a terrible name
getFst :: Consumable b => (a, b) %1 -> a
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
    fromRead :: (Ur Bool, Set.Set Int) %1 -> Ur (TestT IO ())
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
    fromRead :: (Ur Bool, Set.Set Int) %1 -> Ur (TestT IO ())
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
    fromRead :: (Ur Int, Set.Set Int) %1 -> Ur (TestT IO ())
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
    fromRead :: (Ur Int, Set.Set Int) %1 -> Ur (TestT IO ())
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
    fromRead :: (Ur Int, Set.Set Int) %1 -> Ur (TestT IO ())
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
    fromRead :: (Ur Int, Set.Set Int) %1 -> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        sizeOriginal
        (getFst Linear.$ (Set.size (Set.delete val set)))

toListFromList :: Property
toListFromList = property $ do
  l <- forAll list
  let outsideSet = nubOrd . List.sort $ l
  List.sort (unur (Set.fromList l Set.toList)) === outsideSet

unionHomomorphism :: Property
unionHomomorphism = property $ do
  l <- forAll list
  l' <- forAll list
  let listUnion = nubOrd $ List.sort $ List.union l l'
  let setUnion = List.sort $ unur (fromLists l l' doUnion)
  setUnion === listUnion
  where
    fromLists :: [Int] -> [Int] -> (Set Int %1 -> Set Int %1 -> Ur b) %1 -> Ur b
    fromLists l l' f = Set.fromList l (\s -> Set.fromList l' (\s' -> f s s'))

    doUnion :: Set Int %1 -> Set Int %1 -> Ur [Int]
    doUnion s s' = Set.toList (Set.union s s')

intersectHomomorphism :: Property
intersectHomomorphism = property $ do
  l <- forAll list
  l' <- forAll list
  let listIntersect = nubOrd $ List.sort $ List.intersect l l'
  let setIntersect = List.sort $ unur (fromLists l l' doIntersect)
  setIntersect === listIntersect
  where
    fromLists :: [Int] -> [Int] -> (Set Int %1 -> Set Int %1 -> Ur b) %1 -> Ur b
    fromLists l l' f = Set.fromList l (\s -> Set.fromList l' (\s' -> f s s'))

    doIntersect :: Set Int %1 -> Set Int %1 -> Ur [Int]
    doIntersect s s' = Set.toList (Set.intersection s s')

memberHomomorphism :: Property
memberHomomorphism = property $ do
  l <- forAll list
  x <- forAll value
  elem x l === (unur Linear.$ Set.fromList l (getFst Linear.. Set.member x))

sizeHomomorphism :: Property
sizeHomomorphism = property $ do
  l <- forAll list
  length (nubOrd l) === (unur (Set.fromList l (getFst Linear.. Set.size)))
