{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable arrays.
--
-- See the testing framework explained in Test.Data.Mutable.Set.
--
-- The combiniation of axioms and homormorphisms provided fully specify
-- the behavior of arrays.
--
-- Remarks:
--  * We don't test for failure on out-of-bound access (it's trivial)
module Test.Data.Mutable.Array
  ( mutArrTests,
  )
where

import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import qualified Data.Functor.Linear as Data
import qualified Data.Ord.Linear as Linear
import Hedgehog
import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear hiding ((>))
import qualified Data.Vector as Vector
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- # Exported Tests
--------------------------------------------------------------------------------

mutArrTests :: TestTree
mutArrTests = testGroup "Mutable array tests" group

group :: [TestTree]
group =
  -- All tests for exprs of the form (read (const ...) i)
  [ testProperty "∀ s,i,x. read (alloc s x) i = x" readAlloc
  , testProperty "∀ a,s,x,i. read (snd (allocBeside s x a)) i = x" allocBeside
  , testProperty "∀ s,a,i. i < length a, read (resize s 42 a) i = read a i" readResize
  , testProperty "∀ a,i,x. read (write a i x) i = x " readWrite1
  , testProperty "∀ a,i,j/=i,x. read (write a j x) i = read a i" readWrite2
  -- All tests for exprs of the form (length (const ...))
  , testProperty "∀ s,x. len (alloc s x) = s" lenAlloc
  , testProperty "∀ a,i,x. len (write a i x) = len a" lenWrite
  , testProperty "∀ a,s,x. len (resize s x a) = s" lenResizeSeed
  -- Tests against a reference implementation
  , testProperty "∀ a,ix. write a ix . fromList = fromList . write a ix" writeRef
  , testProperty "∀ ix. read ix (fromList l) = l !! i" readRef
  , testProperty "size . fromList = length" sizeRef
  , testProperty "∀ a,s,x. resize s x a = take s (toList a ++ repeat x)" resizeRef
  , testProperty "∀ s,n. slice s n = take s . drop n" sliceRef
  , testProperty "f <$> fromList xs == fromList (f <$> xs)" refFmap
  , testProperty "toList . fromList = id" refToListFromList
  , testProperty "toList . freeze . fromList = id" refFreeze
  , testProperty "dup2 produces identical arrays" refDupable
  -- Regression tests
  , testProperty "do not reorder reads and writes" readAndWriteTest
  , testProperty "do not evaluate values unnecesesarily" strictnessTest
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type ArrayTester = Array.Array Int %1-> Ur (TestT IO ())

nonEmptyList :: Gen [Int]
nonEmptyList = Gen.list (Range.linear 1 1000) value

list :: Gen [Int]
list = Gen.list (Range.linear 0 1000) value

-- | A random value
value :: Gen Int
value = Gen.int (Range.linear (-1000) 1000)

compInts ::
  Ur Int %1->
  Ur Int %1->
  Ur (TestT IO ())
compInts (Ur x) (Ur y) = Ur (x === y)

-- XXX: This is a terrible name
getFst :: Consumable b => (a, b) %1-> a
getFst (a, b) = lseq b a


-- # Tests
--------------------------------------------------------------------------------

readAlloc :: Property
readAlloc = property $ do
  size <- forAll $ Gen.int $ Range.linear 1 1000
  val <- forAll value
  ix <- forAll $ Gen.element [0..size-1]
  test $ unur Linear.$ Array.alloc size val (readAllocTest ix val)

readAllocTest :: Int -> Int -> ArrayTester
readAllocTest ix val arr = compInts (getFst (Array.read arr ix)) (move val)

readResize :: Property
readResize = property $ do
  l <- forAll nonEmptyList
  let size = length l
  newSize <- forAll $ Gen.element [1..(size*4)]
  ix <- forAll $ Gen.element [0..(min size newSize)-1]
  let tester = readResizeTest newSize ix
  test $ unur Linear.$ Array.fromList l tester

readResizeTest :: Int -> Int -> ArrayTester
readResizeTest size ix arr =
  Array.read arr ix
    Linear.& \(Ur old, arr) -> Array.resize size 42 arr
    Linear.& \arr -> Array.read arr ix
    Linear.& getFst
    Linear.& \(Ur new) -> Ur (old === new)

readWrite1 :: Property
readWrite1 = property $ do
  l <- forAll nonEmptyList
  let size = length l
  ix <- forAll $ Gen.element [0..size-1]
  val <- forAll value
  let tester = readWrite1Test ix val
  test $ unur Linear.$ Array.fromList l tester

readWrite1Test :: Int -> Int -> ArrayTester
readWrite1Test ix val arr =
  compInts (move val) (getFst Linear.$ Array.read (Array.write arr ix val) ix)

readWrite2 :: Property
readWrite2 = property $ do
  let list = Gen.list (Range.linearFrom 2 2 1000) value
  l <- forAll list
  let size = length l
  ix <- forAll $ Gen.element [0..size-1]
  jx <- forAll $ Gen.element [ z | z <- [0..size-1], z /= ix ]
  val <- forAll value
  let tester = readWrite2Test ix jx val
  test $ unur Linear.$ Array.fromList l tester

readWrite2Test :: Int -> Int -> Int -> ArrayTester
readWrite2Test ix jx val arr = fromRead (Array.read arr ix)
  where
    fromRead ::
      (Ur Int, Array.Array Int) %1-> Ur (TestT IO ())
    fromRead (val1, arr) =
      compInts
        val1
        (getFst Linear.$ Array.read (Array.write arr jx val) ix)

allocBeside :: Property
allocBeside = property $ do
  l <- forAll nonEmptyList
  let size = length l
  newSize <- forAll $ Gen.element [size..(size*4)]
  val <- forAll value
  ix <- forAll $ Gen.element [0..newSize-1]
  let tester = allocBesideTest newSize val ix
  test $ unur Linear.$ Array.fromList l tester

allocBesideTest :: Int -> Int -> Int -> ArrayTester
allocBesideTest newSize val ix arr =
  Array.allocBeside newSize val arr
    Linear.& getFst
    Linear.& \arr -> Array.read arr ix
    Linear.& getFst
    Linear.& compInts (move val)

lenAlloc :: Property
lenAlloc = property $ do
  size <- forAll $ Gen.int $ Range.linear 0 1000
  val <- forAll value
  test $ unur Linear.$ Array.alloc size val (lenAllocTest size)

lenAllocTest :: Int -> ArrayTester
lenAllocTest size arr =
  compInts (move size) (getFst Linear.$ Array.size arr)

lenWrite :: Property
lenWrite = property $ do
  l <- forAll nonEmptyList
  let size = length l
  val <- forAll value
  ix <- forAll $ Gen.element [0..size-1]
  let tester = lenWriteTest size val ix
  test $ unur Linear.$ Array.fromList l tester

lenWriteTest :: Int -> Int -> Int -> ArrayTester
lenWriteTest size val ix arr =
  compInts (move size)
    (getFst Linear.$ Array.size (Array.write arr ix val))

lenResizeSeed :: Property
lenResizeSeed = property $ do
  l <- forAll list
  let size = length l
  val <- forAll value
  newSize <- forAll $ Gen.element [size..(size*4)]
  let tester = lenResizeSeedTest newSize val
  test $ unur Linear.$ Array.fromList l tester

lenResizeSeedTest :: Int -> Int -> ArrayTester
lenResizeSeedTest newSize val arr =
  compInts
    (move newSize)
    (getFst Linear.$ Array.size (Array.resize newSize val arr))

writeRef :: Property
writeRef = property $ do
  l <- forAll nonEmptyList
  v <- forAll value
  ix <- forAll $ Gen.int $ Range.linear 0 (List.length l - 1)
  let l' = List.take ix l ++ [v] ++ List.drop (ix+1) l
  l' === unur (Array.fromList l (Array.toList Linear.. Array.set ix v))

readRef :: Property
readRef = property $ do
  l <- forAll nonEmptyList
  ix <- forAll $ Gen.int $ Range.linear 0 (length l - 1)
  (l List.!! ix) === (unur (Array.fromList l (getFst Linear.. Array.get ix)))

sizeRef :: Property
sizeRef = property $ do
  l <- forAll list
  length l === (unur (Array.fromList l (getFst Linear.. Array.size)))

resizeRef :: Property
resizeRef = property $ do
  l <- forAll list
  n <- forAll $ Gen.int (Range.linear 0 (length l * 2))
  x <- forAll value
  let expected = take n $ l ++ repeat x
      actual =
        unur Linear.. Array.fromList l Linear.$ \arr ->
          Array.resize n x arr
            Linear.& Array.toList
  actual === expected

refToListFromList :: Property
refToListFromList = property $ do
  xs <- forAll list
  let Ur actual = Array.fromList xs Array.toList
  xs === actual

sliceRef :: Property
sliceRef = property $ do
  xs <- forAll list
  s <- forAll $ Gen.int (Range.linear 0 (length xs))
  n <- forAll $ Gen.int (Range.linear 0 (length xs - s))
  let expected = take n . drop s $ xs
      Ur actual =
        Array.fromList xs Linear.$ \arr ->
          Array.slice s n arr
            Linear.& \(old, new) ->
                       old `lseq` Array.toList new
  expected === actual

refFmap :: Property
refFmap = property $ do
  xs <- forAll list
  let -- An arbitrary function
      f :: Int %1-> Bool
      f = (Linear.> 0)
      expected = map (Linear.forget f) xs
      Ur actual =
        Array.fromList xs Linear.$ \arr ->
          Array.toList (f Data.<$> arr)
  expected === actual

refFreeze :: Property
refFreeze = property $ do
  xs <- forAll list
  let Ur vec = Array.fromList xs Array.freeze
  xs === Vector.toList vec

refDupable :: Property
refDupable = property $ do
  xs <- forAll list
  let Ur (r1, r2) =
        Array.fromList xs Linear.$ \arr ->
          dup2 arr Linear.& \(arr1, arr2) ->
            Array.toList arr1 Linear.& \(Ur l1) ->
              Array.toList arr2 Linear.& \(Ur l2) ->
                Ur (l1, l2)
  xs === r1
  xs === r2

-- https://github.com/tweag/linear-base/pull/135
readAndWriteTest :: Property
readAndWriteTest = withTests 1 . property $
  unur (Array.fromList "a" test) === 'a'
  where
    test :: Array.Array Char %1-> Ur Char
    test arr =
      Array.read arr 0 Linear.& \(before, arr') ->
        Array.write arr' 0 'b' Linear.& \arr'' ->
          arr'' `Linear.lseq` before

-- https://github.com/tweag/linear-base/issues/142
strictnessTest :: Property
strictnessTest = withTests 1 . property $
  unur (Array.fromList [()] test) === ()
  where
    test :: Array.Array () %1-> Ur ()
    test arr =
      Array.write arr 0 (error "this should not be evaluated") Linear.& \arr ->
      Array.read arr 0 Linear.& \(Ur _, arr) ->
        arr `Linear.lseq` Ur ()
