{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable arrays.
--
-- The API is basically 4 mutators or constructors: alloc, write, resize,
-- resizeSeed and 2 accessors: length, read.
--
-- We test arrays behave properly with properties that axiomatically define
-- arrays.  Basically, using these properties, the behavior of any use of
-- arrays should be fully specified without having to look at the
-- implementation.
--
-- An example of such a property would be the following:
--
-- for all i, j, arr, s.t. i /= j and 0 <= i,j < (length arr),
--             read (write arr j x) i == read arr i
--
-- In general, most uses of arrays are of the form
-- \[  accessor (constructor (...)) \].
--
-- Hence, we define properties that allow you to "simplyfy" or "rewrite"
-- any such combination into a simpler form. (More formally,
-- we're defining term re-write rules.) For now, constructor-constructor
-- re-writes are skipped (e.g., write (write a i x) i y = write a i y).
--
-- TODO:
--  * Test failures for out of bound access
--  * Constructor - constructor rules, like
--                write (write a i x) i y = write a i y
module Test.Data.Mutable.Array
  ( mutArrTests,
  )
where

import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import qualified Data.Functor.Linear as Data
import qualified Data.Ord.Linear as Linear
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear hiding ((>))
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
  , testProperty "∀ a,s,x. resize s x a = take s (toList a ++ repeat x)" resizeRef
  , testProperty "∀ s,n. slice s n = take s . drop n" sliceRef
  , testProperty "f <$> fromList xs == fromList (f <$> xs)" refFmap
  , testProperty "toList . fromList = id" refToListFromList
  -- Regression tests
  , testProperty "do not reorder reads and writes" readAndWriteTest
  , testProperty "do not evaluate values unnecesesarily" strictnessTest
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type ArrayTester = Array.Array Int #-> Ur (TestT IO ())

nonEmptyList :: Gen [Int]
nonEmptyList = Gen.list (Range.linear 1 1000) value

list :: Gen [Int]
list = Gen.list (Range.linear 0 1000) value

-- | A random value
value :: Gen Int
value = Gen.int (Range.linear (-1000) 1000)

compInts ::
  Ur Int #->
  Ur Int #->
  Ur (TestT IO ())
compInts (Ur x) (Ur y) = Ur (x === y)

-- XXX: This is a terrible name
getSnd :: Consumable a => (a, b) #-> b
getSnd (a, b) = lseq a b


-- # Tests
--------------------------------------------------------------------------------

readAlloc :: Property
readAlloc = property $ do
  size <- forAll $ Gen.int $ Range.linear 1 1000
  val <- forAll value
  ix <- forAll $ Gen.element [0..size-1]
  test $ unur Linear.$ Array.alloc size val (readAllocTest ix val)

readAllocTest :: Int -> Int -> ArrayTester
readAllocTest ix val arr = compInts (getSnd (Array.read arr ix)) (move val)

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
    Linear.& \(arr, Ur old) -> Array.resize size 42 arr
    Linear.& \arr -> Array.read arr ix
    Linear.& getSnd
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
  compInts (move val) (getSnd Linear.$ Array.read (Array.write arr ix val) ix)

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
      (Array.Array Int, Ur Int) #-> Ur (TestT IO ())
    fromRead (arr, val1) =
      compInts
        val1
        (getSnd Linear.$ Array.read (Array.write arr jx val) ix)

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
    Linear.& getSnd
    Linear.& \arr -> Array.read arr ix
    Linear.& getSnd
    Linear.& compInts (move val)

lenAlloc :: Property
lenAlloc = property $ do
  size <- forAll $ Gen.int $ Range.linear 0 1000
  val <- forAll value
  test $ unur Linear.$ Array.alloc size val (lenAllocTest size)

lenAllocTest :: Int -> ArrayTester
lenAllocTest size arr =
  compInts (move size) (getSnd Linear.$ Array.size arr)

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
    (getSnd Linear.$ Array.size (Array.write arr ix val))

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
    (getSnd Linear.$ Array.size (Array.resize newSize val arr))

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
      f :: Int #-> Bool
      f = (Linear.> 0)
      expected = map (Linear.forget f) xs
      Ur actual =
        Array.fromList xs Linear.$ \arr ->
          Array.toList (f Data.<$> arr)
  expected === actual

-- https://github.com/tweag/linear-base/pull/135
readAndWriteTest :: Property
readAndWriteTest = withTests 1 . property $
  unur (Array.fromList "a" test) === 'a'
  where
    test :: Array.Array Char #-> Ur Char
    test arr =
      Array.read arr 0 Linear.& \(arr', before) ->
        Array.write arr' 0 'b' Linear.& \arr'' ->
          arr'' `Linear.lseq` before

-- https://github.com/tweag/linear-base/issues/142
strictnessTest :: Property
strictnessTest = withTests 1 . property $
  unur (Array.fromList [()] test) === ()
  where
    test :: Array.Array () #-> Ur ()
    test arr =
      Array.write arr 0 (error "this should not be evaluated") Linear.& \arr ->
      Array.read arr 0 Linear.& \(arr, Ur _) ->
        arr `Linear.lseq` Ur ()
