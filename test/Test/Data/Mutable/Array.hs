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
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear
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
  , testProperty "∀ a,i,x. read (write a i x) i = x " readWrite1
  , testProperty "∀ a,i,j/=i,x. read (write a j x) i = read a i" readWrite2
  -- All tests for exprs of the form (length (const ...))
  , testProperty "∀ s,x. len (alloc s x) = s" lenAlloc
  , testProperty "∀ a,i,x. len (write a i x) = len a" lenWrite
  , testProperty "∀ a,s. len (growBy s a) = len a + s" lenGrowBy
  , testProperty "∀ a,s. len (shrinkTo s a) = len s" lenShrinkTo
  -- Tests against a reference implementation
  , testProperty "growBy" growByReference
  , testProperty "shrinkTo" shrinkToReference
  -- Regression tests
  , testProperty "do not reorder reads and writes" readAndWriteTest
  , testProperty "do not evaluate values unnecesesarily" strictnessTest
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type ArrayTester = Array.Array Int #-> Unrestricted (TestT IO ())

-- | A random list
list :: Gen [Int]
list = do
  size <- Gen.int $ Range.linearFrom 0 1 1000
  let size' = Range.singleton size
  Gen.list size' $ Gen.int $ Range.linearFrom 0 (-1000) 1000

-- | A random value
value :: Gen Int
value = Gen.int (Range.linear (-1000) 1000)

compInts ::
  Unrestricted Int #->
  Unrestricted Int #->
  Unrestricted (TestT IO ())
compInts (Unrestricted x) (Unrestricted y) = Unrestricted (x === y)

-- XXX: This is a terrible name
getSnd :: Consumable a => (a, b) #-> b
getSnd (a, b) = lseq a b


-- # Tests
--------------------------------------------------------------------------------

readAlloc :: Property
readAlloc = property $ do
  size <- forAll $ Gen.int $ Range.linearFrom 1 1 1000
  val <- forAll value
  ix <- forAll $ Gen.element [0..size-1]
  test $ unUnrestricted Linear.$ Array.alloc size val (readAllocTest ix val)

readAllocTest :: Int -> Int -> ArrayTester
readAllocTest ix val arr = compInts (getSnd (Array.read ix arr)) (move val)

readWrite1 :: Property
readWrite1 = property $ do
  l <- forAll list
  let size = length l
  ix <- forAll $ Gen.element [0..size-1]
  val <- forAll value
  let tester = readWrite1Test ix val
  test $ unUnrestricted Linear.$ Array.fromList l tester

readWrite1Test :: Int -> Int -> ArrayTester
readWrite1Test ix val arr =
  compInts (move val) (getSnd Linear.$ Array.read ix (Array.write ix val arr))

readWrite2 :: Property
readWrite2 = property $ do
  let list = Gen.list (Range.linearFrom 2 2 1000) value
  l <- forAll list
  let size = length l
  ix <- forAll $ Gen.element [0..size-1]
  jx <- forAll $ Gen.element [ z | z <- [0..size-1], z /= ix ]
  val <- forAll value
  let tester = readWrite2Test ix jx val
  test $ unUnrestricted Linear.$ Array.fromList l tester

readWrite2Test :: Int -> Int -> Int -> ArrayTester
readWrite2Test ix jx val arr = fromRead (Array.read ix arr)
  where
    fromRead ::
      (Array.Array Int, Unrestricted Int) #-> Unrestricted (TestT IO ())
    fromRead (arr, val1) =
      compInts
        val1
        (getSnd Linear.$ Array.read ix (Array.write jx val arr))

lenAlloc :: Property
lenAlloc = property $ do
  size <- forAll $ Gen.int $ Range.linearFrom 1 1 1000
  val <- forAll value
  test $ unUnrestricted Linear.$ Array.alloc size val (lenAllocTest size)

lenAllocTest :: Int -> ArrayTester
lenAllocTest size arr =
  compInts (move size) (getSnd Linear.$ Array.size arr)

lenWrite :: Property
lenWrite = property $ do
  l <- forAll list
  let size = length l
  val <- forAll value
  ix <- forAll $ Gen.element [0..size-1]
  let tester = lenWriteTest size val ix
  test $ unUnrestricted Linear.$ Array.fromList l tester

lenWriteTest :: Int -> Int -> Int -> ArrayTester
lenWriteTest size val ix arr =
  compInts (move size)
    (getSnd Linear.$ Array.size (Array.write ix val arr))

lenGrowBy :: Property
lenGrowBy = property $ do
  l <- forAll list
  let initialSize = length l
  growBy <- forAll $ Gen.element [0..4]
  test . Linear.forget unUnrestricted . Array.fromList l $ \arr ->
    arr
      Linear.& Array.growBy growBy 0
      Linear.& Array.size
      Linear.& getSnd
      Linear.& \(Unrestricted newSize) ->
        Unrestricted $ newSize === initialSize + growBy

lenShrinkTo :: Property
lenShrinkTo = property $ do
  l <- forAll list
  let initialSize = length l
  targetSize <- forAll $ Gen.int (Range.linear 0 initialSize)
  test . Linear.forget unUnrestricted . Array.fromList l $ \arr ->
    arr
      Linear.& Array.shrinkTo targetSize
      Linear.& Array.size
      Linear.& getSnd
      Linear.& \(Unrestricted newSize) ->
        Unrestricted $ newSize === targetSize

growByReference :: Property
growByReference = property $ do
  l <- forAll list
  growth <- forAll $ Gen.int (Range.linear 0 100)

  let expected = l ++ replicate growth 42

  let Unrestricted actual = Array.fromList l $ \arr ->
        arr
          Linear.& Array.growBy growth 42
          Linear.& Array.toList
          Linear.& getSnd

  expected ===  actual

shrinkToReference :: Property
shrinkToReference = property $ do
  l <- forAll list
  target <- forAll $ Gen.int (Range.linear 0 (length l))

  let expected = take target l

  let Unrestricted actual = Array.fromList l $ \arr ->
        arr
          Linear.& Array.shrinkTo target
          Linear.& Array.toList
          Linear.& getSnd

  expected ===  actual

-- https://github.com/tweag/linear-base/pull/135
readAndWriteTest :: Property
readAndWriteTest = withTests 1 . property $
  unUnrestricted (Array.fromList "a" test) === 'a'
  where
    test :: Array.Array Char #-> Unrestricted Char
    test arr =
      Array.read 0 arr Linear.& \(arr', before) ->
        Array.write 0 'b' arr' Linear.& \arr'' ->
          arr'' `Linear.lseq` before

-- https://github.com/tweag/linear-base/issues/142
strictnessTest :: Property
strictnessTest = withTests 1 . property $
  unUnrestricted (Array.fromList [()] test) === ()
  where
    test :: Array.Array () #-> Unrestricted ()
    test arr =
      Array.write 0 (error "this should not be evaluated") arr
        Linear.& Array.read 0
        Linear.& \(arr, Unrestricted _) -> arr `Linear.lseq` Unrestricted ()
