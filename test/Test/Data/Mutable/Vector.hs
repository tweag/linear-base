{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable vectors.
--
-- TODO:
--  * Test failures for out of bound access
--  * Constructor - constructor rules, like
--                write (write v i x) i y = write v i y
module Test.Data.Mutable.Vector
  ( mutVecTests,
  )
where

import qualified Data.Vector.Mutable.Linear as Vector
import qualified Data.Functor.Linear as Linear
import Data.Unrestricted.Linear
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- # Exported Tests
--------------------------------------------------------------------------------

mutVecTests :: TestTree
mutVecTests = testGroup "Mutable vector tests" group

group :: [TestTree]
group =
  -- All tests for exprs of the form (read (const ...) i)
  [ testProperty "∀ s,i,x. read (constant s x) i = x" readConst
  , testProperty "∀ a,i,x. read (write a i x) i = x " readWrite1
  , testProperty "∀ a,i,j/=i,x. read (write a j x) i = read a i" readWrite2
  , testProperty "∀ a,x,(i < len a). read (snoc a x) i = read a i" readSnoc1
  , testProperty "∀ a,x. read (snoc a x) (len a) = x" readSnoc2
  -- All tests for exprs of the form (length (const ...))
  , testProperty "∀ s,x. len (constant s x) = s" lenConst
  , testProperty "∀ a,i,x. len (write a i x) = len a" lenWrite
  , testProperty "∀ a,x. len (snoc a x) = 1 + len a" lenSnoc
  -- Regression tests
  , testProperty "snoc on an empty vector should succeed" snocOnEmptyVector
  , testProperty "do not reorder reads and writes" readAndWriteTest
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type VectorTester = Vector.Vector Int #-> Unrestricted (TestT IO ())

nonEmptyList :: Gen [Int]
nonEmptyList = Gen.list (Range.linear 1 1000) val

list :: Gen [Int]
list = Gen.list (Range.linear 0 1000) val

val :: Gen Int
val = Gen.int (Range.linear (-1000) 1000)

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

snocOnEmptyVector :: Property
snocOnEmptyVector = withTests 1 . property $ do
  let Unrestricted actual =
        Vector.empty
          Linear.$ \vec -> Vector.snoc vec (42 :: Int)
          Linear.& \vec -> Vector.read vec 0
          Linear.& getSnd
  actual === 42

-- | Constant should give us a constant vector.
readConst :: Property
readConst = property $ do
  size <- forAll $ Gen.int $ Range.linear 1 1000
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  test $ unUnrestricted Linear.$ Vector.constant size v (readConstTest ix v)

readConstTest :: Int -> Int -> VectorTester
readConstTest ix val vec = compInts (getSnd (Vector.read vec ix)) (move val)

readWrite1 :: Property
readWrite1 = property $ do
  l <- forAll nonEmptyList
  let size = length l
  ix <- forAll $ Gen.element [0..size-1]
  v <- forAll val
  let tester = readWrite1Test ix v
  test $ unUnrestricted Linear.$ Vector.fromList l tester

readWrite1Test :: Int -> Int -> VectorTester
readWrite1Test ix val vec =
  compInts (move val) (getSnd Linear.$ Vector.read (Vector.write vec ix val) ix)

readWrite2 :: Property
readWrite2 = property $ do
  let list = Gen.list (Range.linearFrom 2 2 1000) val
  l <- forAll list
  let size = length l
  ix <- forAll $ Gen.element [0..size-1]
  jx <- forAll $ Gen.element [ z | z <- [0..size-1], z /= ix ]
  v <- forAll val
  let tester = readWrite2Test ix jx v
  test $ unUnrestricted Linear.$ Vector.fromList l tester

readWrite2Test :: Int -> Int -> Int -> VectorTester
readWrite2Test ix jx val vec = fromRead (Vector.read vec ix)
  where
    fromRead :: (Vector.Vector Int, Unrestricted Int) #-> Unrestricted (TestT IO ())
    fromRead (vec, val1) =
      compInts
        val1
        (getSnd Linear.$ Vector.read (Vector.write vec jx val) ix)

readSnoc1 :: Property
readSnoc1 = property $ do
  l <- forAll nonEmptyList
  let size = length l
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  let tester = readSnoc1Test v ix
  test $ unUnrestricted Linear.$ Vector.fromList l tester

readSnoc1Test :: Int -> Int -> VectorTester
readSnoc1Test val ix vec = fromRead (Vector.read vec ix)
  where
    fromRead :: (Vector.Vector Int, Unrestricted Int) #-> Unrestricted (TestT IO ())
    fromRead (vec,val') =
      compInts (getSnd (Vector.read (Vector.snoc vec val) ix)) val'


readSnoc2 :: Property
readSnoc2 = property $ do
  l <- forAll list
  v <- forAll val
  let tester = readSnoc2Test v
  test $ unUnrestricted Linear.$ Vector.fromList l tester

readSnoc2Test :: Int -> VectorTester
readSnoc2Test val vec = fromLen (Vector.size vec)
  where
    fromLen :: (Vector.Vector Int, Int) #-> Unrestricted (TestT IO ())
    fromLen (vec,len) = fromLen' (vec, move len)

    fromLen' ::
      (Vector.Vector Int, Unrestricted Int) #->
      Unrestricted (TestT IO ())
    fromLen' (vec, Unrestricted len) =
      compInts (getSnd (Vector.read (Vector.snoc vec val) len)) (move val)

lenConst :: Property
lenConst = property $ do
  size <- forAll $ Gen.int $ Range.linear 1 1000
  v <- forAll val
  test $ unUnrestricted Linear.$ Vector.constant size v (lenConstTest size)

lenConstTest :: Int -> VectorTester
lenConstTest size vec =
  compInts (move size) (move Linear.. getSnd Linear.$ Vector.size vec)

lenWrite :: Property
lenWrite = property $ do
  l <- forAll nonEmptyList
  let size = length l
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  let tester = lenWriteTest size v ix
  test $ unUnrestricted Linear.$ Vector.fromList l tester

lenWriteTest :: Int -> Int -> Int -> VectorTester
lenWriteTest size val ix vec =
  compInts
    (move size)
    (move Linear.. getSnd Linear.$ Vector.size (Vector.write vec ix val))

lenSnoc :: Property
lenSnoc = property $ do
 l <- forAll list
 v <- forAll val
 let tester = lenSnocTest v
 test $ unUnrestricted Linear.$ Vector.fromList l tester

lenSnocTest :: Int -> VectorTester
lenSnocTest val vec = fromLen Linear.$ Linear.fmap move (Vector.size vec)
  where
    fromLen ::
      (Vector.Vector Int, Unrestricted Int) #->
      Unrestricted (TestT IO ())
    fromLen (vec, Unrestricted len) =
      compInts (move (len+1)) (move (getSnd (Vector.size (Vector.snoc vec val))))

-- https://github.com/tweag/linear-base/pull/135
readAndWriteTest :: Property
readAndWriteTest = withTests 1 . property $
  unUnrestricted (Vector.fromList "a" test) === 'a'
  where
    test :: Vector.Vector Char #-> Unrestricted Char
    test vec =
      Vector.read vec 0 Linear.& \(vec', before) ->
        Vector.write vec' 0 'b' Linear.& \vec'' ->
          vec'' `Linear.lseq` before
