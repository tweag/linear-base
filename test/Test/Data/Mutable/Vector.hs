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
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type VectorTester = Vector.Vector Int #-> Unrestricted (TestT IO ())

-- | Given a size, make a random list of that lenght
randomList :: Int -> Gen [Int]
randomList size = do
  let size' = Range.singleton size
  Gen.list size' $ Gen.int $ Range.linearFrom 0 (-1000) 1000

randomSize :: Gen Int
randomSize =  Gen.int (Range.constant 100 1000)

randomVal :: Gen Int
randomVal = Gen.int (Range.linear 1 1000)

-- # I like this name better
unLinear :: Unrestricted a #-> a
unLinear = unUnrestricted

compInts ::
  Unrestricted Int #->
  Unrestricted Int #->
  Unrestricted (TestT IO ())
compInts (Unrestricted x) (Unrestricted y) = Unrestricted (x === y)

-- XXX: This is a terrible name
getSnd :: (Consumable a, Movable b) => (a, b) #-> Unrestricted b
getSnd (a, b) = lseq a (move b)


-- # Tests
--------------------------------------------------------------------------------

-- | Constant should give us a constant vector.
readConst :: Property
readConst = property $ do
  size <- forAll randomSize
  val <- forAll randomVal
  ix <- forAll $ Gen.element [0..size-1]
  test $ unUnrestricted Linear.$ Vector.constant size val (readConstTest ix val)

readConstTest :: Int -> Int -> VectorTester
readConstTest ix val vec = compInts (getSnd (Vector.read vec ix)) (move val)

readWrite1 :: Property
readWrite1 = property $ do
  size <- forAll randomSize
  list <- forAll $ randomList size
  ix <- forAll $ Gen.element [0..size-1]
  val <- forAll randomVal
  let tester = readWrite1Test ix val
  test $ unLinear Linear.$ Vector.fromList list tester

readWrite1Test :: Int -> Int -> VectorTester
readWrite1Test ix val vec =
  compInts (move val) (getSnd Linear.$ Vector.read (Vector.write vec ix val) ix)

readWrite2 :: Property
readWrite2 = property $ do
  size <- forAll randomSize
  list <- forAll $ randomList size
  ix <- forAll $ Gen.element [0..size-1]
  jx <- forAll $ Gen.element [ z | z <- [0..size-1], z /= ix ]
  val <- forAll randomVal
  let tester = readWrite2Test ix jx val
  test $ unLinear Linear.$ Vector.fromList list tester

readWrite2Test :: Int -> Int -> Int -> VectorTester
readWrite2Test ix jx val vec = fromRead (Vector.read vec ix)
  where
    fromRead :: (Vector.Vector Int, Int) #-> Unrestricted (TestT IO ())
    fromRead (vec, val1) =
      compInts
        (move val1)
        (getSnd Linear.$ Vector.read (Vector.write vec jx val) ix)

readSnoc1 :: Property
readSnoc1 = property $ do
  size <- forAll randomSize
  list <- forAll $ randomList size
  val <- forAll randomVal
  ix <- forAll $ Gen.element [0..size-1]
  let tester = readSnoc1Test val ix
  test $ unLinear Linear.$ Vector.fromList list tester

readSnoc1Test :: Int -> Int -> VectorTester
readSnoc1Test val ix vec = fromRead (Vector.read vec ix)
  where
    fromRead :: (Vector.Vector Int, Int) #-> Unrestricted (TestT IO ())
    fromRead (vec,val') =
      compInts (getSnd (Vector.read (Vector.snoc vec val) ix)) (move val')


readSnoc2 :: Property
readSnoc2 = property $ do
  size <- forAll randomSize
  list <- forAll $ randomList size
  val <- forAll randomVal
  let tester = readSnoc2Test val
  test $ unLinear Linear.$ Vector.fromList list tester

readSnoc2Test :: Int -> VectorTester
readSnoc2Test val vec = fromLen (Vector.length vec)
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
  size <- forAll randomSize
  val <- forAll randomVal
  test $ unLinear Linear.$ Vector.constant size val (lenConstTest size)

lenConstTest :: Int -> VectorTester
lenConstTest size vec =
  compInts (move size) (getSnd Linear.$ Vector.length vec)

lenWrite :: Property
lenWrite = property $ do
  size <- forAll randomSize
  list <- forAll $ randomList size
  val <- forAll randomVal
  ix <- forAll $ Gen.element [0..size-1]
  let tester = lenWriteTest size val ix
  test $ unLinear Linear.$ Vector.fromList list tester

lenWriteTest :: Int -> Int -> Int -> VectorTester
lenWriteTest size val ix vec =
  compInts
    (move size)
    (getSnd Linear.$ Vector.length (Vector.write vec ix val))

lenSnoc :: Property
lenSnoc = property $ do
 size <- forAll randomSize
 list <- forAll $ randomList size
 val <- forAll randomVal
 let tester = lenSnocTest val
 test $ unLinear Linear.$ Vector.fromList list tester

lenSnocTest :: Int -> VectorTester
lenSnocTest val vec = fromLen Linear.$ Linear.fmap move (Vector.length vec)
  where
    fromLen ::
      (Vector.Vector Int, Unrestricted Int) #->
      Unrestricted (TestT IO ())
    fromLen (vec, Unrestricted len) =
      compInts (move (len+1)) (getSnd (Vector.length (Vector.snoc vec val)))
