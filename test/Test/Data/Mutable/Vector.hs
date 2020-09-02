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
  , testProperty "do not reorder reads and writes" readAndWriteTest
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type VectorTester = Vector.Vector Int #-> Unrestricted (TestT IO ())

-- | Given a size, make a random list of that lenght
list :: Gen [Int]
list = do
  size <- Gen.int $ Range.linearFrom 1 1 1000
  let size' = Range.singleton size
  Gen.list size' $ Gen.int $ Range.linearFrom 0 (-1000) 1000

val :: Gen Int
val = Gen.int (Range.linear (-1000) 1000)

compInts ::
  Unrestricted Int #->
  Unrestricted Int #->
  Unrestricted (TestT IO ())
compInts (Unrestricted x) (Unrestricted y) = Unrestricted (x === y)

-- XXX: This is a terrible name
getSnd :: (Consumable a) => (a, b) #-> b
getSnd (a, b) = lseq a b


-- # Tests
--------------------------------------------------------------------------------

-- | Constant should give us a constant vector.
readConst :: Property
readConst = property $ do
  size <- forAll $ Gen.int $ Range.linearFrom 1 1 1000
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  test $ unUnrestricted Linear.$ Vector.constant size v (readConstTest ix v)

readConstTest :: Int -> Int -> VectorTester
readConstTest ix val vec = compInts (getSnd (Vector.read ix vec)) (move val)

readWrite1 :: Property
readWrite1 = property $ do
  l <- forAll list
  let size = length l
  ix <- forAll $ Gen.element [0..size-1]
  v <- forAll val
  let tester = readWrite1Test ix v
  test $ unUnrestricted Linear.$ Vector.fromList l tester

readWrite1Test :: Int -> Int -> VectorTester
readWrite1Test ix val vec =
  compInts (move val) (getSnd Linear.$ Vector.read ix (Vector.write ix val vec))

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
readWrite2Test ix jx val vec = fromRead (Vector.read ix vec)
  where
    fromRead :: (Vector.Vector Int, Unrestricted Int) #-> Unrestricted (TestT IO ())
    fromRead (vec, val1) =
      compInts
        val1
        (getSnd Linear.$ Vector.read ix (Vector.write jx val vec))

readSnoc1 :: Property
readSnoc1 = property $ do
  l <- forAll list
  let size = length l
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  let tester = readSnoc1Test v ix
  test $ unUnrestricted Linear.$ Vector.fromList l tester

readSnoc1Test :: Int -> Int -> VectorTester
readSnoc1Test val ix vec = fromRead (Vector.read ix vec)
  where
    fromRead :: (Vector.Vector Int, Unrestricted Int) #-> Unrestricted (TestT IO ())
    fromRead (vec, val') =
      compInts
        (getSnd (Vector.read ix (Vector.snoc val vec)))
        val'


readSnoc2 :: Property
readSnoc2 = property $ do
  l <- forAll list
  test . Linear.forget unUnrestricted
    $ Vector.fromList l
    $ \vec ->
      vec
        Linear.& Vector.length
        Linear.& \(vec, Unrestricted len) -> Vector.snoc 42 vec
        Linear.& Vector.read (length l)
        Linear.& getSnd
        Linear.& compInts (Unrestricted 42)

lenConst :: Property
lenConst = property $ do
  size <- forAll $ Gen.int $ Range.linearFrom 1 1 1000
  v <- forAll val
  test $ unUnrestricted Linear.$ Vector.constant size v (lenConstTest size)

lenConstTest :: Int -> VectorTester
lenConstTest size vec =
  compInts (move size) (getSnd Linear.$ Vector.length vec)

lenWrite :: Property
lenWrite = property $ do
  l <- forAll list
  let size = length l
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  let tester = lenWriteTest size v ix
  test $ unUnrestricted Linear.$ Vector.fromList l tester

lenWriteTest :: Int -> Int -> Int -> VectorTester
lenWriteTest size val ix vec =
  compInts
    (move size)
    (getSnd Linear.$ Vector.length (Vector.write ix val vec))

lenSnoc :: Property
lenSnoc = property $ do
  l <- forAll list
  test . Linear.forget unUnrestricted
    $ Vector.fromList l
    $ \vec ->
      vec
        Linear.& Vector.length
        Linear.& \(vec, Unrestricted oldLength) -> Vector.snoc 42 vec
        Linear.& Vector.length
        Linear.& getSnd
        Linear.& \(Unrestricted newLength) -> Unrestricted $ newLength === oldLength + 1

-- https://github.com/tweag/linear-base/pull/135
readAndWriteTest :: Property
readAndWriteTest = withTests 1 . property $
  unUnrestricted (Vector.fromList "a" test) === 'a'
  where
    test :: Vector.Vector Char #-> Unrestricted Char
    test vec =
      Vector.read 0 vec Linear.& \(vec', before) ->
        Vector.write 0 'b' vec' Linear.& \vec'' ->
          vec'' `Linear.lseq` before
