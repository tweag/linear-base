{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Unrestricted.Linear
import qualified Data.Functor.Linear as Data
import Hedgehog
import Data.Ord.Linear as Linear
import Data.Maybe (mapMaybe)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear hiding ((>))
import qualified Data.Vector as ImmutableVector
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
  , testProperty "∀ a,x,(i < len a). read (push a x) i = read a i" readPush1
  , testProperty "∀ a,x. read (push a x) (len a) = x" readPush2
  -- All tests for exprs of the form (length (const ...))
  , testProperty "∀ s,x. len (constant s x) = s" lenConst
  , testProperty "∀ a,i,x. len (write a i x) = len a" lenWrite
  , testProperty "∀ a,x. len (push a x) = 1 + len a" lenPush
  -- Tests against a reference implementation
  , testProperty "pop . push _ = id" refPopPush
  , testProperty "push . pop = id" refPushPop
  , testProperty "slice s n = take s . drop n" refSlice
  , testProperty "toList . fromList = id" refToListFromList
  , testProperty "toList can be implemented with repeated pops" refToListViaPop
  , testProperty "fromList can be implemented with repeated pushes" refFromListViaPush
  , testProperty "toList works with extra capacity" refToListWithExtraCapacity
  , testProperty "fromList xs <> fromList ys = fromList (xs <> ys)" refMappend
  , testProperty "mapMaybe f (fromList xs) = fromList (mapMaybe f xs)" refMapMaybe
  , testProperty "filter f (fromList xs) = fromList (filter f xs)" refFilter
  , testProperty "f <$> fromList xs == fromList (f <$> xs)" refFmap
  , testProperty "toList . freeze . fromList = id" refFreeze
  -- Regression tests
  , testProperty "push on an empty vector should succeed" snocOnEmptyVector
  , testProperty "do not reorder reads and writes" readAndWriteTest
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type VectorTester = Vector.Vector Int #-> Ur (TestT IO ())

nonEmptyList :: Gen [Int]
nonEmptyList = Gen.list (Range.linear 1 1000) val

list :: Gen [Int]
list = Gen.list (Range.linear 0 1000) val

val :: Gen Int
val = Gen.int (Range.linear (-1000) 1000)

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

snocOnEmptyVector :: Property
snocOnEmptyVector = withTests 1 . property $ do
  let Ur actual =
        Vector.empty
          Linear.$ \vec -> Vector.push vec (42 :: Int)
          Linear.& \vec -> Vector.read vec 0
          Linear.& getSnd
  actual === 42

-- | Constant should give us a constant vector.
readConst :: Property
readConst = property $ do
  size <- forAll $ Gen.int $ Range.linear 1 1000
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  test $ unur Linear.$ Vector.constant size v (readConstTest ix v)

readConstTest :: Int -> Int -> VectorTester
readConstTest ix val vec = compInts (getSnd (Vector.read vec ix)) (move val)

readWrite1 :: Property
readWrite1 = property $ do
  l <- forAll nonEmptyList
  let size = length l
  ix <- forAll $ Gen.element [0..size-1]
  v <- forAll val
  let tester = readWrite1Test ix v
  test $ unur Linear.$ Vector.fromList l tester

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
  test $ unur Linear.$ Vector.fromList l tester

readWrite2Test :: Int -> Int -> Int -> VectorTester
readWrite2Test ix jx val vec = fromRead (Vector.read vec ix)
  where
    fromRead :: (Vector.Vector Int, Ur Int) #-> Ur (TestT IO ())
    fromRead (vec, val1) =
      compInts
        val1
        (getSnd Linear.$ Vector.read (Vector.write vec jx val) ix)

readPush1 :: Property
readPush1 = property $ do
  l <- forAll nonEmptyList
  let size = length l
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  let tester = readPush1Test v ix
  test $ unur Linear.$ Vector.fromList l tester

readPush1Test :: Int -> Int -> VectorTester
readPush1Test val ix vec = fromRead (Vector.read vec ix)
  where
    fromRead :: (Vector.Vector Int, Ur Int) #-> Ur (TestT IO ())
    fromRead (vec,val') =
      compInts (getSnd (Vector.read (Vector.push vec val) ix)) val'


readPush2 :: Property
readPush2 = property $ do
  l <- forAll list
  v <- forAll val
  let tester = readPush2Test v
  test $ unur Linear.$ Vector.fromList l tester

readPush2Test :: Int -> VectorTester
readPush2Test val vec = fromLen (Vector.size vec)
  where
    fromLen ::
      (Vector.Vector Int, Ur Int) #->
      Ur (TestT IO ())
    fromLen (vec, Ur len) =
      compInts (getSnd (Vector.read (Vector.push vec val) len)) (move val)

lenConst :: Property
lenConst = property $ do
  size <- forAll $ Gen.int $ Range.linear 1 1000
  v <- forAll val
  test $ unur Linear.$ Vector.constant size v (lenConstTest size)

lenConstTest :: Int -> VectorTester
lenConstTest size vec =
  compInts (move size) (getSnd Linear.$ Vector.size vec)

lenWrite :: Property
lenWrite = property $ do
  l <- forAll nonEmptyList
  let size = length l
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  let tester = lenWriteTest size v ix
  test $ unur Linear.$ Vector.fromList l tester

lenWriteTest :: Int -> Int -> Int -> VectorTester
lenWriteTest size val ix vec =
  compInts
    (move size)
    (getSnd Linear.$ Vector.size (Vector.write vec ix val))

lenPush :: Property
lenPush = property $ do
 l <- forAll list
 v <- forAll val
 let tester = lenPushTest v
 test $ unur Linear.$ Vector.fromList l tester

lenPushTest :: Int -> VectorTester
lenPushTest val vec = fromLen Linear.$ Vector.size vec
  where
    fromLen ::
      (Vector.Vector Int, Ur Int) #->
      Ur (TestT IO ())
    fromLen (vec, Ur len) =
      compInts (move (len+1)) (getSnd (Vector.size (Vector.push vec val)))

refToListFromList :: Property
refToListFromList = property $ do
  xs <- forAll list
  let Ur actual = Vector.fromList xs Vector.toList
  xs === actual

refToListWithExtraCapacity :: Property
refToListWithExtraCapacity = property $ do
  xs <- forAll list
  let val = 12
      expected = xs ++ [val]
      Ur actual =
        Vector.fromList xs Linear.$ \vec ->
          Vector.push vec val -- This will cause the vector to grow.
            Linear.& Vector.toList
  expected === actual

refPopPush :: Property
refPopPush = property $ do
  xs <- forAll list
  let Ur actual =
        Vector.fromList xs Linear.$ \vec ->
          Vector.push vec (error "not used")
            Linear.& Vector.pop
            Linear.& \(vec, Ur _) ->
                        Vector.toList vec
  xs === actual

refPushPop :: Property
refPushPop = property $ do
  xs <- forAll nonEmptyList
  let Ur actual =
        Vector.fromList xs Linear.$ \vec ->
          Vector.pop vec
            Linear.& \(vec, Ur (Just a)) ->
                        Vector.push vec a
            Linear.& Vector.toList
  xs === actual

refToListViaPop :: Property
refToListViaPop = property $ do
  xs <- forAll list
  let Ur actual =
        Vector.fromList xs (popAll [])
  xs === actual
 where
   popAll :: [a] -> Vector.Vector a #-> Ur [a]
   popAll acc vec =
     Vector.pop vec Linear.& \case
       (vec', Ur Nothing) -> vec' `lseq` Ur acc
       (vec', Ur (Just x)) -> popAll (x:acc) vec'

refFromListViaPush :: Property
refFromListViaPush = property $ do
  xs <- forAll list
  let Ur actual =
        Vector.empty Linear.$
          Vector.toList Linear.. pushAll xs
  xs === actual
 where
   pushAll :: [a] -> Vector.Vector a #-> Vector.Vector a
   pushAll [] vec = vec
   pushAll (x:xs) vec = Vector.push vec x Linear.& pushAll xs

refSlice :: Property
refSlice = property $ do
  xs <- forAll list
  s <- forAll $ Gen.int (Range.linear 0 (length xs))
  n <- forAll $ Gen.int (Range.linear 0 (length xs - s))
  let expected = take n . drop s $ xs
      Ur actual =
        Vector.fromList xs Linear.$ \arr ->
          Vector.slice s n arr
            Linear.& Vector.toList
  expected === actual

refMappend :: Property
refMappend = property $ do
  xs <- forAll list
  ys <- forAll list
  let expected = xs <> ys
      Ur actual =
        Vector.fromList xs Linear.$ \vx ->
          Vector.fromList ys Linear.$ \vy ->
            Vector.toList (vx Linear.<> vy)
  expected === actual

refFmap :: Property
refFmap = property $ do
  xs <- forAll list
  let -- An arbitrary function
      f :: Int #-> Bool
      f = (Linear.> 0)
      expected = map (Linear.forget f) xs
      Ur actual =
        Vector.fromList xs Linear.$ \vec ->
          Vector.toList (f Data.<$> vec)
  expected === actual

refMapMaybe :: Property
refMapMaybe = property $ do
  xs <- forAll list
  let -- An arbitrary function
      f :: Int -> Maybe Bool
      f = (\a -> if a Prelude.< 0 then Nothing else Just (a `mod` 2 == 0))
      expected = mapMaybe f xs
      Ur actual =
        Vector.fromList xs Linear.$ \vec ->
          Vector.mapMaybe vec f
            Linear.& Vector.toList
  expected === actual

refFilter :: Property
refFilter = property $ do
  xs <- forAll list
  let -- An arbitrary function
      f :: Int -> Bool
      f = (Prelude.< 0)
      expected = filter f xs
      Ur actual =
        Vector.fromList xs Linear.$ \vec ->
          Vector.filter vec f
            Linear.& Vector.toList
  expected === actual

refFreeze :: Property
refFreeze = property $ do
  xs <- forAll list

  -- Add a new element at the end of the vector
  -- to force resizing, to test the case where
  -- sz < cap.
  shouldAppend <- forAll Gen.bool

  let expected =
        if shouldAppend
        then xs ++ [12]
        else xs

      Ur actual = Vector.fromList xs Linear.$ \vec ->
           (if shouldAppend
            then Vector.push vec 12
            else vec
           ) Linear.& Vector.freeze
  expected === ImmutableVector.toList actual

-- https://github.com/tweag/linear-base/pull/135
readAndWriteTest :: Property
readAndWriteTest = withTests 1 . property $
  unur (Vector.fromList "a" test) === 'a'
  where
    test :: Vector.Vector Char #-> Ur Char
    test vec =
      Vector.read vec 0 Linear.& \(vec', before) ->
        Vector.write vec' 0 'b' Linear.& \vec'' ->
          vec'' `Linear.lseq` before
