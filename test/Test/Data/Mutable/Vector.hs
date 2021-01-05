{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable vectors.
--
-- See the testing framework explained in Test.Data.Mutable.Set.
--
-- The combination of axioms and homomorphisms provided functionally specify
-- the behavior of vectors.
--
-- Remarks:
--  * We don't test for failure on out-of-bound access
--  * We don't test the empty constructor
module Test.Data.Mutable.Vector
  ( mutVecTests,
  )
where

import qualified Data.Vector.Mutable.Linear as Vector
import Data.Unrestricted.Linear
import qualified Data.Functor.Linear as Data
import Hedgehog
import Data.Ord.Linear as Linear hiding (Eq(..))
import Data.Maybe (mapMaybe)
import qualified Data.List as List
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
  , testProperty
      "write ix a v = (\\l -> take ix l ++ [a] ++ drop (ix+1) l) . toList"
      refWrite
  , testProperty "fst $ modify f ix v = snd $ f ((toList v) !! ix)" refModify1
  , testProperty
      "snd (modify f i v) = write (toList v) i (fst (f ((toList v) !! i))))"
      refModify2
  , testProperty "toList . push x = snoc x . toList" refPush
  , testProperty "toList . pop = init . toList" refPop
  , testProperty "read ix v = (toList v) !! ix" refRead
  , testProperty "size = length . toList" refSize
  , testProperty "toList . shrinkToFit = toList" refShrinkToFit
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

type VectorTester = Vector.Vector Int %1-> Ur (TestT IO ())

nonEmptyList :: Gen [Int]
nonEmptyList = Gen.list (Range.linear 1 1000) val

list :: Gen [Int]
list = Gen.list (Range.linear 0 1000) val

val :: Gen Int
val = Gen.int (Range.linear (-1000) 1000)

compInts ::
  Ur Int %1->
  Ur Int %1->
  Ur (TestT IO ())
compInts (Ur x) (Ur y) = Ur (x === y)

-- XXX: This is a terrible name
getFst :: Consumable b => (a, b) %1-> a
getFst (a, b) = lseq b a

getSnd :: Consumable a => (a, b) %1-> b
getSnd (a, b) = lseq a b


-- # Tests
--------------------------------------------------------------------------------

snocOnEmptyVector :: Property
snocOnEmptyVector = withTests 1 . property $ do
  let Ur actual =
        Vector.empty
          Linear.$ \vec -> Vector.push (42 :: Int) vec
          Linear.& Vector.get 0
          Linear.& getFst
  actual === 42

-- | Constant should give us a constant vector.
readConst :: Property
readConst = property $ do
  size <- forAll $ Gen.int $ Range.linear 1 1000
  v <- forAll val
  ix <- forAll $ Gen.element [0..size-1]
  test $ unur Linear.$ Vector.constant size v (readConstTest ix v)

readConstTest :: Int -> Int -> VectorTester
readConstTest ix val vec = compInts (getFst (Vector.read vec ix)) (move val)

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
  compInts (move val) (getFst Linear.$ Vector.read (Vector.write vec ix val) ix)

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
    fromRead :: (Ur Int, Vector.Vector Int) %1-> Ur (TestT IO ())
    fromRead (val1, vec) =
      compInts
        val1
        (getFst Linear.$ Vector.read (Vector.write vec jx val) ix)

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
    fromRead :: (Ur Int, Vector.Vector Int) %1-> Ur (TestT IO ())
    fromRead (val', vec) =
      compInts (getFst (Vector.get ix (Vector.push val vec))) val'


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
      (Ur Int, Vector.Vector Int) %1->
      Ur (TestT IO ())
    fromLen (Ur len, vec) =
      compInts (getFst (Vector.get len (Vector.push val vec))) (move val)

lenConst :: Property
lenConst = property $ do
  size <- forAll $ Gen.int $ Range.linear 1 1000
  v <- forAll val
  test $ unur Linear.$ Vector.constant size v (lenConstTest size)

lenConstTest :: Int -> VectorTester
lenConstTest size vec =
  compInts (move size) (getFst Linear.$ Vector.size vec)

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
    (getFst Linear.$ Vector.size (Vector.write vec ix val))

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
      (Ur Int, Vector.Vector Int) %1->
      Ur (TestT IO ())
    fromLen (Ur len, vec) =
      compInts (move (len+1)) (getFst (Vector.size (Vector.push val vec)))

refWrite :: Property
refWrite = property $ do
  l <- forAll nonEmptyList
  ix <- forAll $ Gen.element [0..(length l - 1)]
  v <- forAll val
  let l' = listWrite ix v l
  l' === unur (Vector.fromList l (Vector.toList Linear.. Vector.set ix v))
  where

    listWrite :: Int -> a -> [a] -> [a]
    listWrite n _ _ | n Prelude.< 0 = error "Index negative"
    listWrite _ _ [] = error "Index too big"
    listWrite 0 a (_:xs) = a:xs
    listWrite n a (x:xs) = x : listWrite (n-1) a xs

refModify1 :: Property
refModify1 = property $ do
  l <- forAll nonEmptyList
  let f x = (mod x 5, (mod x 5) Prelude.< 3)
  ix <- forAll $ Gen.element [0..(length l - 1)]
  snd (f (l !! ix)) === unur (Vector.fromList l (getFst Linear.. Vector.modify f ix))

refModify2 :: Property
refModify2 = property $ do
  l <- forAll nonEmptyList
  let f x = 3*x*x - 2*x + 4
  ix <- forAll $ Gen.element [0..(length l - 1)]
  let l' = listMod ix f l
  l' === unur (Vector.fromList l (Vector.toList Linear.. Vector.modify_ f ix))
  where
    listMod :: Int -> (a -> a) -> [a] -> [a]
    listMod n _ _ | n Prelude.< 0 = error "Index negative"
    listMod _ _ [] = error "Index too big"
    listMod 0 f (x:xs) = f x : xs
    listMod n f (x:xs) = x : listMod (n-1) f xs

refPush :: Property
refPush = property $ do
  l <- forAll list
  v <- forAll val
  let l' = l ++ [v]
  l' === unur (Vector.fromList l (Vector.toList Linear.. Vector.push v))

refPop :: Property
refPop = property $ do
  l <- forAll nonEmptyList
  let v = Vector.fromList l (Vector.toList Linear.. getSnd Linear.. Vector.pop)
  List.init l === unur v

refRead :: Property
refRead = property $ do
  l <- forAll nonEmptyList
  ix <- forAll $ Gen.element [0..(length l - 1)]
  let value = l List.!! ix
  value === unur (Vector.fromList l (getFst Linear.. Vector.get ix))

refSize :: Property
refSize = property $ do
  l <- forAll list
  length l === unur (Vector.fromList l (getFst Linear.. Vector.size))

refShrinkToFit :: Property
refShrinkToFit = property $ do
  l <- forAll list
  l === unur (Vector.fromList l (Vector.toList Linear.. Vector.shrinkToFit))

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
          Vector.push val vec -- This will cause the vector to grow.
            Linear.& Vector.toList
  expected === actual

refPopPush :: Property
refPopPush = property $ do
  xs <- forAll list
  let Ur actual =
        Vector.fromList xs Linear.$ \vec ->
          Vector.push (error "not used") vec
            Linear.& Vector.pop
            Linear.& \(Ur _, vec) ->
                        Vector.toList vec
  xs === actual

refPushPop :: Property
refPushPop = property $ do
  xs <- forAll nonEmptyList
  let Ur actual =
        Vector.fromList xs Linear.$ \vec ->
          Vector.pop vec
            Linear.& \(Ur (Just a), vec) ->
                        Vector.push a vec
            Linear.& Vector.toList
  xs === actual

refToListViaPop :: Property
refToListViaPop = property $ do
  xs <- forAll list
  let Ur actual =
        Vector.fromList xs (popAll [])
  xs === actual
 where
   popAll :: [a] -> Vector.Vector a %1-> Ur [a]
   popAll acc vec =
     Vector.pop vec Linear.& \case
       (Ur Nothing, vec') -> vec' `lseq` Ur acc
       (Ur (Just x), vec') -> popAll (x:acc) vec'

refFromListViaPush :: Property
refFromListViaPush = property $ do
  xs <- forAll list
  let Ur actual =
        Vector.empty Linear.$
          Vector.toList Linear.. pushAll xs
  xs === actual
 where
   pushAll :: [a] -> Vector.Vector a %1-> Vector.Vector a
   pushAll [] vec = vec
   pushAll (x:xs) vec = Vector.push x vec Linear.& pushAll xs

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
      f :: Int %1-> Bool
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
            then Vector.push 12 vec
            else vec
           ) Linear.& Vector.freeze
  expected === ImmutableVector.toList actual

-- https://github.com/tweag/linear-base/pull/135
readAndWriteTest :: Property
readAndWriteTest = withTests 1 . property $
  unur (Vector.fromList "a" test) === 'a'
  where
    test :: Vector.Vector Char %1-> Ur Char
    test vec =
      Vector.read vec 0 Linear.& \(before, vec') ->
        Vector.write vec' 0 'b' Linear.& \vec'' ->
          vec'' `Linear.lseq` before
