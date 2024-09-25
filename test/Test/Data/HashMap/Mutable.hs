{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable hashmaps
--
-- See the testing framework explained in Test.Data.Mutable.Set.
--
-- The combination of axioms and homomorphisms provided (for the most part)
-- functionally specify the behavior of hashmaps. There are a few things
-- we leave out and mention below.
--
-- Remarks:
-- * We don't test trivial things like: empty, capacity
-- * We don't test member since we test lookup
-- * We don't test alter and hope insert and delete tests suffice
-- * We don't test filterWithKey and hope the test for filter suffices
-- * We don't test mapMaybe since mapMaybeWithKey is more general
module Test.Data.HashMap.Mutable
  ( mutHMTests,
  )
where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Function ((&))
import qualified Data.Functor.Linear as Linear
import qualified Data.HashMap.Mutable.Linear as HashMap
import Data.List (sort)
import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)
import Data.Unrestricted.Linear
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

-- # Exported Tests
--------------------------------------------------------------------------------

mutHMTests :: TestTree
mutHMTests = testGroup "Mutable hashmap tests" group

group :: [TestTree]
group =
  [ -- Axiomatic tests
    testPropertyNamed "∀ k,v,m. lookup k (insert m k v) = Just v" "lookupInsert1" lookupInsert1,
    testPropertyNamed
      "∀ k,v,m,k'/=k. lookup k'(insert m k v) = lookup k' m"
      "lookuInsert2"
      lookupInsert2,
    testPropertyNamed "∀ k,m. lookup k (delete m k) = Nothing" "lookupDelete1" lookupDelete1,
    testPropertyNamed
      "∀ k,m,k'/=k. lookup k' (delete m k) = lookup k' m"
      "lookupDelete2"
      lookupDelete2,
    testPropertyNamed "∀ k,v,m. member k (insert m k v) = True" "memberInsert" memberInsert,
    testPropertyNamed "∀ k,m. member k (delete m k) = False" "memberDelete" memberDelete,
    testPropertyNamed "∀ k,v,m. size (insert (m-k) k v) = 1+ size (m-k)" "sizeInsert" sizeInsert,
    testPropertyNamed "∀ k,m with k. size (delete m k) + 1 = size m" "deleteSize" deleteSize,
    -- Homorphism tests against a reference implementation
    testPropertyNamed "insert k v h = fromList (toList h ++ [(k,v)])" "refInsert" refInsert,
    testPropertyNamed "delete k h = fromList (filter (!= k . fst) (toList h))" "refDelete" refDelete,
    testPropertyNamed "fst . lookup k h = lookup k (toList h)" "refLookup" refLookup,
    testPropertyNamed "mapMaybe f h = fromList . mapMaybe (uncurry f) . toList" "refMap" refMap,
    testPropertyNamed "size = length . toList" "refSize" refSize,
    testPropertyNamed "toList . fromList = id" "refToListFromList" refToListFromList,
    testPropertyNamed "filter f (fromList xs) = fromList (filter f xs)" "refFilter" refFilter,
    testPropertyNamed "fromList xs <> fromList ys = fromList (xs <> ys)" "refMappend" refMappend,
    testPropertyNamed "unionWith reference" "refUnionWith" refUnionWith,
    testPropertyNamed "intersectionWith reference" "refIntersectionWith" refIntersectionWith,
    -- Misc
    testPropertyNamed "toList . shrinkToFit = toList" "shrinkToFitTest" shrinkToFitTest
  ]

-- # Internal Library
--------------------------------------------------------------------------------

-- # Mini Testing Framework
----------------------------------------

-- | All tests are on maps from int to string
type HMap = HashMap.HashMap Int String

-- | A test checks a boolean property on a hashmap and consumes it
type HMTest = HMap %1 -> Ur Bool

maxSize :: Int
maxSize = 800

-- HashMap's have lots of corner cases, so we try harder to find them.
defProperty :: PropertyT IO () -> Property
defProperty = withTests 1000 . property

-- | Run a test on a random HashMap
testOnAnyHM :: PropertyT IO HMTest -> Property
testOnAnyHM propHmtest = defProperty $ do
  kvs <- forAll keyVals
  hmtest <- propHmtest
  assert $ unur Linear.$ HashMap.fromList kvs hmtest

testKVPairExists :: (Int, String) -> HMTest
testKVPairExists (k, v) hmap =
  fromLookup Linear.$ getFst Linear.$ HashMap.lookup k hmap
  where
    fromLookup :: Ur (Maybe String) %1 -> Ur Bool
    fromLookup (Ur Nothing) = Ur False
    fromLookup (Ur (Just v')) = Ur (v' == v)

testKeyMember :: Int -> HMTest
testKeyMember key hmap = getFst Linear.$ HashMap.member key hmap

testKeyNotMember :: Int -> HMTest
testKeyNotMember key hmap = Linear.fmap Linear.not (testKeyMember key hmap)

-- | That is, test that lookup gives us `Nothing`
testKeyMissing :: Int -> HMTest
testKeyMissing key hmap =
  fromLookup Linear.$ getFst Linear.$ HashMap.lookup key hmap
  where
    fromLookup :: Ur (Maybe String) %1 -> Ur Bool
    fromLookup (Ur Nothing) = Ur True
    fromLookup (Ur _) = Ur False

testLookupUnchanged :: (HMap %1 -> HMap) -> Int -> HMTest
testLookupUnchanged f k hmap = fromLookup (HashMap.lookup k hmap)
  where
    fromLookup :: (Ur (Maybe String), HMap) %1 -> Ur Bool
    fromLookup (look1, hmap') =
      compareMaybes look1 (getFst Linear.$ HashMap.lookup k (f hmap'))

insertPair :: (Int, String) -> HMap %1 -> HMap
insertPair (k, v) hmap = HashMap.insert k v hmap

-- XXX: This is a terrible name
getFst :: (Consumable b) => (a, b) %1 -> a
getFst (a, b) = lseq b a

compareMaybes ::
  (Eq a) =>
  Ur (Maybe a) %1 ->
  Ur (Maybe a) %1 ->
  Ur Bool
compareMaybes (Ur a) (Ur b) = Ur (a == b)

-- # Random Generation
----------------------------------------

-- | Key generator
key :: Gen Int
key = Gen.int $ Range.linearFrom 0 (-20) 20

-- | Value generator
val :: Gen String
val = do
  let strSize = Range.singleton 3
  Gen.string strSize Gen.alpha

-- | Random pairs with no duplicate keys
keyVals :: Gen [(Int, String)]
keyVals = do
  size <- Gen.int $ Range.linear 0 maxSize
  let sizeGen = Range.singleton size
  keys <- Gen.list sizeGen key
  vals <- Gen.list sizeGen val
  return $ zip keys vals

-- # Tests
--------------------------------------------------------------------------------

lookupInsert1 :: Property
lookupInsert1 = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let insertKV = insertPair (k, v)
  let checkKV = testKVPairExists (k, v)
  return (checkKV Linear.. insertKV)

lookupInsert2 :: Property
lookupInsert2 = testOnAnyHM $ do
  k <- forAll key
  k' <- forAll $ Gen.filter (/= k) key
  v <- forAll val
  let insertKV = insertPair (k, v)
  return (testLookupUnchanged insertKV k')

lookupDelete1 :: Property
lookupDelete1 = testOnAnyHM $ do
  k <- forAll key
  let checkNoKey = testKeyMissing k
  return (checkNoKey Linear.. HashMap.delete k)

lookupDelete2 :: Property
lookupDelete2 = testOnAnyHM $ do
  k <- forAll key
  k' <- forAll $ Gen.filter (/= k) key
  return (testLookupUnchanged (HashMap.delete k) k')

memberInsert :: Property
memberInsert = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let insertKV = insertPair (k, v)
  let isMemberK = testKeyMember k
  return (isMemberK Linear.. insertKV)

memberDelete :: Property
memberDelete = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let pair = (k, v)
  let insertKV = insertPair pair
  let checkNotMember = testKeyNotMember k
  return (checkNotMember Linear.. HashMap.delete k Linear.. insertKV)

sizeInsert :: Property
sizeInsert = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let pair = (k, v)
  let insertCheckSize = checkSizeAfterInsert pair
  return (insertCheckSize Linear.. HashMap.delete k)

checkSizeAfterInsert :: (Int, String) -> HMTest
checkSizeAfterInsert (k, v) hmap = withSize Linear.$ HashMap.size hmap
  where
    withSize :: (Ur Int, HMap) %1 -> Ur Bool
    withSize (oldSize, hmap) =
      checkSize oldSize Linear.$
        getFst Linear.$
          HashMap.size Linear.$
            HashMap.insert k v hmap
    checkSize :: Ur Int %1 -> Ur Int %1 -> Ur Bool
    checkSize (Ur old) (Ur new) =
      Ur ((old + 1) == new)

deleteSize :: Property
deleteSize = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let insertKV = insertPair (k, v)
  let checkSize = checkSizeAfterDelete k
  return (checkSize Linear.. insertKV)

checkSizeAfterDelete :: Int -> HMTest
checkSizeAfterDelete key hmap = fromSize (HashMap.size hmap)
  where
    fromSize :: (Ur Int, HMap) %1 -> Ur Bool
    fromSize (orgSize, hmap) =
      compSizes orgSize Linear.$
        getFst Linear.$
          HashMap.size (HashMap.delete key hmap)
    compSizes :: Ur Int %1 -> Ur Int %1 -> Ur Bool
    compSizes (Ur orgSize) (Ur newSize) =
      Ur ((newSize + 1) == orgSize)

refInsert :: Property
refInsert = defProperty $ do
  k <- forAll key
  v <- forAll val
  kvs <- forAll keyVals
  let listInsert = HashMap.fromList (kvs ++ [(k, v)]) HashMap.toList
  let hmInsert = HashMap.fromList kvs (HashMap.toList Linear.. HashMap.insert k v)
  sort (unur listInsert) === sort (unur hmInsert)

refDelete :: Property
refDelete = defProperty $ do
  k <- forAll key
  kvs <- forAll keyVals
  let kvs' = filter ((/= k) . fst) kvs
  let listInsert = HashMap.fromList kvs' HashMap.toList
  let hmInsert = HashMap.fromList kvs (HashMap.toList Linear.. HashMap.delete k)
  sort (unur listInsert) === sort (unur hmInsert)

refLookup :: Property
refLookup = defProperty $ do
  kvs <- forAll keyVals
  k <- forAll key
  let listLookup = List.lookup k (List.reverse kvs)
  let hmLookup = HashMap.fromList kvs (getFst Linear.. HashMap.lookup k)
  listLookup === unur hmLookup

refMap :: Property
refMap = defProperty $ do
  let f k v = if mod k 5 < 3 then Just (show k ++ v) else Nothing
  let f' (k, v) = fmap ((,) k) (f k v)
  kvs <- forAll keyVals
  let mappedList = mapMaybe f' (nubOrdOn fst (List.reverse kvs))
  let mappedHm = HashMap.fromList kvs (HashMap.toList Linear.. HashMap.mapMaybeWithKey f)
  sort mappedList === sort (unur mappedHm)

refSize :: Property
refSize = defProperty $ do
  kvs <- forAll keyVals
  length (nubOrdOn fst kvs) === unur (HashMap.fromList kvs (getFst Linear.. HashMap.size))

refToListFromList :: Property
refToListFromList = defProperty $ do
  xs <- forAll keyVals

  let expected =
        Map.fromList xs
          & Map.toList

      Ur actual = HashMap.fromList xs HashMap.toList

  sort expected === sort actual

refFilter :: Property
refFilter = defProperty $ do
  xs <- forAll keyVals

  let predicate "" = False
      predicate (i : _) = i < 'h'

      expected =
        Map.fromList xs
          & Map.filter predicate
          & Map.toList

      Ur actual =
        HashMap.fromList xs Linear.$
          HashMap.toList Linear.. HashMap.filter predicate

  sort expected === sort actual

refMappend :: Property
refMappend = defProperty $ do
  xs <- forAll keyVals
  ys <- forAll keyVals

  let Ur expected =
        HashMap.fromList (xs <> ys) Linear.$ HashMap.toList

      Ur actual =
        HashMap.fromList xs Linear.$ \hx ->
          HashMap.fromList ys Linear.$ \hy ->
            HashMap.toList (hx Linear.<> hy)

  sort expected === sort actual

refUnionWith :: Property
refUnionWith = defProperty $ do
  xs <- forAll keyVals
  ys <- forAll keyVals

  let combine a b = a ++ "," ++ b

      expected =
        Map.unionWith
          combine
          (Map.fromList xs)
          (Map.fromList ys)
          & Map.toList

      Ur actual =
        HashMap.fromList xs Linear.$ \hx ->
          HashMap.fromList ys Linear.$ \hy ->
            HashMap.unionWith combine hx hy
              Linear.& HashMap.toList

  sort expected === sort actual

refIntersectionWith :: Property
refIntersectionWith = defProperty $ do
  xs <- forAll keyVals
  ys <- forAll keyVals

  let expected =
        Map.intersectionWith
          (,)
          (Map.fromList xs)
          (Map.fromList ys)
          & Map.toList

      Ur actual =
        HashMap.fromList xs Linear.$ \hx ->
          HashMap.fromList ys Linear.$ \hy ->
            HashMap.intersectionWith (,) hx hy
              Linear.& HashMap.toList

  sort expected === sort actual

shrinkToFitTest :: Property
shrinkToFitTest = defProperty $ do
  kvs <- forAll keyVals
  let shrunk = (HashMap.fromList kvs (HashMap.toList Linear.. HashMap.shrinkToFit))
  sort (nubOrdOn fst (List.reverse kvs)) === sort (unur shrunk)
