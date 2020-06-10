{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable hashmaps
-- We are excluding tests for alter because it's assumed that alter is
-- implemented parametrically around insert and delete.
module Test.Data.Mutable.HashMap
  ( mutHMTests,
  )
where

import qualified Data.Functor.Linear as Linear
import qualified Data.HashMap.Linear as HashMap
import Data.Unrestricted.Linear
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

-- # Exported Tests
--------------------------------------------------------------------------------

mutHMTests :: TestTree
mutHMTests = testGroup "Mutable hashmap tests" group

group :: [TestTree]
group =
  [ testProperty "∀ k,v. lookup k (sing k v) = Just v" lookupSing1,
    testProperty "∀ k,v,k'/=k. lookup k' (sing k v) = Nothing" lookupSing2,
    testProperty "∀ k,v,m. lookup k (insert m k v) = Just v" lookupInsert1,
    testProperty
      "∀ k,v,m,k'/=k. lookup k'(insert m k v) = lookup k' m"
      lookupInsert2,
    testProperty "∀ k,m. lookup k (delete m k) = Nothing" lookupDelete1,
    testProperty
      "∀ k,m,k'/=k. lookup k' (delete m k) = lookup k' m"
      lookupDelete2,
    testProperty "∀ k,v,m. member k (insert m k v) = True" memberInsert,
    testProperty "∀ k,m. member k (delete m k) = False" memberDelete,
    testProperty "∀ k,v,m. size (insert (m-k) k v) = 1+ size (m-k)" sizeInsert,
    testProperty "∀ k,m with k. size (delete m k) + 1 = size m" deleteSize
  ]

-- # Internal Library
--------------------------------------------------------------------------------

-- # Mini Testing Framework
----------------------------------------

-- | All tests are on maps from int to string
type HMap = HashMap.HashMap Int String

-- | A test checks a boolean property on a hashmap and consumes it
type HMTest = HMap #-> Unrestricted Bool

-- | Run a test on a random HashMap
testOnAnyHM :: PropertyT IO HMTest -> Property
testOnAnyHM propHmtest = property $ do
  (kv : kvs) <- forAll keyVals
  let randHM = makeHM kvs
  hmtest <- propHmtest
  let test = hmtest Linear.. randHM
  assert $ unUnrestricted Linear.$ HashMap.singleton kv test

testKVPairExists :: (Int, String) -> HMTest
testKVPairExists (k, v) hmap =
  fromLookup Linear.$ getSnd Linear.$ HashMap.lookup hmap k
  where
    fromLookup :: Unrestricted (Maybe String) #-> Unrestricted Bool
    fromLookup (Unrestricted Nothing) = Unrestricted False
    fromLookup (Unrestricted (Just v')) = Unrestricted (v' == v)

testKeyMember :: Int -> HMTest
testKeyMember key hmap = move Linear.$ getSnd Linear.$ HashMap.member hmap key

testKeyNotMember :: Int -> HMTest
testKeyNotMember key hmap = Linear.fmap Linear.not (testKeyMember key hmap)

-- | That is, test that lookup gives us `Nothing`
testKeyMissing :: Int -> HMTest
testKeyMissing key hmap =
  fromLookup Linear.$ getSnd Linear.$ HashMap.lookup hmap key
  where
    fromLookup :: Unrestricted (Maybe String) #-> Unrestricted Bool
    fromLookup (Unrestricted Nothing) = Unrestricted True
    fromLookup (Unrestricted _) = Unrestricted False

testLookupUnchanged :: (HMap #-> HMap) -> Int -> HMTest
testLookupUnchanged f k hmap = fromLookup (HashMap.lookup hmap k)
  where
    fromLookup :: (HMap, Unrestricted (Maybe String)) #-> Unrestricted Bool
    fromLookup (hmap', look1) =
      compareMaybes look1 (getSnd Linear.$ HashMap.lookup (f hmap') k)

deleteKey :: Int -> HMap #-> HMap
deleteKey key hmap = HashMap.delete hmap key

insertPair :: (Int, String) -> HMap #-> HMap
insertPair (k, v) hmap = HashMap.insert hmap k v

-- XXX: This is a terrible name
getSnd :: (Consumable a) => (a, b) #-> b
getSnd (a, b) = lseq a b

compareMaybes :: Eq a =>
  Unrestricted (Maybe a) #->
  Unrestricted (Maybe a) #->
  Unrestricted Bool
compareMaybes (Unrestricted a) (Unrestricted b) = Unrestricted (a == b)

-- # Random Generation
----------------------------------------

-- | Key generator
key :: Gen Int
key = Gen.int $ Range.linearFrom 0 (-900) 900

-- | Value generator
val :: Gen String
val = do
  let strSize = Range.singleton 4
  Gen.string strSize Gen.alpha

-- | Random pairs
keyVals :: Gen [(Int, String)]
keyVals = do
  size <- Gen.int $ Range.constantFrom 600 400 800
  let sizeGen = Range.singleton size
  keys <- Gen.list sizeGen key
  vals <- Gen.list sizeGen val
  return $ zip keys vals

-- | Insert all given pairs in order
makeHM :: [(Int, String)] -> HMap #-> HMap
makeHM [] hmap = hmap
makeHM ((k, v) : xs) hmap = makeHM xs (HashMap.insert hmap k v)

-- # Tests
--------------------------------------------------------------------------------

lookupSing1 :: Property
lookupSing1 = property $ do
  k <- forAll key
  v <- forAll val
  let test = testKVPairExists (k, v)
  assert $ unUnrestricted Linear.$ HashMap.singleton (k, v) test

lookupSing2 :: Property
lookupSing2 = property $ do
  k <- forAll key
  v <- forAll val
  k' <- forAll $ Gen.filter (/= k) key
  let test = testKeyMissing k'
  assert $ unUnrestricted Linear.$ HashMap.singleton (k, v) test

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
  let deleteK = deleteKey k
  let checkNoKey = testKeyMissing k
  return (checkNoKey Linear.. deleteK)

lookupDelete2 :: Property
lookupDelete2 = testOnAnyHM $ do
  k <- forAll key
  k' <- forAll $ Gen.filter (/= k) key
  let deleteK = deleteKey k
  return (testLookupUnchanged deleteK k')

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
  let deleteK = deleteKey k
  let insertKV = insertPair pair
  let checkNotMember = testKeyNotMember k
  return (checkNotMember Linear.. deleteK Linear.. insertKV)

sizeInsert :: Property
sizeInsert = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let pair = (k, v)
  let deleteK = deleteKey k
  let insertCheckSize = checkSizeAfterInsert pair
  return (insertCheckSize Linear.. deleteK)

checkSizeAfterInsert :: (Int, String) -> HMTest
checkSizeAfterInsert (k, v) hmap = withSize Linear.$ HashMap.size hmap
  where
    withSize :: (HMap, Int) #-> Unrestricted Bool
    withSize (hmap, oldSize) =
      checkSize (move oldSize)
        Linear.$ move
        Linear.$ getSnd
        Linear.$ HashMap.size
        Linear.$ HashMap.insert hmap k v
    checkSize :: Unrestricted Int #-> Unrestricted Int #-> Unrestricted Bool
    checkSize (Unrestricted old) (Unrestricted new) =
      Unrestricted ((old + 1) == new)

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
    fromSize :: (HMap, Int) #-> Unrestricted Bool
    fromSize (hmap, orgSize) =
      compSizes (move orgSize)
        Linear.$ move
        Linear.$ getSnd
        Linear.$ HashMap.size (HashMap.delete hmap key)
    compSizes :: Unrestricted Int #-> Unrestricted Int #-> Unrestricted Bool
    compSizes (Unrestricted orgSize) (Unrestricted newSize) =
      Unrestricted ((newSize + 1) == orgSize)
