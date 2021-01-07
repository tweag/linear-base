{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Foreign (foreignGCTests) where

import Data.Typeable
import Control.Monad (void)
import Control.Exception hiding (assert)
import qualified Foreign.Heap as Heap
import Foreign.List (List)
import qualified Foreign.List as List
import qualified Foreign.Marshal.Pure as Manual
import qualified Prelude
import Prelude.Linear
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- # Organizing tests
-------------------------------------------------------------------------------

foreignGCTests :: TestTree
foreignGCTests = testGroup "foreignGCTests"
  [ listExampleTests
  , heapExampleTests
  ]

listExampleTests :: TestTree
listExampleTests = testGroup "list tests"
  [ testProperty "List.toList . List.fromList = id" invertNonGCList
  , testProperty "map id = id" mapIdNonGCList
  , testProperty "memory freed post-exception" testExecptionOnMem
  ]

heapExampleTests :: TestTree
heapExampleTests = testGroup "heap tests"
  [ testProperty "sort = heapsort" nonGCHeapSort ]


-- # Internal library
-------------------------------------------------------------------------------

list :: Gen [Int]
list = Gen.list (Range.linear 0 1000) (Gen.int (Range.linear 0 100))

eqList :: forall a. (Manual.Representable a, Movable a, Eq a) =>
  List a %1-> List a %1-> Ur Bool
eqList l1 l2 = move $ (List.toList l1) == (List.toList l2)

data InjectedError = InjectedError
  deriving (Typeable, Show)

instance Exception InjectedError


-- # Properties
-------------------------------------------------------------------------------

invertNonGCList :: Property
invertNonGCList = property Prelude.$ do
  xs <- forAll list
  let xs' = unur $
        Manual.withPool (\p -> move $ List.toList $ List.ofList xs p)
  xs === xs'

mapIdNonGCList :: Property
mapIdNonGCList = property Prelude.$ do
  xs <- forAll list
  let boolTest = unur $ Manual.withPool $ \p ->
        dup3 p & \(p0,p1,p2) ->
          eqList (List.ofList xs p0) (List.map id (List.ofList xs p1) p2)
  assert boolTest

testExecptionOnMem :: Property
testExecptionOnMem = property Prelude.$ do
  xs <- forAll list
  let bs = xs ++ (throw InjectedError)
  let writeBadList = Manual.withPool (move . List.toList . List.ofRList bs)
  let ignoreCatch = \_ -> Prelude.return ()
  evalIO (catch @InjectedError (void (evaluate writeBadList)) ignoreCatch)

nonGCHeapSort :: Property
nonGCHeapSort = property Prelude.$ do
  xs <- forAll list
  let ys :: [(Int,())] = zip xs $ Prelude.replicate (Prelude.length xs) ()
  (Heap.sort ys) === (reverse $ sort ys)

