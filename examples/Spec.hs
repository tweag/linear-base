{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.OffHeap as Manual
import Data.OffHeap (Pool)
import Foreign.Storable (Storable)
import qualified OffHeap.Heap as Heap ()
import qualified OffHeap.List as List
import OffHeap.List (List)
import Prelude.Linear
import Test.Hspec
import Test.QuickCheck

eqList :: forall a. (Storable a, Movable a, Eq a) => List a ->. List a ->. Unrestricted Bool
eqList l1 l2 =
    eqUL (move (List.toList l1)) (move (List.toList l2))
  where
    eqUL :: Unrestricted [a] ->. Unrestricted [a] ->. Unrestricted Bool
    eqUL (Unrestricted as1) (Unrestricted as2) = Unrestricted (as1 == as2)

main :: IO ()
main = hspec $ do
  describe "Off-heap lists" $ do
    describe "ofList" $ do
      it "is invertible" $
        property (\(l :: [Int]) -> unUnrestricted (Manual.withPool $ \pool ->
          let
            check :: Unrestricted [Int] ->. Unrestricted Bool
            check (Unrestricted l') = Unrestricted $ l' == l
          in
            check $ move (List.toList $ List.ofList l pool)))

    describe "map" $ do
      it "of identity if the identity" $
        property (\(l :: [Int]) -> unUnrestricted (Manual.withPool $ \pool ->
          let
            check :: (Pool, Pool, Pool) ->. Unrestricted Bool
            check (pool1, pool2, pool3) =
              eqList
                (List.map (\x -> x) (List.ofList l pool1) pool2)
                (List.ofList l pool3)
          in
            check (dup3 pool)))

