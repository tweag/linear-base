{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.OffHeap as Manual
import Data.OffHeap (Pool)
import Foreign.Storable (Storable)
import qualified OffHeap.List as List
import OffHeap.List (List)
import Prelude.Linear
import Test.Hspec
import Test.QuickCheck

eqList :: forall a. (Storable a, Movable a, Eq a) => List a ->. List a ->. Pool ->. Unrestricted Bool
eqList l1 l2 pool =
    eqUL (move (List.toList l1)) (move (List.toList l2)) (consume pool)
  where
    eqUL :: Unrestricted [a] ->. Unrestricted [a] ->. () ->. Unrestricted Bool
    eqUL (Unrestricted as1) (Unrestricted as2) () = Unrestricted (as1 == as2)

main :: IO ()
main = hspec $ do
  describe "Off-heap lists" $ do
    describe "map" $ do
      it "of identity if the identity" $
        property (\(l :: [Bool]) -> unUnrestricted (Manual.withPool $ \pool ->
          let
            mapId = uncurry (List.map (\x -> x))

            check :: (List Bool, Pool) ->. Unrestricted Bool
            check (l', pool) = check' l' (List.ofList l pool)

            check' :: List Bool ->. (List Bool, Pool) ->. Unrestricted Bool
            check' l1 (l2, pool) = eqList l1 l2 pool
          in
            check $ mapId $ List.ofList l pool ))