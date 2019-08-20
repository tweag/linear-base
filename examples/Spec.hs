{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
import Control.Monad as P (void)
import qualified Data.Array.Destination as DArray
import qualified Data.Array.Polarized.Pull as Pull
import qualified Data.Array.Polarized.Push as Push
import qualified Data.Array.Polarized as Polarized
import qualified Data.List as L
import Data.Typeable
import qualified Data.Vector as Vector
import qualified Foreign.Heap as Heap
import Foreign.List (List)
import qualified Foreign.List as List
import Foreign.Marshal.Pure (Pool)
import qualified Foreign.Marshal.Pure as Manual
import Prelude (return)
import qualified Prelude as P
import Prelude.Linear
import Test.Hspec
import Test.QuickCheck

eqList :: forall a. (Manual.Representable a, Movable a, Eq a) => List a ->. List a ->. Unrestricted Bool
eqList l1 l2 =
    eqUL (move (List.toList l1)) (move (List.toList l2))
  where
    eqUL :: Unrestricted [a] ->. Unrestricted [a] ->. Unrestricted Bool
    eqUL (Unrestricted as1) (Unrestricted as2) = Unrestricted (as1 == as2)

data InjectedError = InjectedError
  deriving (Typeable, Show)

instance Exception InjectedError

main :: IO ()
main = hspec P.$ do
  describe "Off-heap lists" P.$ do
    describe "ofList" P.$ do
      it "is invertible" P.$
        property (\(l :: [Int]) -> unUnrestricted (Manual.withPool $ \pool ->
          let
            check :: Unrestricted [Int] ->. Unrestricted Bool
            check (Unrestricted l') = Unrestricted P.$ l' == l
          in
            check $ move (List.toList $ List.ofList l pool)))

    describe "map" P.$ do
      it "of identity is the identity" P.$
        property (\(l :: [Int]) -> unUnrestricted (Manual.withPool $ \pool ->
          let
            check :: (Pool, Pool, Pool) ->. Unrestricted Bool
            check (pool1, pool2, pool3) =
              eqList
                (List.map (\x -> x) (List.ofList l pool1) pool2)
                (List.ofList l pool3)
          in
            check (dup3 pool)))

    -- XXX: improve the memory corruption test by adding a 'take n' for a random
    -- 'n' before producing an error.
    describe "exceptions" P.$ do
      it "doesn't corrupt memory" P.$ do
        property (\(l :: [Int]) -> do
          let l' = l ++ (throw InjectedError)
          catch @InjectedError
            (P.void P.$ evaluate
               (Manual.withPool $ \pool ->
                   move (List.toList $ List.ofRList l' pool)))
            (\ _ -> return ())
           )


  describe "Off-heap heaps" P.$ do
    describe "sort" P.$ do
      it "sorts" P.$
        property (\(l :: [(Int, ())]) -> Heap.sort l == (L.reverse P.$ L.sort l))

  describe "Linear arrays" P.$ do
    describe "destination arrays" P.$ do
      describe "alloc, mirror" P.$
        it "roundtrips correctly" P.$
          property (\(l :: [Int]) ->
            let xs = Vector.fromList l
                n = Vector.length xs
             in DArray.alloc n (DArray.mirror xs id) == xs)

      describe "replicate" P.$
        it "replicates" P.$
          property (\(x :: Int) (NonNegative n) ->
            DArray.alloc n (DArray.replicate x) == Vector.replicate n x)

      describe "fill" P.$
        it "fills one slot" P.$
          property (\(x :: Int) ->
            DArray.alloc 1 (DArray.fill x) == Vector.singleton x)

      describe "fromFunction" P.$ do
        it "mimics Vector.enumFromN" P.$
          property (\(start :: Int) (NonNegative n) ->
            DArray.alloc n (DArray.fromFunction (+start) n) == Vector.enumFromN start n)
        it "matches Vector.generate" P.$
          property (\(f :: Fun Int Int) (NonNegative n) ->
            DArray.alloc n (DArray.fromFunction (applyFun f) n) == Vector.generate n (applyFun f))

    describe "polarized arrays" P.$ do
      describe "conversions" P.$ do
        it "roundtrips correctly" P.$
          property (\(l :: [Int]) ->
            let xs = Vector.fromList l
             in Push.alloc (Polarized.transfer (Pull.fromVector xs)) == xs)

    describe "push arrays" P.$ do
      describe "append" P.$
        it "matches Vector append" P.$
          property (\(x :: [Int]) (y :: [Int]) ->
            let xs = Vector.fromList x
                ys = Vector.fromList y
             in Push.alloc (Polarized.walk xs <> Polarized.walk ys) == xs Vector.++ ys)

      describe "make" P.$
        it "matches Vector.replicate" P.$
          property (\(x :: Int) (NonNegative n) ->
            Push.alloc (Push.make x n) == Vector.replicate n x)

      -- TODO: figure out a way to test fmap on push arrays (linearity issues at the moment)

    describe "pull arrays" P.$ do
      describe "append" P.$
        it "matches Vector append" P.$
          property (\(x :: [Int]) (y :: [Int]) ->
            let xs = Vector.fromList x
                ys = Vector.fromList y
             in Pull.toVector (Pull.fromVector xs <> Pull.fromVector ys) == xs Vector.++ ys)

      describe "asList" P.$
        it "roundtrips correctly" P.$
          property (\(x :: [Int]) ->
            Pull.asList (Pull.fromVector (Vector.fromList x)) == x)

      describe "singleton" P.$
        it "matches list singleton" P.$
          property (\(x :: Int) -> Pull.asList (Pull.singleton x) == [x])

      describe "split" P.$
        it "matches list splitAt" P.$
          property (\(x :: [Int]) (NonNegative n) ->
            let xs = Pull.fromVector (Vector.fromList x)
                (l,r) = Pull.split n xs
             in (Pull.asList l, Pull.asList r) == splitAt n x)

      -- TODO: test `zip` to make sure it does error when given different lengths
      describe "zip" P.$
        it "doesn't fail for equal lengths" P.$
          property (\(x :: [(Int, Int)])  ->
            let (x',y') = unzip x
                xs = Pull.fromVector (Vector.fromList x')
                ys = Pull.fromVector (Vector.fromList y')
             in Pull.asList (Pull.zip xs ys) == zip x' y')

      describe "make" P.$
        it "matches list replicate" P.$
          property (\(x :: Int) (NonNegative n) ->
            Pull.asList (Pull.make x n) == replicate n x)
