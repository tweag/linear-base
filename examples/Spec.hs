{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
import Control.Monad (void)
import qualified Data.Array.Destination as DArray
import qualified Data.Array.Polarized.Pull as Pull
import qualified Data.Array.Polarized.Push as Push
import qualified Data.Array.Polarized as Polarized
import Data.Typeable
import qualified Data.Vector as Vector
import qualified Foreign.Heap as Heap
import Foreign.List (List)
import qualified Foreign.List as List
import Foreign.Marshal.Pure (Pool)
import qualified Foreign.Marshal.Pure as Manual
import Prelude (return)
import qualified Prelude
import Prelude.Linear hiding (void)
import Test.Hspec
import Test.QuickCheck

eqList :: forall a. (Manual.Representable a, Movable a, Eq a) => List a #-> List a #-> Ur Bool
eqList l1 l2 = move $ (List.toList l1) == (List.toList l2)

data InjectedError = InjectedError
  deriving (Typeable, Show)

instance Exception InjectedError

main :: IO ()
main = hspec Prelude.$ do
  describe "Off-heap lists" Prelude.$ do
    describe "ofList" Prelude.$ do
      it "is invertible" Prelude.$
        property (\(l :: [Int]) -> unur (Manual.withPool $ \pool ->
          let
            check :: Ur [Int] #-> Ur Bool
            check (Ur l') = move $ l' == l
          in
            check $ move (List.toList $ List.ofList l pool)))

    describe "map" Prelude.$ do
      it "of identity is the identity" Prelude.$
        property (\(l :: [Int]) -> unur (Manual.withPool $ \pool ->
          let
            check :: (Pool, Pool, Pool) #-> Ur Bool
            check (pool1, pool2, pool3) =
              eqList
                (List.map (\x -> x) (List.ofList l pool1) pool2)
                (List.ofList l pool3)
          in
            check (dup3 pool)))

    -- XXX: improve the memory corruption test by adding a 'take n' for a random
    -- 'n' before producing an error.
    describe "exceptions" Prelude.$ do
      it "doesn't corrupt memory" Prelude.$ do
        property (\(l :: [Int]) -> do
          let l' = l ++ (throw InjectedError)
          catch @InjectedError
            (void Prelude.$ evaluate
               (Manual.withPool $ \pool ->
                   move (List.toList $ List.ofRList l' pool)))
            (\ _ -> return ())
           )


  describe "Off-heap heaps" Prelude.$ do
    describe "sort" Prelude.$ do
      it "sorts" Prelude.$
        property (\(l :: [(Int, ())]) -> Heap.sort l == (reverse $ sort l))

  describe "Linear arrays" Prelude.$ do
    describe "destination arrays" Prelude.$ do
      describe "alloc, mirror" Prelude.$
        it "roundtrips correctly" Prelude.$
          property (\(l :: [Int]) ->
            let xs = Vector.fromList l
                n = Vector.length xs
             in DArray.alloc n (DArray.mirror xs id) Prelude.== xs)

      describe "replicate" Prelude.$
        it "replicates" Prelude.$
          property (\(x :: Int) (NonNegative n) ->
            DArray.alloc n (DArray.replicate x) Prelude.== Vector.replicate n x)

      describe "fill" Prelude.$
        it "fills one slot" Prelude.$
          property (\(x :: Int) ->
            DArray.alloc 1 (DArray.fill x) Prelude.== Vector.singleton x)

      describe "fromFunction" Prelude.$ do
        it "mimics Vector.enumFromN" Prelude.$
          property (\(start :: Int) (NonNegative n) ->
            DArray.alloc n (DArray.fromFunction (Prelude.+ start)) Prelude.== Vector.enumFromN start n)
        it "matches Vector.generate" Prelude.$
          property (\(f :: Fun Int Int) (NonNegative n) ->
            DArray.alloc n (DArray.fromFunction (applyFun f)) Prelude.== Vector.generate n (applyFun f))

    describe "polarized arrays" Prelude.$ do
      describe "conversions" Prelude.$ do
        it "roundtrips correctly" Prelude.$
          property (\(l :: [Int]) ->
            let xs = Vector.fromList l
             in Push.alloc (Polarized.transfer (Pull.fromVector xs)) Prelude.== xs)

    describe "push arrays" Prelude.$ do
      describe "append" Prelude.$
        it "matches Vector append" Prelude.$
          property (\(x :: [Int]) (y :: [Int]) ->
            let xs = Vector.fromList x
                ys = Vector.fromList y
             in Push.alloc (Polarized.walk xs <> Polarized.walk ys) Prelude.== xs Vector.++ ys)

      describe "make" Prelude.$
        it "matches Vector.replicate" Prelude.$
          property (\(x :: Int) (NonNegative n) ->
            Push.alloc (Push.make x n) Prelude.== Vector.replicate n x)

      -- TODO: figure out a way to test fmap on push arrays (linearity issues at the moment)

    describe "pull arrays" Prelude.$ do
      describe "append" Prelude.$
        it "matches Vector append" Prelude.$
          property (\(x :: [Int]) (y :: [Int]) ->
            let xs = Vector.fromList x
                ys = Vector.fromList y
             in Pull.toVector (Pull.fromVector xs <> Pull.fromVector ys) Prelude.== xs Vector.++ ys)

      describe "asList" Prelude.$
        it "roundtrips correctly" Prelude.$
          property (\(x :: [Int]) ->
            Pull.asList (Pull.fromVector (Vector.fromList x)) == x)

      describe "singleton" Prelude.$
        it "matches list singleton" Prelude.$
          property (\(x :: Int) -> Pull.asList (Pull.singleton x) == [x])

      describe "split" Prelude.$
        it "matches list splitAt" Prelude.$
          property (\(x :: [Int]) (NonNegative n) ->
            let xs = Pull.fromVector (Vector.fromList x)
                (l,r) = Pull.split n xs
             in (Pull.asList l, Pull.asList r) == splitAt n x)

      -- TODO: test `zip` to make sure it does error when given different lengths
      describe "zip" Prelude.$
        it "doesn't fail for equal lengths" Prelude.$
          property (\(x :: [(Int, Int)])  ->
            let (x',y') = unzip x
                xs = Pull.fromVector (Vector.fromList x')
                ys = Pull.fromVector (Vector.fromList y')
             in Pull.asList (Pull.zip xs ys) == zip x' y')

      describe "make" Prelude.$
        it "matches list replicate" Prelude.$
          property (\(x :: Int) (NonNegative n) ->
            Pull.asList (Pull.make x n) == replicate n x)
