{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- Uncomment the line below to observe the generated (optimised) Core. It will
-- land in a file named “Array.dump-simpl”
-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dsuppress-uniques #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mutable.Array (benchmarks) where

import Control.DeepSeq (rnf)
import qualified Data.Array.Mutable.Linear as Array.Linear
import qualified Data.Array.Mutable.Linear as Array.Linear.Array
import Data.Functor.Compose
import Data.Kind
import qualified Data.Unrestricted.Linear as Linear
import qualified Data.Vector
import qualified Prelude.Linear as Linear
import Test.Tasty.Bench

dontFuse :: a -> a
dontFuse a = a
{-# NOINLINE dontFuse #-}

arr_size :: Int
arr_size = 1_000

benchmarks :: Benchmark
benchmarks =
  bgroup
    "arrays"
    [ runImpls "alloc" bAlloc arr_size,
      runImpls "toList" bToList arr_size,
      runImpls "map" bMap arr_size,
      runImpls "reads" bReads arr_size,
      runImpls "successive writes (very unfair to vector)" bSets arr_size
    ]

--------------------------------------------------------------------------------

data Impls
  = Impls
      (Array.Linear.Array Int %1 -> ())
      (Data.Vector.Vector Int -> ())

runImpls :: String -> Impls -> Int -> Benchmark
runImpls name impls sz0 =
  let Impls linear dataVector = impls
   in bgroup
        name
        [ bench "Data.Array.Mutable.Linear" $ whnf (runLinear linear) sz0,
          bench "Data.Vector" $ whnf (runDataVector dataVector) sz0
        ]
  where
    runLinear :: (Array.Linear.Array Int %1 -> ()) -> Int -> ()
    runLinear cb sz = Linear.unur (Array.Linear.alloc sz 0 (\a -> Linear.move (cb a)))

    runDataVector :: (Data.Vector.Vector Int -> ()) -> Int -> ()
    runDataVector cb sz = cb (Data.Vector.replicate sz 0)

type ArrayThing :: (Type -> Type) -> Constraint
class ArrayThing arr where
  size :: arr a %1 -> (Linear.Ur Int, arr a)
  get :: Int -> arr a %1 -> (Linear.Ur a, arr a)
  set :: Int -> a -> arr a %1 -> arr a
  toList :: arr a %1 -> Linear.Ur [a]
  amap :: (a -> b) -> arr a %1 -> arr b

  -- | Note: I [Arnaud Spiwack] initially thought I could use
  -- 'Consumable'/'consume' for this. But it doesn't work because the natural
  -- 'consume' function for `Ur x` doesn't evaluate the `x` at all. We need to
  -- evaluate the `x` in the 'Vector' instance.
  force :: arr a %1 -> ()

instance ArrayThing Array.Linear.Array where
  size = Array.Linear.Array.size
  get = Array.Linear.Array.unsafeGet
  set = Array.Linear.Array.unsafeSet
  toList = Array.Linear.Array.toList
  amap = Array.Linear.Array.map
  force = Linear.consume

instance ArrayThing (Compose Linear.Ur Data.Vector.Vector) where
  size (Compose (Linear.Ur v)) = (Linear.Ur (Data.Vector.length v), Compose (Linear.Ur v))
  get i (Compose (Linear.Ur v)) = (Linear.Ur (v Data.Vector.! i), Compose (Linear.Ur v))
  set i a (Compose (Linear.Ur v)) = Compose (Linear.Ur (v Data.Vector.// [(i, a)]))
  toList (Compose (Linear.Ur v)) = Linear.Ur (Data.Vector.toList v)
  amap f (Compose (Linear.Ur v)) = Compose (Linear.Ur (Data.Vector.map f v))
  force (Compose (Linear.Ur v)) = v `seq` ()

cleanup :: ((Compose Linear.Ur f a) %1 -> b) -> (f a -> b)
cleanup k a = k (Compose (Linear.Ur a))

--------------------------------------------------------------------------------

bToList :: Impls
bToList = Impls linear dataVector
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl arr =
      arr
        Linear.& toList
        Linear.& Linear.lift rnf
        Linear.& Linear.unur

    linear :: Array.Linear.Array Int %1 -> ()
    linear = impl

    dataVector :: Data.Vector.Vector Int -> ()
    dataVector = cleanup impl
{-# NOINLINE bToList #-}

bMap :: Impls
bMap = Impls linear dataVector
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl arr =
      arr
        Linear.& amap (+ 1)
        Linear.& get 5
        Linear.& \case
          (Linear.Ur _, arr') -> force arr'

    linear :: Array.Linear.Array Int %1 -> ()
    linear = impl

    dataVector :: Data.Vector.Vector Int -> ()
    dataVector = cleanup impl
{-# NOINLINE bMap #-}

bReads :: Impls
bReads = Impls linear dataVector
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl arr0 =
      arr0
        Linear.& size
        Linear.& \(Linear.Ur sz, arr) ->
          arr
            Linear.& go 0 sz
      where
        go :: (ArrayThing arr) => Int -> Int -> arr Int %1 -> ()
        go start end arr
          | start < end =
              get start arr
                Linear.& \(Linear.Ur i, arr') -> i `Linear.seq` go (start + 1) end arr'
          | otherwise = force arr

    linear :: Array.Linear.Array Int %1 -> ()
    linear = impl

    dataVector :: Data.Vector.Vector Int -> ()
    dataVector = cleanup impl
{-# NOINLINE bReads #-}

bAlloc :: Impls
bAlloc = Impls linear dataVector
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl = force

    linear :: Array.Linear.Array Int %1 -> ()
    linear = impl

    dataVector :: Data.Vector.Vector Int -> ()
    dataVector = cleanup impl
{-# NOINLINE bAlloc #-}

bSets :: Impls
bSets = Impls linear dataVector
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl arr0 =
      case size arr0 of
        (Linear.Ur sz, arr) -> go 0 sz arr
      where
        go :: (ArrayThing arr) => Int -> Int -> arr Int %1 -> ()
        go start end arr
          | start < end =
              go (start + 1) end Linear.$ set start 42 arr
          | otherwise = force arr

    linear :: Array.Linear.Array Int %1 -> ()
    linear = impl

    dataVector :: Data.Vector.Vector Int -> ()
    dataVector = cleanup impl
