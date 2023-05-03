{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
-- Uncomment the line below to observe the generated (optimised) Core. It will
-- land in a file named “Array.dump-simpl”
-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dsuppress-uniques #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.Mutable.Array (benchmarks) where

import Control.DeepSeq (rnf)
import qualified Data.Array.Mutable.Linear as Array.Linear
import qualified Data.Array.Mutable.Linear as Array.Linear.Array
import qualified Data.Foldable
import Data.Functor.Compose
import Data.Kind
import qualified Data.Sequence
import qualified Data.Unrestricted.Linear as Linear
import qualified Data.Vector
import Prelude.Linear (($), (&))
import qualified Prelude.Linear as Linear
import Test.Tasty.Bench
import Prelude hiding (($))

dontFuse :: a -> a
dontFuse a = a
{-# NOINLINE dontFuse #-}

arr_size :: Int
arr_size = 1_000

benchmarks :: Benchmark
benchmarks =
  bgroup
    "arrays"
    $ runImpls
      [ bAlloc,
        bToList,
        bMap,
        bReads,
        bSets
      ]

--------------------------------------------------------------------------------

data Impl where
  Impl :: String -> (forall arr. (ArrayThing arr) => arr Int %1 -> ()) -> Impl

runImpls :: [Impl] -> [Benchmark]
runImpls = map (runImpl arr_size)

runImpl :: Int -> Impl -> Benchmark
runImpl sz0 (Impl name impl) =
  bgroup
    name
    [ bench "Data.Array.Mutable.Linear" $ whnf (runLinear impl) sz0,
      bench "Data.Vector" $ whnf (runDataVector (cleanup impl)) sz0,
      bench "Data.Sequence" $ whnf (runSequence (cleanup impl)) sz0
    ]
  where
    runLinear :: (Array.Linear.Array Int %1 -> ()) -> Int -> ()
    runLinear cb sz = Linear.unur (Array.Linear.alloc sz 0 (\a -> Linear.move (cb a)))

    runDataVector :: (Data.Vector.Vector Int -> ()) -> Int -> ()
    runDataVector cb sz = cb (Data.Vector.replicate sz 0)

    runSequence :: (Data.Sequence.Seq Int -> ()) -> Int -> ()
    runSequence cb sz = cb (Data.Sequence.replicate sz 0)

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

type UArrayThing :: (Type -> Type) -> Constraint
class UArrayThing arr where
  usize :: arr a -> Int
  uget :: Int -> arr a -> a
  uset :: Int -> a -> arr a -> arr a
  utoList :: arr a -> [a]
  uamap :: (a -> b) -> arr a -> arr b
  uforce :: arr a -> ()

instance ArrayThing Array.Linear.Array where
  size = Array.Linear.Array.size
  get = Array.Linear.Array.unsafeGet
  set = Array.Linear.Array.unsafeSet
  toList = Array.Linear.Array.toList
  amap = Array.Linear.Array.map
  force = Linear.consume

instance (UArrayThing arr) => ArrayThing (Compose Linear.Ur arr) where
  size (Compose (Linear.Ur arr)) = (Linear.Ur (usize arr), Compose (Linear.Ur arr))
  get i (Compose (Linear.Ur arr)) = (Linear.Ur (uget i arr), Compose (Linear.Ur arr))
  set i a (Compose (Linear.Ur arr)) = Compose (Linear.Ur (uset i a arr))
  toList (Compose (Linear.Ur arr)) = Linear.Ur (utoList arr)
  amap f (Compose (Linear.Ur arr)) = Compose (Linear.Ur (uamap f arr))
  force (Compose (Linear.Ur arr)) = uforce arr

instance UArrayThing Data.Vector.Vector where
  usize = Data.Vector.length
  uget i v = v Data.Vector.! i
  uset i a v = v Data.Vector.// [(i, a)]
  utoList = Data.Vector.toList
  uamap = Data.Vector.map
  uforce = (`seq` ())

instance UArrayThing Data.Sequence.Seq where
  usize = Data.Sequence.length
  uget i s = Data.Sequence.index s i
  uset = Data.Sequence.update
  utoList = Data.Foldable.toList
  uamap = fmap

  -- I'm not sure about this one: on the one hand it forces the data structure
  -- to be allocated. On the other hand, it will do an extra traversal. Maybe
  -- there's a better comparison that can be done.
  uforce s = (foldMap (\_ -> Strict) s) `seq` ()

cleanup :: ((Compose Linear.Ur f a) %1 -> b) -> (f a -> b)
cleanup k a = k (Compose (Linear.Ur a))

data Strict = Strict

instance Semigroup Strict where
  Strict <> x = x

instance Monoid Strict where
  mempty = Strict

--------------------------------------------------------------------------------

bToList :: Impl
bToList = Impl "toList" impl
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl arr = arr & toList & Linear.lift rnf & Linear.unur
{-# NOINLINE bToList #-}

bMap :: Impl
bMap = Impl "map" impl
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl arr =
      case arr & amap (+ 1) & get 5 of
        (Linear.Ur _, arr') -> force arr'
{-# NOINLINE bMap #-}

bReads :: Impl
bReads = Impl "reads" impl
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl arr0 =
      case size arr0 of
        (Linear.Ur sz, arr) -> go 0 sz arr
      where
        go :: (ArrayThing arr) => Int -> Int -> arr Int %1 -> ()
        go start end arr
          | start < end =
              case get start arr of
                (Linear.Ur i, arr') -> i `Linear.seq` go (start + 1) end arr'
          | otherwise = force arr
{-# NOINLINE bReads #-}

bAlloc :: Impl
bAlloc = Impl "alloc" impl
  where
    impl :: (ArrayThing arr) => arr Int %1 -> ()
    impl = force
{-# NOINLINE bAlloc #-}

bSets :: Impl
bSets = Impl "successive writes (very unfair to vector)" impl
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
