{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module Data.Mutable.Array (benchmarks) where

import Gauge
import Data.Function ((&))
import qualified Data.Unrestricted.Linear as Linear
import qualified Prelude.Linear as Linear
import Control.DeepSeq (rnf)

import qualified Data.Array.Mutable.Linear as Array.Linear
import qualified Data.Vector

dontFuse :: a -> a
dontFuse a = a
{-# NOINLINE dontFuse #-}

arr_size :: Int
arr_size = 10_000_000

benchmarks :: Benchmark
benchmarks = bgroup "arrays"
  [ runImpls "toList" bToList arr_size
  , runImpls "map" bMap arr_size
  , runImpls "reads" bReads arr_size
  ]

--------------------------------------------------------------------------------

data Impls =
  Impls
    (Array.Linear.Array Int %1-> ())
    (Data.Vector.Vector Int -> ())

runImpls :: String -> Impls -> Int -> Benchmark
runImpls name impls size =
  let Impls linear dataVector = impls
  in bgroup name
       [ bench "Data.Array.Mutable.Linear" $ whnf (runLinear linear) size
       , bench "Data.Vector" $ whnf (runDataVector dataVector) size
       ]
 where
  runLinear :: (Array.Linear.Array Int %1-> ()) -> Int -> ()
  runLinear cb sz = Linear.unur (Array.Linear.alloc sz 0 (\a -> Linear.move (cb a)))

  runDataVector :: (Data.Vector.Vector Int -> ()) -> Int -> ()
  runDataVector cb sz = cb (Data.Vector.replicate sz 0)

--------------------------------------------------------------------------------

bToList :: Impls
bToList = Impls linear dataVector
  where
   linear :: Array.Linear.Array Int %1-> ()
   linear hm =
     hm
       Linear.& Array.Linear.toList
       Linear.& Linear.lift rnf
       Linear.& Linear.unur

   dataVector :: Data.Vector.Vector Int -> ()
   dataVector hm =
     hm
       & Data.Vector.toList
       & rnf
{-# NOINLINE bToList #-}

bMap :: Impls
bMap = Impls linear dataVector
  where
   linear :: Array.Linear.Array Int %1-> ()
   linear hm =
     hm
       Linear.& Array.Linear.map (+1)
       Linear.& Array.Linear.unsafeGet 5
       Linear.& (`Linear.lseq` ())

   dataVector :: Data.Vector.Vector Int -> ()
   dataVector hm =
     hm
       & Data.Vector.map (+1)
       & dontFuse -- This looks like cheating, I know. But we're trying to measure
                  -- the speed of `map`, and without this, `vector` fuses the `map`
                  -- with the subsequent `index` to skip writing to the rest of the
                  -- vector.
       & (`Data.Vector.unsafeIndex` 5)
       & (`seq` ())
{-# NOINLINE bMap #-}

bReads :: Impls
bReads = Impls linear dataVector
  where
   linear :: Array.Linear.Array Int %1-> ()
   linear hm =
     hm
       Linear.& Array.Linear.size
       Linear.& \(Linear.Ur sz, arr) -> arr
       Linear.& go 0 sz
    where
     go :: Int -> Int -> Array.Linear.Array Int %1-> ()
     go start end arr
       | start < end =
           Array.Linear.unsafeGet start arr
             Linear.& \(Linear.Ur i, arr') -> i `Linear.seq` go (start + 1) end arr'
       | otherwise = arr `Linear.lseq` ()

   dataVector :: Data.Vector.Vector Int -> ()
   dataVector v =
     let sz = Data.Vector.length v
      in go 0 sz
    where
     go :: Int -> Int -> ()
     go start end
       | start < end =
           (v Data.Vector.! start) `seq`  go (start + 1) end
       | otherwise = ()
{-# NOINLINE bReads #-}
