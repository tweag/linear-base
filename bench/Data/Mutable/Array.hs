{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module Data.Mutable.Array (benchmarks) where

import Gauge
import Data.Function ((&))
import qualified Data.Unrestricted.Linear as Linear
import Data.List (foldl')
import qualified Prelude.Linear as Linear

import qualified Data.Array.Mutable.Linear as Array.Linear
import qualified Data.Vector


arr_size :: Int
arr_size = 10_000_000

benchmarks :: Benchmark
benchmarks = bgroup "arrays"
  [ runImpls "map" bMap arr_size
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

bMap :: Impls
bMap = Impls linear dataVector
  where
   linear :: Array.Linear.Array Int %1-> ()
   linear hm =
     hm
       Linear.& Array.Linear.map (+1)
       Linear.& Array.Linear.toList
       Linear.& Linear.lift (foldl' (+) 0)
       Linear.& Linear.unur
       Linear.& (`Linear.lseq` ())

   dataVector :: Data.Vector.Vector Int -> ()
   dataVector hm =
     hm
       & Data.Vector.map (+1)
       & Data.Vector.toList
       & foldl' (+) 0
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
