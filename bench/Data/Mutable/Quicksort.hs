{-# LANGUAGE NumericUnderscores #-}

module Data.Mutable.Quicksort (benchmarks) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (sort)
import Simple.Quicksort (quickSort)
import System.Random
import Test.Tasty.Bench

-- Follows thread from https://discourse.haskell.org/t/linear-haskell-quicksort-performance/10280

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x : xs) = qs ltx ++ x : qs gex
  where
    ltx = [y | y <- xs, y < x]
    gex = [y | y <- xs, y >= x]

linArrayQuicksort, lazyListQuicksort, stdLibSort :: [Int] -> [Int]
linArrayQuicksort = quickSort
lazyListQuicksort = qs
stdLibSort = sort

gen :: StdGen
gen = mkStdGen 4541645642

randomListBuilder :: Int -> IO [Int]
randomListBuilder size = evaluate $ force $ take size (randoms gen :: [Int])

sizes :: [Int]
sizes = [1_000, 50_000, 1_000_000]

benchmarks :: Benchmark
benchmarks =
  bgroup
    "quicksort"
    ( ( \size ->
          env (randomListBuilder size) $ \randomList ->
            bgroup
              ("size " ++ (show size))
              [ bench "linArrayQuicksort" $
                  nf linArrayQuicksort randomList,
                bench "lazyListQuicksort" $
                  nf lazyListQuicksort randomList,
                bench "stdLibSort" $
                  nf stdLibSort randomList
              ]
      )
        <$> sizes
    )
