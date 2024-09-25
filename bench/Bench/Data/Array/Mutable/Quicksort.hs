{-# LANGUAGE NumericUnderscores #-}

module Bench.Data.Array.Mutable.Quicksort (benchmarks) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Array.Mutable.Quicksort (quicksortUsingArray, quicksortUsingList)
import Data.List (sort)
import System.Random
import Test.Tasty.Bench

-- Follows thread from https://discourse.haskell.org/t/linear-haskell-quicksort-performance/10280

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
              [ bench "quicksortUsingArray" $
                  nf quicksortUsingArray randomList,
                bench "quicksortUsingList" $
                  nf quicksortUsingList randomList,
                bench "sortStdLib" $
                  nf sort randomList
              ]
      )
        <$> sizes
    )
