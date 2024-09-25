{-# OPTIONS_GHC -Wno-type-defaults #-}

module Bench.Compact.DList where

import Bench.Compact.Utils as Utils
import Compact.DList as DList
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Test.Tasty.Bench (Benchmark)

dataSets :: [(IO [[Int]], String)]
dataSets =
  [ (evaluate $ force (fmap (\i -> [(10 * i + 0) .. (10 * i + 9)]) [0 .. (((2 ^ 10) `div` 10) - 1)]), "2^10"),
    (evaluate $ force (fmap (\i -> [(10 * i + 0) .. (10 * i + 9)]) [0 .. (((2 ^ 13) `div` 10) - 1)]), "2^13"),
    (evaluate $ force (fmap (\i -> [(10 * i + 0) .. (10 * i + 9)]) [0 .. (((2 ^ 16) `div` 10) - 1)]), "2^16"),
    (evaluate $ force (fmap (\i -> [(10 * i + 0) .. (10 * i + 9)]) [0 .. (((2 ^ 19) `div` 10) - 1)]), "2^19"),
    (evaluate $ force (fmap (\i -> [(10 * i + 0) .. (10 * i + 9)]) [0 .. (((2 ^ 22) `div` 10) - 1)]), "2^22")
  ]

dlistBenchgroup :: Benchmark
dlistBenchgroup = benchImpls "List and DList concatenation" DList.impls dataSets
