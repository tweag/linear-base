{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Bench.Compact.Map where

import Bench.Compact.Utils as Utils
import Compact.Map as Map
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Test.Tasty.Bench (Benchmark)

dataSets :: [(IO [Int], String)]
dataSets =
  [ ((evaluate $ force [1 .. 2 ^ 10]), "2^10"),
    ((evaluate $ force [1 .. 2 ^ 13]), "2^13"),
    ((evaluate $ force [1 .. 2 ^ 16]), "2^16"),
    ((evaluate $ force [1 .. 2 ^ 19]), "2^19"),
    ((evaluate $ force [1 .. 2 ^ 22]), "2^22")
  ]

mapBenchgroup :: Benchmark
mapBenchgroup = benchImpls "map on List" Map.impls dataSets
