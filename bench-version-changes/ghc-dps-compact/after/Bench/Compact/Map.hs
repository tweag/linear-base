{-# LANGUAGE LinearTypes #-}
module Bench.Compact.Map where

import Compact.Map as Map
import Bench.Compact.Utils as Utils
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Test.Tasty.Bench (Benchmark)

dataSets :: [(IO (Int %1 -> Int, [Int]), String)]
dataSets =
  [ ((\x -> 2 * x + 1,) <$> (evaluate $ force [1 .. 2^10]), "2^10")
  , ((\x -> 2 * x + 1,) <$> (evaluate $ force [1 .. 2^13]), "2^13")
  , ((\x -> 2 * x + 1,) <$> (evaluate $ force [1 .. 2^16]), "2^16")
  , ((\x -> 2 * x + 1,) <$> (evaluate $ force [1 .. 2^19]), "2^19")
  , ((\x -> 2 * x + 1,) <$> (evaluate $ force [1 .. 2^22]), "2^22")
  ]

mapBenchgroup = benchImpls Map.impls dataSets
