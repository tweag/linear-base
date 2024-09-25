module Bench.Compact where

import Test.Tasty.Bench

benchmarks :: Benchmark
benchmarks =
  bgroup
    "DPS interface for compact regions (unsupported in this GHC version)"
    []
