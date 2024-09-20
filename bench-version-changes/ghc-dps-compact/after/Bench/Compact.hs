module Bench.Compact where

import Test.Tasty.Bench

import Bench.Compact.BFTraversal (bftraversalBenchgroup)
import Bench.Compact.Map (mapBenchgroup)
import Bench.Compact.DList (dlistBenchgroup)
import Bench.Compact.Queue (queueBenchgroup)
import Bench.Compact.SExpr (sexprBenchgroup)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "DPS interface for compact regions"
    [ bftraversalBenchgroup
    , mapBenchgroup
    , dlistBenchgroup
    , queueBenchgroup
    , sexprBenchgroup
    ]
