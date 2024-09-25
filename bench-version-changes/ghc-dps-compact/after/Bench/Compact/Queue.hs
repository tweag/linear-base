{-# OPTIONS_GHC -Wno-type-defaults #-}

module Bench.Compact.Queue where

import Bench.Compact.Utils as Utils
import Compact.Queue as Queue
import Data.Word (Word64)
import Test.Tasty.Bench (Benchmark)

dataSets :: [(IO Word64, String)]
dataSets =
  [ (return $ 2 ^ 10, "2^10"),
    (return $ 2 ^ 13, "2^13"),
    (return $ 2 ^ 16, "2^16"),
    (return $ 2 ^ 19, "2^19"),
    (return $ 2 ^ 22, "2^22")
  ]

queueBenchgroup :: Benchmark
queueBenchgroup = benchImpls "Queue enqueue operations" Queue.impls dataSets
