module Bench.Compact.BFTraversal where

import Compact.BFTraversal as BFTraversal
import Bench.Compact.Utils as Utils
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Test.Tasty.Bench (Benchmark)

dataSets :: [(IO (BinTree ()), String)]
dataSets =
  [ (evaluate $ force (go 0 10), "2^10")
  , (evaluate $ force (go 0 13), "2^13")
  , (evaluate $ force (go 0 16), "2^16")
  , (evaluate $ force (go 0 19), "2^19")
  , (evaluate $ force (go 0 22), "2^22")
  ]
  where
      go :: Int -> Int -> BinTree ()
      go currentDepth maxDepth =
        if currentDepth >= maxDepth
          then Nil
          else Node () (go (currentDepth + 1) maxDepth) (go (currentDepth + 1) maxDepth)

bftraversalBenchgroup = Utils.benchImpls BFTraversal.impls dataSets
