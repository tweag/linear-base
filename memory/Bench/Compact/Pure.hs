module Bench.Compact.Pure (benchmarks) where

import Bench.Compact.SExpr
import GHC.Compact (compact, getCompact)
import Test.Tasty.Bench

benchmarks :: Benchmark
benchmarks =
  bgroup
    "compact region allocs"
    [ bench "parser without dest" . nfIO $ do
        sampleData <- loadSampleData
        let res = parseWithoutDest sampleData
        resInRegion <- compact res
        return $ getCompact resInRegion,
      bench "parser using dest" . nfIO $ parseUsingDest <$> loadSampleData
    ]
