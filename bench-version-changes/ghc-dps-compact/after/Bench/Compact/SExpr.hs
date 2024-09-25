module Bench.Compact.SExpr where

import Bench.Compact.Utils as Utils
import Compact.SExpr as SExpr
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.ByteString.Char8 as BSC
import Test.Tasty.Bench (Benchmark)

dataSetDir :: String
dataSetDir = "bench-version-changes/ghc-dps-compact/after/datasets/"

dataSets :: [(IO BSC.ByteString, String)]
dataSets =
  [ (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_10.sexpr"), "2^10"),
    (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_13.sexpr"), "2^13"),
    (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_16.sexpr"), "2^16"),
    (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_19.sexpr"), "2^19"),
    (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_22.sexpr"), "2^22")
  ]

sexprBenchgroup :: Benchmark
sexprBenchgroup = Utils.benchImpls "S-expression parser" SExpr.impls dataSets
