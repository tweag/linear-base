module Bench.Compact.SExpr where

import Compact.SExpr as SExpr
import Bench.Compact.Utils as Utils
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import qualified Data.ByteString.Char8 as BSC
import Test.Tasty.Bench (Benchmark)

dataSetDir :: String
dataSetDir = "bench-version-changes/ghc-dps-compact/after/datasets/"

dataSets :: [(IO ByteString, String)]
dataSets =
  [ (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_10.sexpr"), "2^10")
  , (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_13.sexpr"), "2^13")
  , (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_16.sexpr"), "2^16")
  , (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_19.sexpr"), "2^19")
  , (evaluate . force =<< BSC.readFile (dataSetDir ++ "data_2_22.sexpr"), "2^22")
  ]

sexprBenchgroup = Utils.benchImpls SExpr.impls dataSets
