module Main (main) where

import qualified Bench.Compact.Pure as Compact
import Bench.Compact.SExpr
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import System.Environment
import Test.Tasty.Bench (defaultMain)

-- Launch regular benchmark with
-- stack bench linear-base:bench:memory --ba '+RTS -T'

-- Profile parseWithoutDest with
-- stack bench --library-profiling --executable-profiling --ghc-options '-fprof-late' linear-base:bench:memory --ba '+RTS -p -RTS runParseWithoutDest'

-- Profile parseUsingDest with
-- stack bench --library-profiling --executable-profiling --ghc-options '-fprof-late' linear-base:bench:memory --ba '+RTS -p -RTS runParseUsingDest'

-- remove useless lines in profiling results with
-- .*?0\.0    0\.0     0\.0    0\.0\n

-- remove all lines with no individual contribution to alloc with
-- .*?([0-9]+\.[0-9])\s+?0\.0\s+?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\n

-- remove all lines with no inherited contribution to alloc with
-- .*?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\s+?0\.0\n

main :: IO ()
main = do
  args <- getArgs
  case args of
    "runParseWithoutDest" : _ -> do
      sampleData <- loadSampleData
      let res = parseWithoutDest sampleData
      evaluate $ rnf $ res
    "runParseUsingDest" : _ -> do
      sampleData <- loadSampleData
      let res = parseUsingDest sampleData
      evaluate $ rnf $ res
    _ ->
      defaultMain
        [ Compact.benchmarks
        ]
