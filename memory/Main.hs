module Main (main) where

import qualified Compact.Pure as Compact

import Gauge (defaultMain)
import Compact.SExpr

-- Launch with
-- stack bench linear-base:bench:memory --ba '--regress allocated:iters +RTS -T'

main :: IO ()
main = do
  defaultMain
    [ Compact.benchmarks
    ]
