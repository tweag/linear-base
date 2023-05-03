module Main (main) where

import qualified Compact.Pure as Compact

import Test.Tasty.Bench (defaultMain)
import Compact.SExpr

-- Launch with
-- stack bench linear-base:bench:memory --ba '+RTS -T'

main :: IO ()
main = do
  defaultMain
    [ Compact.benchmarks
    ]
