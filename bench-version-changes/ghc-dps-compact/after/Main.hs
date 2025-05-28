module Main where

import qualified Bench.Compact as Compact
import qualified Bench.Data.Array.Mutable as Array
import qualified Bench.Data.Array.Mutable.Quicksort as Quicksort
import qualified Bench.Data.HashMap.Mutable as HashMap
import System.Environment (getArgs)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["manualLaunch", request] ->
      Compact.launchImpl request
    _ ->
      defaultMain
        [ Array.benchmarks,
          Quicksort.benchmarks,
          HashMap.benchmarks,
          Compact.benchmarks
        ]

-- nix-shell --arg installHls 'false' --pure --run "
-- echo \$'=== Benchmarks (manual launch, filtered) ===\n\n' > benchmark_ghc-dps-compact-95615577d7_prof.txt && \
-- cabal run -w /home/thomas/tweag/linear-base/ghc-dps-compact-95615577d7/bin/ghc -v0 linear-base:bench:bench -- -l | \
-- while read -r name; do \
--   case \"\$name\" in \
--     All.DPS\ interface\ for\ compact\ regions*) \
--       for i in {1..5}; do \
--         echo \"=== Running \$name (iteration \$i) ===\" | tee -a benchmark_ghc-dps-compact-95615577d7_prof.txt; \
--         cabal run -w /home/thomas/tweag/linear-base/ghc-dps-compact-95615577d7/bin/ghc -v0 linear-base:bench:bench -- manualLaunch \"\$name\" +RTS -T -S -RTS 2>&1 | tee -a benchmark_ghc-dps-compact-95615577d7_prof.txt; \
--       done \
--       ;; \
--     *) ;; \
--   esac; \
-- done
-- "
