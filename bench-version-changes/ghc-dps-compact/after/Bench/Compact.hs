module Bench.Compact where

import Bench.Compact.BFTraversal (bftraversalBenchgroup, bftraversalLaunch)
import Bench.Compact.DList (dlistBenchgroup, dlistLaunch)
import Bench.Compact.Map (mapBenchgroup, mapLaunch)
import Bench.Compact.Queue (queueBenchgroup, queueLaunch)
import Bench.Compact.SExpr (sexprBenchgroup, sexprLaunch)
import System.Exit (exitFailure)
import Test.Tasty.Bench

benchmarks :: Benchmark
benchmarks =
  bgroup
    "DPS interface for compact regions"
    [ bftraversalBenchgroup,
      mapBenchgroup,
      dlistBenchgroup,
      queueBenchgroup,
      sexprBenchgroup
    ]

launchImpl' :: [(String -> Maybe (IO ()))] -> String -> IO ()
launchImpl' launchers request = do
  let tryLaunch [] = Nothing
      tryLaunch (l : ls) = case l request of
        Just action -> Just action
        Nothing -> tryLaunch ls
  case tryLaunch launchers of
    Just action -> do
      action
    Nothing -> do
      putStrLn $ "Error"
      exitFailure

launchImpl :: String -> IO ()
launchImpl =
  launchImpl'
    [ bftraversalLaunch,
      mapLaunch,
      dlistLaunch,
      queueLaunch,
      sexprLaunch
    ]
