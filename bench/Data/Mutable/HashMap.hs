module Data.Mutable.HashMap (hmbench) where

import Gauge

hmbench :: Benchmark
hmbench = bgroup "Comparing hashmaps"
  [ bgroup "linear-base: Data.HashMap.Mutable.Linear" [linearbench]
  , bgroup "base: Data.Map.Strict" [purestrictbench]
  ]

linearbench :: Benchmark
linearbench = bench "" $ nf id ()

purestrictbench :: Benchmark
purestrictbench = bench "" $ nf id ()
