{-# LANGUAGE LinearTypes #-}
module Test.Compact.Map where

import Compact.Map
import Test.Compact.Utils

dataset :: (Int %1 -> Int, [Int])
dataset = (\x -> 2 * x + 1, [1 .. 2^10])

mapTests :: TestTree
mapTests = safetySameAsFirstImpl "Map function over list of integers" impls dataset
