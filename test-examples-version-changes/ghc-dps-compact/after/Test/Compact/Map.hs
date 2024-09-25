{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Compact.Map where

import Compact.Map
import Test.Compact.Utils
import Test.Tasty (TestTree)

dataset :: [Int]
dataset = [1 .. 2 ^ 10]

mapTests :: TestTree
mapTests = safetySameAsFirstImpl "Map function over list of integers" impls dataset
