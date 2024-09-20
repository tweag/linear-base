module Test.Compact.DList where

import Compact.DList
import Test.Compact.Utils

dataset :: [[Int]]
dataset = fmap (\i -> [(10 * i + 0)..(10 * i + 9)]) [0..(((2^10) `div` 10) - 1)]

dlistTests :: TestTree
dlistTests = safetySameAsFirstImpl "List & Difference lists concatenation" impls dataset