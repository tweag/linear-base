{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Compact.Queue where

import Compact.Queue
import Test.Compact.Utils
import Test.Tasty (TestTree)

queueTests :: TestTree
queueTests = safetySameAsFirstImpl "Enqueue elements in a queue" impls (2 ^ 10)
