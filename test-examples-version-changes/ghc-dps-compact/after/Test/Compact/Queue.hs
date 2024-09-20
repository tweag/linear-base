module Test.Compact.Queue where

import Compact.Queue
import Test.Compact.Utils

queueTests :: TestTree
queueTests = safetySameAsFirstImpl "Enqueue elements in a queue" impls (2^10)
