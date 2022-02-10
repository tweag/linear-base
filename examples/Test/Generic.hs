module Test.Generic (genericTests) where

import Generic.Traverse (genericTraverseTests)
import Test.Tasty

genericTests :: TestTree
genericTests =
  testGroup
    "Generic tests"
    [ genericTraverseTests
    ]
