module Test.Compact where

import Test.Compact.BFTraversal (bfTraversalTests)
import Test.Compact.DList (dlistTests)
import Test.Compact.Map (mapTests)
import Test.Compact.Queue (queueTests)
import Test.Compact.SExpr (sexprTests)

import Test.Tasty

compactTests :: TestTree
compactTests =
    testGroup
        "DPS interface for compact regions"
        [ bfTraversalTests
        , dlistTests
        , mapTests
        , queueTests
        , sexprTests
        ]
