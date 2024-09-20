module Test.Compact.BFTraversal where

import Compact.BFTraversal
import Test.Compact.Utils

dataset :: BinTree ()
dataset = Node
                        ()
                        (Node () (Leaf ()) (Leaf ()))
                        (Node () (Leaf ()) Nil)

expected :: (BinTree Int, Int)
                    expected =
                      ( Node
                          0
                          (Node 1 (Leaf 3) (Leaf 4))
                          (Node 2 (Leaf 5) Nil),
                        6
                      )

bftraversalTests :: TestTree
bftraversalTests =
  safetySameAsExpected "Breadth-first tree traversal" impls dataset expected