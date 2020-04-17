{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.Tasty

import Test.Data.Mutable.Array (mutArrTests)
import Test.Data.Mutable.Vector (mutVecTests)
import Test.Data.Mutable.HashMap (mutHMTests)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests"
  [ mutArrTests
  , mutVecTests
  , mutHMTests
  ]

