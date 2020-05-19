{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.Tasty
import Test.Data.Mutable.Array (mutArrTests)
import Test.Data.Mutable.Vector (mutVecTests)
import Test.Data.Mutable.HashMap (mutHMTests)
import Test.Data.Mutable.Set (mutSetTests)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests"
  [ mutArrTests
  , mutVecTests
  , mutHMTests
  , mutSetTests
  ]
