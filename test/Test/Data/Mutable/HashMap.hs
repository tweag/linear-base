{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Mutable.HashMap (mutHMTests) where

import Hedgehog
import Control.Monad.Morph ( lift )
import Test.Tasty
import Test.Tasty.Hedgehog ( testProperty )

mutHMTests :: TestTree
mutHMTests = testGroup "Mutable hashmap tests" group

group :: [TestTree]
group =
  [testProperty "always true property" alwaysTrue]

alwaysTrue :: Property
alwaysTrue = property $ lift $ return ()

