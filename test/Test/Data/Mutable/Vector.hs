{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Mutable.Vector (mutVecTests) where

import Hedgehog
import Control.Monad.Morph ( lift )
import Test.Tasty
import Test.Tasty.Hedgehog ( testProperty )

mutVecTests :: TestTree
mutVecTests = testGroup "Mutable vector tests" group

group :: [TestTree]
group =
  [testProperty "always true property" alwaysTrue]

alwaysTrue :: Property
alwaysTrue = property $ lift $ return ()

