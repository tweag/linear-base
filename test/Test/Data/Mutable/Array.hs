{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Mutable.Array (mutArrTests) where

import Hedgehog
import Control.Monad.Morph ( lift )
import Test.Tasty
import Test.Tasty.Hedgehog ( testProperty )

mutArrTests :: TestTree
mutArrTests = testGroup "Mutable array tests" group

group :: [TestTree]
group =
  [testProperty "always true property" alwaysTrue]

alwaysTrue :: Property
alwaysTrue = property $ lift $ return ()


