{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Mutable.Vector (mutVecTests) where

import Hedgehog

mutVecTests :: IO Bool
mutVecTests = checkParallel group

group :: Group
group = Group "Mutable vector tests" $
  []



