{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Mutable.Array (mutArrTests) where

import Hedgehog

mutArrTests :: IO Bool
mutArrTests = checkParallel group

group :: Group
group = Group "Mutable array tests" $
  []



