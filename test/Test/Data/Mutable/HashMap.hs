{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Mutable.HashMap (mutHMTests) where

import Hedgehog

mutHMTests :: IO Bool
mutHMTests = checkParallel group

group :: Group
group = Group "Mutable hashmap tests" $
  []



