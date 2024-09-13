{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.Functor.Linear (genericTests) where

import Data.Functor.Linear (genericTraverse)
import qualified Data.Functor.Linear as Data
import Generics.Linear.TH
import Hedgehog
import Prelude.Linear
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)
import qualified Prelude

data Pair a = MkPair a a
  deriving (Show, Prelude.Eq)

$(deriveGeneric1 ''Pair)

instance Data.Functor Pair where
  fmap f (MkPair x y) = MkPair (f x) (f y)

instance Data.Traversable Pair where
  traverse = genericTraverse

genericTests :: TestTree
genericTests =
  testGroup
    "Generic tests"
    [ genericTraverseTests
    ]

genericTraverseTests :: TestTree
genericTraverseTests =
  testGroup
    "genericTraverse examples"
    [pairTest]

pairTest :: TestTree
pairTest =
  testPropertyNamed "traverse via genericTraverse with WithLog and Pair" "propertyPairTest" propertyPairTest

propertyPairTest :: Property
propertyPairTest =
  property $
    ( Data.traverse
        (\x -> (Sum (1 :: Int), 2 * x))
        (MkPair 3 4 :: Pair Int)
    )
      === (Sum 2, (MkPair 6 8))
