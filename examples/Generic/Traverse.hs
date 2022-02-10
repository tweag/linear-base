{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Generic.Traverse (genericTraverseTests) where

import Data.Functor.Linear (genericTraverse)
import qualified Data.Functor.Linear as Data
import Generics.Linear.TH
import Hedgehog
import Prelude.Linear
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import qualified Prelude

data Pair a = MkPair a a
  deriving (Show, Prelude.Eq)

$(deriveGeneric1 ''Pair)

instance Data.Functor Pair where
  fmap f (MkPair x y) = MkPair (f x) (f y)

instance Data.Traversable Pair where
  traverse = genericTraverse

pairTest :: TestTree
pairTest =
  testProperty "traverse via genericTraverse with WithLog and Pair" $
    property $
      ( Data.traverse
          (\x -> (Adding (1 :: Int), 2 * x))
          (MkPair 3 4 :: Pair Int)
      )
        === (Adding 2, (MkPair 6 8))

genericTraverseTests :: TestTree
genericTraverseTests =
  testGroup
    "genericTraverse examples"
    [pairTest]
