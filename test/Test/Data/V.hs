{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module Test.Data.V (vInspectionTests) where

import Data.V.Linear (V)
import qualified Data.V.Linear as V
import Prelude.Linear
import Test.Tasty
import Test.Tasty.Inspection

vInspectionTests :: TestTree
vInspectionTests =
  testGroup
    "Inspection testing of elim and make for V"
    [ $(inspectTest $ 'make3 ==- 'manualMake3),
      $(inspectTest $ 'elim3 ==- 'manualElim3)
    ]

make3 :: a %1 -> a %1 -> a %1 -> V 3 a
make3 = V.make

manualMake3 :: a %1 -> a %1 -> a %1 -> V 3 a
manualMake3 x y z = V.cons x . V.cons y . V.cons z $ V.empty

elim3 :: (a %1 -> a %1 -> a %1 -> [a]) %1 -> V 3 a %1 -> [a]
elim3 = V.elim

manualElim3 :: (a %1 -> a %1 -> a %1 -> [a]) %1 -> V 3 a %1 -> [a]
manualElim3 f v =
  case V.uncons v of
    (x, v') ->
      case V.uncons v' of
        (y, v'') ->
          case V.uncons v'' of
            (z, v''') ->
              case V.consume v''' of
                () -> f x y z
