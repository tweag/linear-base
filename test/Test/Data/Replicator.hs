{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module Test.Data.Replicator (replicatorInspectionTests) where

import Data.Replicator.Linear (Replicator)
import qualified Data.Replicator.Linear as Replicator
import Prelude.Linear
import Test.Tasty
import Test.Tasty.Inspection

replicatorInspectionTests :: TestTree
replicatorInspectionTests =
  testGroup
    "Inspection testing of elim for Replicator"
    [$(inspectTest $ 'elim3 === 'manualElim3)]

elim3 :: (a %1 -> a %1 -> a %1 -> [a]) %1 -> Replicator a %1 -> [a]
elim3 f r = Replicator.elim f r

manualElim3 :: (a %1 -> a %1 -> a %1 -> [a]) %1 -> Replicator a %1 -> [a]
manualElim3 f r =
  Replicator.next r & \case
    (x, r') ->
      Replicator.next r' & \case
        (y, r'') ->
          Replicator.extract r'' & \case
            z -> f x y z
