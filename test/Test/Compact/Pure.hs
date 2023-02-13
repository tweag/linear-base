{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Compact.Pure (compactPureTests) where

import Compact.Pure
import Control.Exception (evaluate)
import Control.Monad (return, (>>), (>>=))
import Data.Replicator.Linear (Replicator)
import qualified Data.Replicator.Linear as Replicator
import GHC.Compact (Compact, compact, compactAdd, getCompact)
import Prelude.Linear
import Test.Tasty
import Test.Tasty.HUnit
import Prelude ((<$>))
import Control.Functor.Linear ((<&>))

compactPureTests :: TestTree
compactPureTests =
  testGroup
    "Demo for compact regions and destinations"
    [testCaseInfo "Compact region demo" compactDemo]

-- Launch with

-- $ stack test --ta '-p "Compact region demo"'

compactDemo :: IO String
compactDemo = do
  let s1 :: Ur (Int, Int)
      !s1 = withRegion $ \r ->
             case dup r of
              (r1, r2) ->
                complete $
                  (alloc r1)
                    <&> (\dp -> case fillPair dp of (dl, dr) -> fillLeaf dl 1 `lseq` dr)
                    <&> (`fillComp` (alloc r2))
                    <&> (\dr -> fillLeaf dr 2)
  let s2 :: Ur (Int, (Int, Int))
      !s2 =
        withRegion $ \r ->
          case dup r of
            (r1, r2) ->
              complete $
                (alloc r1)
                  <&> (\dp -> case fillPair dp of (dl, dr) -> fillLeaf dl 1 `lseq` dr)
                  <&> (`fillComp` ((alloc r2) <&> (\dr -> case fillPair dr of (dr1, dr2) -> fillLeaf dr1 2 `lseq` dr2)))
                  <&> (\dr2 -> fillLeaf dr2 3)
  putStrLn "\nConstruction ok\n"
  dispHeap s1
  putStrLn $ "\n==================================================\n"
  dispHeap s2

  return "Done"
