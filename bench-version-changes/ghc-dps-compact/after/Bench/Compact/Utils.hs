{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bench.Compact.Utils where

import Control.DeepSeq
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseInfo, assertEqual)
import Test.Tasty.Bench
import Control.Exception (evaluate)
import Data.Functor ((<&>))
import GHC.Compact (compact, getCompact)

import qualified Compact.Map as Map
import qualified Compact.BFTraversal as BFTraversal
import qualified Compact.DList as DList
import qualified Compact.Queue as Queue
import qualified Compact.SExpr as SExpr

import qualified Bench.Compact.Map as Map
import qualified Bench.Compact.BFTraversal as BFTraversal
import qualified Bench.Compact.DList as DList
import qualified Bench.Compact.Queue as Queue
import qualified Bench.Compact.SExpr as SExpr

benchImpls :: forall m a r. (NFData r) => String -> [(a %m -> r, String, Bool)] -> [(IO a, String)] -> Benchmark
benchImpls name impls datasets = do
  bgroup name (
    datasets <&> \(loadSampleData, sizeName) -> env loadSampleData $ \sampleData ->
      testGroup sizeName $ concat $ impls <&> \(impl, implName, isLazy) -> if isLazy
          then
            [ bench (implName ++ ".force") $ (flip whnfAppIO) sampleData $ \sampleData -> evaluate $ force $ impl sampleData,
              bench (implName ++ ".copyIntoReg") $ (flip whnfAppIO) sampleData $ \sampleData -> do
                    resInRegion <- compact $ impl sampleData
                    evaluate $ getCompact $ resInRegion
            ]
          else
            [ bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> evaluate $ impl sampleData ])

launchImpl :: String -> IO ()
launchImpl s =
  let (_all, dotModuleName) = span (/= '.') s
      (moduleName, dotBenchmark) = span (/= '.') (tail dotModuleName)
      (_benchmark, dotImplSizeSpec) = span (/= '.') (tail dotBenchmark)
      implSizeSpec = tail dotImplSizeSpec
   in case (_all ++ "." ++ moduleName ++ "." ++ _benchmark) of
        "All.Bench.Compact.Map.benchmark" -> Utils.launchImpl' implSizeSpec Map.impls Map.dataSets
        "All.Bench.Compact.BFTraversal.benchmark" -> Utils.launchImpl' implSizeSpec BFTraversal.impls BFTraversal.dataSets
        "All.Bench.Compact.DList.benchmark" -> Utils.launchImpl' implSizeSpec DList.impls DList.dataSets
        "All.Bench.Compact.Queue.benchmark" -> Utils.launchImpl' implSizeSpec Queue.impls Queue.dataSets
        "All.Bench.Compact.SExpr.benchmark" -> Utils.launchImpl' implSizeSpec SExpr.impls SExpr.dataSets
        s' -> error ("benchmark group '" ++ s' ++ "' not found")

launchImpl' :: forall m a r. (NFData r) => String -> [(a %m -> r, String, Bool)] -> [(IO a, String)] -> IO ()
launchImpl' requestedImplDataSetspec impls datasets = go impls (go' datasets) where
  (requestedSize, dotRequestedImplSpec) = span (/= '.') requestedImplDataSetspec
  (requestedImplRadical, requestedImplVariant) = span (/= '.') (tail dotRequestedImplSpec)
  go [] _ = error ("requested implementation '" ++ requestedImplRadical ++ "' not found")
  go ((impl, implName, isLazy):_) loadSampleData | implName == requestedImplRadical = do
    sampleData <- loadSampleData
    if isLazy
      then case requestedImplVariant of
        ".force" -> evaluate $ rwhnf $ force $ impl sampleData
        ".copyIntoReg" -> do
          resInRegion <- compact $ impl sampleData
          evaluate $ rwhnf $ getCompact $ resInRegion
        _ -> error ("variant '" ++ requestedImplVariant ++ "' not found (required for lazy impl)")
    else
      evaluate $ rwhnf $ impl sampleData
    putStrLn "Done!"
  go (_:xs) loadSampleData = go xs loadSampleData

  go' [] = error ("requested size '" ++ requestedSize ++ "' not found")
  go' ((loadSampleData, sizeName):_) | sizeName == requestedSize = loadSampleData
  go' (_:xs) = go' xs
