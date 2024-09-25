{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Compact.Utils where

import Data.Functor ((<&>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCaseInfo)

safetySameAsFirstImpl :: forall m a r. (Show r, Eq r) => String -> [(a %m -> r, String, Bool)] -> a -> TestTree
safetySameAsFirstImpl name impls sampleData = do
  let ((refImpl, refImplName, _) : otherImpls) = impls
  testGroup
    ("All " ++ name ++ " implementations give the same result")
    ( otherImpls <&> \(impl, implName, _) ->
        testCaseInfo (refImplName ++ " and " ++ implName ++ " give the same result") $ do
          let expected = refImpl sampleData
              actual = impl sampleData
          assertEqual "same result" expected actual
          return ""
    )

safetySameAsExpected :: forall m a r. (Show r, Eq r) => String -> [(a %m -> r, String, Bool)] -> a -> r -> TestTree
safetySameAsExpected name impls sampleData expected = do
  testGroup
    ("All " ++ name ++ " implementations give the same result")
    ( impls <&> \(impl, implName, _) ->
        testCaseInfo (implName ++ " gives the expected result") $ do
          let actual = impl sampleData
          assertEqual "same result" expected actual
          return ""
    )
