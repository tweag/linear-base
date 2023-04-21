{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Compact.Pure (compactPureTests) where

import Compact.Pure
import Compact.Pure.Internal
import Control.Functor.Linear ((<&>))
import Control.Monad (return)
import GHC.Generics (Generic)
import Prelude.Linear
import Test.Tasty
import Test.Tasty.HUnit

compactPureTests :: TestTree
compactPureTests =
  testGroup
    "Demo for compact regions and destinations"
    [testCaseInfo "Compact region demo" compactDemo]

-- Launch with

-- $ stack test --ta '-p "Compact region demo"'

data Foo a b = MkFoo {unBar :: a, unBaz :: (b, b), unBoo :: a} deriving (Generic)

compactDemo :: IO String
compactDemo = do
  let s1 :: Ur (Int, Int)
      !s1 = withRegion $ \r ->
        case dup r of
          (r1, r2) ->
            complete $
              alloc r1
                <&> ( \dp ->
                        case dp <| C @"(,)" of
                          (dl, dr) ->
                            dl <|.. 1 `lseq`
                              dr
                                <|. alloc r2
                                <|.. 2
                    )
  let s2 :: Ur (Int, (Int, Int))
      !s2 = withRegion $ \r -> case dup r of
        (r1, r2) ->
          complete $
            alloc r1
              <&> ( \dp ->
                      case dp <| C @"(,)" of
                        (dl, dr) ->
                          dl <|.. 1 `lseq`
                            dr
                              <|. (alloc r2 <&> (\dp' -> case dp' <| C @"(,)" of (dr1, dr2) -> dr1 <|.. 2 `lseq` dr2))
                              <|.. 3
                  )
  let s3 :: Ur (Foo Int Char, Int)
      !s3 = withRegion $ \r -> 
          completeExtract $
            alloc r
              <&> ( \d ->
                      case d <| C @"MkFoo" of
                        (dBar, dBaz, dBoo) ->
                          dBar <|.. 1
                            `lseq` ( case dBaz <| C @"(,)" of
                                       (dl, dr) -> dl <|.. 'a' `lseq` dr <|.. 'b'
                                   )
                            `lseq` dBoo <|.. 2
                            `lseq` Ur 14
                  )

  putStrLn "\nConstruction ok\n"
  putStrLn $ showHeap s1
  putStrLn $ "\n==================================================\n"
  putStrLn $ showHeap s2
  putStrLn $ "\n==================================================\n"
  putStrLn $ showHeap s3

  return "Done"
