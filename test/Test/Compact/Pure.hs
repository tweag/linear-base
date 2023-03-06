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
import Data.Unrestricted.Linear (dup4, dup5)
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
        case dup4 r of
          (r1, r2, r3, r4) ->
            complete $
              alloc r1
                <&> ( \dp ->
                        case dp <| C @"(,)" of
                          (dl, dr) ->
                            dl <|. intoR r2 1 `lseq`
                              dr
                                <|. alloc r3
                                <|. intoR r4 2
                    )
  let s2 :: Ur (Int, (Int, Int))
      !s2 = withRegion $ \r -> case dup5 r of
        (r1, r2, r3, r4, r5) ->
          complete $
            alloc r1
              <&> ( \dp ->
                      case dp <| C @"(,)" of
                        (dl, dr) ->
                          dl <|. intoR r2 1 `lseq`
                            dr
                              <|. (alloc r3 <&> (\dp' -> case dp' <| C @"(,)" of (dr1, dr2) -> dr1 <|. intoR r4 2 `lseq` dr2))
                              <|. intoR r5 3
                  )
  let s3 :: Ur (Foo Int Char)
      !s3 = withRegion $ \r -> case dup5 r of
        (r1, r2, r3, r4, r5) ->
          complete $
            alloc r1
              <&> ( \d ->
                      case d <| C @"MkFoo" of
                        (dBar, dBaz, dBoo) ->
                          dBar <|. intoR r2 1
                            `lseq` ( case dBaz <| C @"(,)" of
                                       (dl, dr) -> dl <|. intoR r3 'a' `lseq` dr <|. intoR r4 'b'
                                   )
                            `lseq` dBoo <|. intoR r5 2
                  )

  putStrLn "\nConstruction ok\n"
  putStrLn $ showHeap s1
  putStrLn $ "\n==================================================\n"
  putStrLn $ showHeap s2
  putStrLn $ "\n==================================================\n"
  putStrLn $ showHeap s3

  return "Done"
