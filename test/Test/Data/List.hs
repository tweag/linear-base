{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.List (listTests) where

import qualified Data.List.Linear as List
import qualified Data.Num.Linear as Num
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude.Linear
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)
import qualified Prelude

listTests :: TestTree
listTests =
  testGroup
    "List tests"
    [ testPropertyNamed "take n ++ drop n = id" "take_drop" take_drop,
      testPropertyNamed "length . take n = const n" "take_length" take_length,
      testPropertyNamed "zipWith is lazy" "zipWith_lazy" zipWith_lazy,
      testPropertyNamed "zipWith3 is lazy" "zipWith3_lazy" zipWith3_lazy
    ]

take_drop :: Property
take_drop = property $ do
  n <- forAll $ Gen.int (Range.linear 0 50)
  classify "0" $ n == 0
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int (Range.linear 0 40))
  classify "length > n" $ Prelude.length xs > n
  List.take n xs ++ List.drop n xs === xs

take_length :: Property
take_length = property $ do
  n <- forAll $ Gen.int (Range.linear 0 50)
  classify "0" $ n == 0
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int (Range.linear 0 40))
  classify "length > n" $ Prelude.length xs > n
  case Prelude.length xs > n of
    True -> do
      annotate "Prelude.length xs > n"
      Prelude.length (List.take n xs) === n
    False -> do
      annotate "Prelude.length xs < n"
      Prelude.length (List.take n xs) === Prelude.length xs

zipWith_lazy :: Property
zipWith_lazy = property $ do
  lgth <- forAll $ Gen.word (Range.linear 0 50)
  _ <- eval $ Prelude.head (xs lgth)
  Prelude.return ()
  where
    xs :: Word -> [Word]
    xs lgth = List.zipWith (Num.+) (0 : error "bottom") [0 .. lgth]

zipWith3_lazy :: Property
zipWith3_lazy = property $ do
  lgth1 <- forAll $ Gen.word (Range.linear 0 50)
  lgth2 <- forAll $ Gen.word (Range.linear 0 50)
  _ <- eval $ Prelude.head (xs lgth1 lgth2)
  Prelude.return ()
  where
    xs :: Word -> Word -> [Word]
    xs lgth1 lgth2 = List.zipWith3 (\x y z -> x Num.+ y Num.+ z) (0 : error "bottom") [0 .. lgth1] [0 .. lgth2]
