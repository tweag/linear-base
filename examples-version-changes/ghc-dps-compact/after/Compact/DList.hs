{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-type-defaults #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Compact.DList where

import Compact.Destination
import Control.Functor.Linear ((<&>))
import Prelude.Linear hiding (foldl', foldr, concat)
import Data.Proxy (Proxy)
import qualified Prelude as NonLin

newtype DList r a = DList (Incomplete r [a] (Dest r [a]))

new :: forall r a. (Region r) => Token %1 -> DList r a
new token = DList (alloc @r token)

append :: forall r a. (Region r) => DList r a %1 -> a -> DList r a
append (DList i) x =
  DList $ i <&> \dl -> case dl & fill @'(:) of
    (dh, dt) -> dh & fillLeaf x `lseq` dt

concat :: forall r a. (Region r) => DList r a %1 -> DList r a %1 -> DList r a
concat (DList i1) (DList i2) = DList $ i1 <&> \dl -> dl & fillComp i2

toList :: forall r a. (Region r) => DList r a %1 -> [a]
toList (DList i) = unur $ fromIncomplete_ $ i <&> \dl -> dl & fill @'[]

toUList :: forall r a. (Region r) => DList r a %1 -> Ur [a]
toUList (DList i) = fromIncomplete_ $ i <&> \dl -> dl & fill @'[]

fromList :: forall r a. (Region r) => Token %1 -> [a] -> DList r a
fromList token l = DList (alloc @r token <&> \d -> go d l) where
  go dl [] = dl
  go dl (x:xs) = case dl & fill @'(:) of (dh, dt) -> dh & fillLeaf x `lseq` go dt xs

newtype DListN a = DListN ([a] %1 -> [a])

newN :: forall a. DListN a
newN = DListN (\ys -> ys)

appendN :: forall a. DListN a %1 -> a %1 -> DListN a
appendN (DListN f) x =
  DListN $ \ys -> f ([x] ++ ys)

concatN :: forall a. DListN a %1 -> DListN a %1 -> DListN a
concatN (DListN f1) (DListN f2) = DListN $ f1 . f2

toListN :: forall a. DListN a %1 -> [a]
toListN (DListN f) = f []

fromListN :: forall a. [a] %1 -> DListN a
fromListN xs = DListN $ \ys -> xs ++ ys

-------------------------------------------------------------------------------

impls :: [([[Int]] -> [Int], String, Bool)]
impls =
  [ (concatRight, "concatRight", True)
  , (differenceListNaiveLeft, "differenceListNaiveLeft", True)
  , (differenceListDestLeft, "differenceListDestLeft", False)
  ]

foldl' :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
foldl' _ s [] = s
foldl' f s (x : xs) = let !r = (f s x) in foldl' f r xs

foldr :: forall a b. (b -> a %1 -> a) -> a %1 -> [b] -> a
foldr _ s [] = s
foldr f s (x : xs) = x `f` foldr f s xs

concatLeft :: [[a]] -> [a]
concatLeft = foldl' (\xs ys -> xs ++ ys) []

concatRight :: [[a]] -> [a]
concatRight = foldr (\xs ys -> xs ++ ys) []

differenceListNaiveLeft :: [[a]] -> [a]
differenceListNaiveLeft lists = toListN (foldl' (\dl ys -> let !r = dl `concatN` (fromListN ys) in r) newN lists)

differenceListDestLeft :: [[a]] -> [a]
differenceListDestLeft lists = unur (withRegion (\(_ :: Proxy r) t ->
  let f :: (Token, DList r a) %1 -> [a] -> (Token, DList r a)
      f (t, dl) ys =
        let !(t', t'') = dup2 t
            !r = dl `concat` (fromList @r t' ys)
         in (t'', r)
      !(t', t'') = dup2 t
      !(t''', dl) = foldl' f (t'', new @r t') lists
   in consume t''' `lseq` toUList dl ))
