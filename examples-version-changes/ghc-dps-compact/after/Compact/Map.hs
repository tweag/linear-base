{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-type-defaults #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Compact.Map where

import Compact.Destination
import Control.Functor.Linear ((<&>))
import Data.Kind (Type)
import Prelude.Linear

mapL :: forall a b. (a %1 -> b) -> [a] -> [b]
mapL _ [] = []
mapL f (x : xs) = (f x) : (mapL f xs)

mapS :: forall a b. (a %1 -> b) -> [a] -> [b]
mapS _ [] = []
mapS f (x : xs) =
  let !r = f x
      !tail = mapS f xs
   in r : tail

mapSH :: forall a b. (a %1 -> b) -> [a] -> [b]
mapSH _ [] = []
mapSH f (x : xs) =
  let !r = f x
   in r : (mapSH f xs)

mapST :: forall a b. (a %1 -> b) -> [a] -> [b]
mapST _ [] = []
mapST f (x : xs) =
  let !tail = mapST f xs
   in (f x) : tail

mapTRL :: forall a b. (a %1 -> b) -> [a] -> [b]
mapTRL f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) = go ((f x) : acc) xs

mapTRS :: forall a b. (a %1 -> b) -> [a] -> [b]
mapTRS f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) =
      let !r = f x
          !cons = r : acc
       in go cons xs

mapTRSH :: forall a b. (a %1 -> b) -> [a] -> [b]
mapTRSH f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) =
      let !r = f x
       in go (r : acc) xs

mapTRST :: forall a b. (a %1 -> b) -> [a] -> [b]
mapTRST f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) =
      let !cons = (f x) : acc
       in go cons xs

mapDestTRL :: forall (r :: Type) a b. (Region r) => (a %1 -> b) -> [a] -> Dest r [b] %1 -> ()
mapDestTRL _ [] dl = dl & fill @'[]
mapDestTRL f (x : xs) dl = case dl & fill @'(:) of
  (dh, dt) -> dh & fillLeaf (f x) `lseq` mapDestTRL f xs dt

mapDestTRS :: forall (r :: Type) a b. (Region r) => (a %1 -> b) -> [a] -> Dest r [b] %1 -> ()
mapDestTRS _ [] dl = dl & fill @'[]
mapDestTRS f (x : xs) dl = case dl & fill @'(:) of
  (dh, dt) -> let !r = f x in dh & fillLeaf r `lseq` mapDestTRS f xs dt

mapDestFL :: forall (r :: Type) a b. (Region r) => (a %1 -> b) -> [a] -> Dest r [b] %1 -> ()
mapDestFL f l dl =
  (foldl_ fillConsF dl l) & fill @'[]
  where
    fillConsF :: Dest r [b] %1 -> a -> Dest r [b]
    fillConsF dl x = case dl & fill @'(:) of
      (dh, dt) -> dh & fillLeaf (f x) `lseq` dt
    foldl_ :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
    foldl_ _ s [] = s
    foldl_ f s (x : xs) = foldl_ f (f s x) xs

mapDestFSL :: forall (r :: Type) a b. (Region r) => (a %1 -> b) -> [a] -> Dest r [b] %1 -> ()
mapDestFSL f l dl =
  (foldl_ fillConsF dl l) & fill @'[]
  where
    fillConsF dl x = case dl & fill @'(:) of
      (dh, dt) -> dh & fillLeaf (f x) `lseq` dt
    foldl_ :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
    foldl_ _ s [] = s
    foldl_ f s (x : xs) = let !r = (f s x) in foldl_ f r xs

mapDestFLS :: forall (r :: Type) a b. (Region r) => (a %1 -> b) -> [a] -> Dest r [b] %1 -> ()
mapDestFLS f l dl =
  (foldl_ fillConsF dl l) & fill @'[]
  where
    fillConsF dl x = case dl & fill @'(:) of
      (dh, dt) -> let !r = f x in dh & fillLeaf r `lseq` dt
    foldl_ :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
    foldl_ _ s [] = s
    foldl_ f s (x : xs) = foldl_ f (f s x) xs

mapDestFS :: forall (r :: Type) a b. (Region r) => (a %1 -> b) -> [a] -> Dest r [b] %1 -> ()
mapDestFS f l dl =
  (foldl_ fillConsF dl l) & fill @'[]
  where
    fillConsF dl x = case dl & fill @'(:) of
      (dh, dt) -> let !r = f x in dh & fillLeaf r `lseq` dt
    foldl_ :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
    foldl_ _ s [] = s
    foldl_ f s (x : xs) = let !r = (f s x) in foldl_ f r xs

-------------------------------------------------------------------------------

dpsWrapper :: (forall (r :: Type) a b. (Region r) => (a %1 -> b) -> [a] -> Dest r [b] %1 -> ()) -> (Int %1 -> Int) -> [Int] -> [Int]
dpsWrapper impl f l = unur (withRegion (\ @r t -> fromIncomplete_ (alloc @r t <&> \d -> impl f l d)))

impls' :: (Int %1 -> Int) -> [([Int] -> [Int], String, Bool)]
impls' f =
  [ (mapL f, "mapL", True),
    (mapS f, "mapS", True),
    (mapSH f, "mapSH", True),
    (mapST f, "mapST", True),
    (mapTRL f, "mapTRL", True),
    (mapTRS f, "mapTRS", True),
    (mapTRSH f, "mapTRSH", True),
    (mapTRST f, "mapTRST", True),
    (dpsWrapper mapDestTRL f, "mapDestTRL", False),
    (dpsWrapper mapDestTRS f, "mapDestTRS", False),
    (dpsWrapper mapDestFL f, "mapDestFL", False),
    (dpsWrapper mapDestFLS f, "mapDestFLS", False),
    (dpsWrapper mapDestFSL f, "mapDestFSL", False),
    (dpsWrapper mapDestFS f, "mapDestFS", False)
  ]

impls :: [([Int] -> [Int], String, Bool)]
impls = impls' (\x -> 2 * x + 1)
