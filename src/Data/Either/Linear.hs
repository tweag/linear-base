{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains useful functions for working with 'Either's.
module Data.Either.Linear
  ( either
  , lefts
  , rights
  , fromLeft
  , fromRight
  , partitionEithers
  )
  where

import Data.Unrestricted.Linear
import Prelude (Either(..))


-- XXX Design Notes
-- Functions like isLeft do not make sense in a linear program.
--------------------------------------------------------------------------------


-- | Linearly consume an @Either@ by applying the first linear function on a
-- value constructed with @Left@ and the second linear function on a value
-- constructed with @Right@.
either :: (a #-> c) -> (b #-> c) -> Either a b #-> c
either f _ (Left x) = f x
either _ g (Right y) = g y


-- | Get all the left elements in order, and consume the right ones.
lefts :: Consumable b => [Either a b] #-> [a]
lefts [] = []
lefts (Left a : xs) = a : lefts xs
lefts (Right b : xs) = lseq b (lefts xs)


-- | Get all the right elements in order, and consume the left ones.
rights :: Consumable a => [Either a b] #-> [b]
rights [] = []
rights (Left a : xs) = lseq a (rights xs)
rights (Right b : xs) = b : rights xs


-- | Get the left element of a consumable @Either@ with a default
fromLeft :: (Consumable a, Consumable b) => a #-> Either a b #-> a
fromLeft x (Left a) = lseq x a
fromLeft x (Right b) = lseq b x

-- | Get the right element of a consumable @Either@ with a default
fromRight :: (Consumable a, Consumable b) => b #-> Either a b #-> b
fromRight x (Left a) = lseq a x
fromRight x (Right b) = lseq x b

-- | Partition and consume a list of @Either@s into two lists with all the
-- lefts in one and the rights in the second, in the order they appeared in the
-- initial list.
partitionEithers :: [Either a b] #-> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers (x:xs) = fromRecur x (partitionEithers xs)
  where
    fromRecur :: Either a b #-> ([a], [b]) #-> ([a], [b])
    fromRecur (Left a) (as, bs) = (a:as, bs)
    fromRecur (Right b) (as, bs) = (as, b:bs)
