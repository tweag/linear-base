{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- This module defines linear mutable sets.
-- Please import this module qualified to avoid name clashes.
module Data.Set.Mutable.Linear
  ( -- * Mutable Sets
    Set,
    empty,
    insert,
    delete,
    size,
    member,
    fromList,
    Keyed,
  )
where

import qualified Data.HashMap.Linear as Linear
import Data.Unrestricted.Linear
import qualified Prelude.Linear as Linear
import GHC.Stack


-- # Data Definitions
-------------------------------------------------------------------------------

-- XXX This representation could be improved on with AVL trees, for example
newtype Set a = Set (Linear.HashMap a ())

type Keyed a = Linear.Keyed a


-- # Constructors and Mutators
-------------------------------------------------------------------------------

empty :: Keyed a => Int -> (Set a #-> Ur b) #-> Ur b
empty s (f :: Set a #-> Ur b) =
  Linear.empty s (\hm -> f (Set hm))

fromList :: (HasCallStack, Keyed a) => [a] -> (Set a #-> Ur b) #-> Ur b
fromList xs f = empty ((length xs) * 2 + 1) (f Linear.. insertAll xs)
  where
    insertAll :: Keyed a => [a] -> Set a #-> Set a
    insertAll [] set = set
    insertAll (x':xs') set = insertAll xs' (insert set x')

insert :: Keyed a => Set a #-> a -> Set a
insert (Set hmap) a = Set (Linear.insert hmap a ())

delete :: Keyed a => Set a #-> a -> Set a
delete (Set hmap) a = Set (Linear.delete hmap a)


-- # Accessors
-------------------------------------------------------------------------------

size :: Keyed a => Set a #-> (Set a, Ur Int)
size (Set hm) =
  Linear.size hm Linear.& \(hm', s) -> (Set hm', s)

member :: Keyed a => Set a #-> a -> (Set a, Ur Bool)
member (Set hm) a =
  Linear.member hm a Linear.& \(hm', b) -> (Set hm', b)


-- # Typeclass Instances
-------------------------------------------------------------------------------

instance Consumable (Set a) where
  consume (Set hmap) = consume hmap

