{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- This module defines linear mutable sets.
--
-- The underlying implementation uses 'Data.HashMap.Linear', so it inherits
-- the time and memory characteristics of it.
--
-- Please import this module qualified to avoid name clashes.
module Data.Set.Mutable.Linear
  ( -- * Mutable Sets
    Set,
    empty,
    insert,
    delete,
    union,
    intersection,
    size,
    member,
    fromList,
    toList,
    Keyed,
  )
where

import qualified Data.HashMap.Linear as Linear
import qualified Prelude.Linear as Linear
import Prelude (Int, Bool)
import qualified Prelude
import Data.Monoid.Linear
import Data.Unrestricted.Linear


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

toList :: Keyed a => Set a #-> Ur [a]
toList (Set hm) =
  Linear.toList hm
    Linear.& \(Ur xs) -> Ur (Prelude.map Prelude.fst xs)

insert :: Keyed a => Set a #-> a -> Set a
insert (Set hmap) a = Set (Linear.insert hmap a ())

delete :: Keyed a => Set a #-> a -> Set a
delete (Set hmap) a = Set (Linear.delete hmap a)

union :: Keyed a => Set a #-> Set a #-> Set a
union (Set hm1) (Set hm2) =
  Set (Linear.unionWith (\_ _ -> ()) hm1 hm2)

intersection :: Keyed a => Set a #-> Set a #-> Set a
intersection (Set hm1) (Set hm2) =
  Set (Linear.intersectionWith (\_ _ -> ()) hm1 hm2)

-- # Accessors
-------------------------------------------------------------------------------

size :: Keyed a => Set a #-> (Ur Int, Set a)
size (Set hm) =
  Linear.size hm Linear.& \(s, hm') -> (s, Set hm')

member :: Keyed a => Set a #-> a -> (Ur Bool, Set a)
member (Set hm) a =
  Linear.member hm a Linear.& \(b, hm') -> (b, Set hm')

fromList :: Keyed a => [a] -> (Set a #-> Ur b) #-> Ur b
fromList xs f =
  Linear.fromList (Prelude.map (,()) xs) (\hm -> f (Set hm))

-- # Typeclass Instances
-------------------------------------------------------------------------------

instance Prelude.Semigroup (Set a) where
  (<>) = Prelude.error "Prelude.(<>): invariant violation, unrestricted Set"

instance Keyed a => Semigroup (Set a) where
  (<>) = union

instance Consumable (Set a) where
  consume (Set hmap) = consume hmap

instance Dupable (Set a) where
  dup2 (Set hm) = dup2 hm Linear.& \(hm1, hm2) ->
    (Set hm1, Set hm2)
