{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Set.Mutable.Linear.Internal where

import qualified Data.HashMap.Mutable.Linear as Linear
import Data.Monoid.Linear
import Data.Unrestricted.Linear
import qualified Prelude.Linear as Linear hiding (insert)
import Prelude (Bool, Int)
import qualified Prelude

-- # Data Definitions
-------------------------------------------------------------------------------

-- XXX This representation could be improved on with AVL trees, for example
newtype Set a = Set (Linear.HashMap a ())

type Keyed a = Linear.Keyed a

-- # Constructors and Mutators
-------------------------------------------------------------------------------

empty :: Keyed a => Int -> (Set a %1 -> Ur b) %1 -> Ur b
empty s (f :: Set a %1 -> Ur b) =
  Linear.empty s (\hm -> f (Set hm))

toList :: Keyed a => Set a %1 -> Ur [a]
toList (Set hm) =
  Linear.toList hm
    Linear.& \(Ur xs) -> Ur (Prelude.map Prelude.fst xs)

insert :: Keyed a => a -> Set a %1 -> Set a
insert a (Set hmap) = Set (Linear.insert a () hmap)

delete :: Keyed a => a -> Set a %1 -> Set a
delete a (Set hmap) = Set (Linear.delete a hmap)

union :: Keyed a => Set a %1 -> Set a %1 -> Set a
union (Set hm1) (Set hm2) =
  Set (Linear.unionWith (\_ _ -> ()) hm1 hm2)

intersection :: Keyed a => Set a %1 -> Set a %1 -> Set a
intersection (Set hm1) (Set hm2) =
  Set (Linear.intersectionWith (\_ _ -> ()) hm1 hm2)

-- # Accessors
-------------------------------------------------------------------------------

size :: Keyed a => Set a %1 -> (Ur Int, Set a)
size (Set hm) =
  Linear.size hm Linear.& \(s, hm') -> (s, Set hm')

member :: Keyed a => a -> Set a %1 -> (Ur Bool, Set a)
member a (Set hm) =
  Linear.member a hm Linear.& \(b, hm') -> (b, Set hm')

fromList :: Keyed a => [a] -> (Set a %1 -> Ur b) %1 -> Ur b
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
  dup2 (Set hm) =
    dup2 hm Linear.& \(hm1, hm2) ->
      (Set hm1, Set hm2)
