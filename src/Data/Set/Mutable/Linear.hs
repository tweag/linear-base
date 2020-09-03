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

empty :: Keyed a => Int -> (Set a #-> Unrestricted b) #-> Unrestricted b
empty s (f :: Set a #-> Unrestricted b) =
  Linear.empty (Linear.Size s) (\hm -> f (Set hm))

fromList :: (HasCallStack, Keyed a) => [a] -> (Set a #-> Unrestricted b) #-> Unrestricted b
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

size :: Keyed a => Set a #-> (Set a, Int)
size (Set hmap) = fromHashMapSize (Linear.size hmap)
  where
    fromHashMapSize :: (Linear.HashMap a (), Int) #-> (Set a, Int)
    fromHashMapSize (hmap, size) = (Set hmap, size)

member :: Keyed a => Set a #-> a -> (Set a, Bool)
member (Set hmap) a = fromHashMapMember (Linear.member hmap a)
  where
    fromHashMapMember :: (Linear.HashMap a (), Bool) #-> (Set a, Bool)
    fromHashMapMember (hmap, bool) = (Set hmap, bool)


-- # Typeclass Instances
-------------------------------------------------------------------------------

instance Consumable (Set a) where
  consume (Set hmap) = consume hmap

