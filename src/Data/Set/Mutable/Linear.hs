{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- This module defines linear mutable sets.
-- Please import this module qualified to avoid name clashes.

module Data.Set.Mutable.Linear
  ( -- * Mutable Sets
    Set,
    singleton,
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
import qualified Data.Coerce as Coerce
import qualified Prelude.Linear as Linear
import Unsafe.Linear as Unsafe
import GHC.Stack


-- # Data Definitions
-------------------------------------------------------------------------------

-- XXX This representation could be improved on with AVL trees, for example
newtype Set a = Set (Linear.HashMap a ())

type Keyed a = Linear.Keyed a

-- # Constructors and Mutators
-------------------------------------------------------------------------------

singleton :: Keyed a => a -> (Set a #-> Unrestricted b) -> Unrestricted b
singleton a (f :: Set a #-> Unrestricted b) = Linear.singleton (a,()) f'
  where
    f' :: Linear.HashMap a () #-> Unrestricted b
    f' hmap = f (Set hmap)

fromList :: HasCallStack =>
  Keyed a => [a] -> (Set a #-> Unrestricted b) -> Unrestricted b
fromList [] _ = error "Creating a set from an empty list"
fromList (x:xs) f = singleton x (f Linear.. insertAll xs)
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
    fromHashMapSize = Unsafe.toLinear Coerce.coerce

member :: Keyed a => Set a #-> a -> (Set a, Bool)
member (Set hmap) a = fromHashMapMember (Linear.member hmap a)
  where
    fromHashMapMember :: (Linear.HashMap a (), Bool) #-> (Set a, Bool)
    fromHashMapMember = Unsafe.toLinear Coerce.coerce

-- # Typeclass Instances
-------------------------------------------------------------------------------

instance Consumable (Set a) where
  consume (Set hmap) = consume hmap
