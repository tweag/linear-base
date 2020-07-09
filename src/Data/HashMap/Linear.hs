{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- This module provides mutable hashmaps with a linear interface.
--
-- To use mutable hashmaps, create a linear computation of type
-- @HashMap k v #-> Unrestricted b@ and feed it to 'singleton' with
-- an initial key-value pair @(k,v)@.
--
-- This hashmap is implemented with robin hood hashing which has good average
-- case performance.
module Data.HashMap.Linear
  ( -- * A mutable hashmap
    HashMap,
    -- * Run a computation using a 'HashMap'
    singleton,
    singleton',
    -- * Modifiers and Constructors
    alter,
    insert,
    delete,
    insertAll,
    -- * Accessors
    size,
    member,
    lookup,
    Keyed,
  )
where

import Data.Array.Mutable.Linear
import Data.Hashable
import Data.Unrestricted.Linear
import Prelude.Linear hiding ((+), lookup, read)
import qualified Unsafe.Linear as Unsafe
import Prelude ((+))
import qualified Prelude

-- # Implementation Notes
-- This is a simple implementatation of robin hood hashing.
--
-- See these links:
--
-- * https://programming.guide/robin-hood-hashing.html
-- * https://andre.arko.net/2017/08/24/robin-hood-hashing/
-- * https://cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf
--

-- # Core Data Types
--------------------------------------------------

-- | Robin values are triples of the key, value and PSL
-- (the probe sequence length).
type RobinVal k v = (k, v, PSL)

-- | A probe sequence length
newtype PSL = PSL Int
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Num, Prelude.Show)

-- | An array of Robin values
type RobinArr k v = Array (RobinVal k v)

-- | At minimum, we need to store hashable
-- and identifiable keys
type Keyed k = (Eq k, Hashable k)

-- | A mutable hashmap with a linear API
data HashMap k v where
  -- | HashMap (size,count) array-of-robin-values
  -- The size is the amount of memory the array takes up.
  -- The count is the number of stored mappings.
  -- Our sparseness invariant: count*3 <= size.
  HashMap :: (Int, Int) -> RobinArr k v #-> HashMap k v

-- INVARIANTS:
--   * Cells are empty iff the PSL is -1.
--   * We are sparse: count*3 <= size, with correct values of count and size
--   * Each (RobinVal k v) has the correct PSL

-- | The results of searching for where to insert a key
data RobinQuery k where
  -- | A key, PSL pair to insert at an index, that has not
  -- yet been touched. Invariant: the PSL must be -1 at that index.
  IndexToInsert :: (k, PSL) -> Int -> RobinQuery k
  -- A key, PSL pair to update the value at an index.
  IndexToUpdate :: (k, PSL) -> Int -> RobinQuery k
  -- | A key, PSL pair for a swap at an index.
  -- The swapped-out pair will then need to be inserted downstream.
  IndexToSwap :: (k, PSL) -> Int -> RobinQuery k

-- # Construction and Modification
--------------------------------------------------

-- | Run a computation using a singleton hashmap
singleton :: Keyed k =>
  (k, v) -> (HashMap k v #-> Unrestricted b) -> Unrestricted b
singleton (k :: k, v :: v) (f :: HashMap k v #-> Unrestricted b) =
  alloc defaultSize (k, v, -1) applyHM
  where
    applyHM :: Array (RobinVal k v) #-> Unrestricted b
    applyHM arr = let ixToHash = (hash k) `mod` defaultSize in
      f $ HashMap (defaultSize, 1) (write arr ixToHash (k, v, 0))

singleton' :: Keyed k =>
  (k, v) -> (HashMap k v #-> Unrestricted b) -> b
singleton' kv f = unUnrestricted (singleton kv f)

-- XXX: re-write linearly
-- | Given a key @k@ to 'lookup', look up a @Maybe v@ and feed it to
-- a function @Maybe v -> Maybe v@, and if the result is @Nothing@,
-- delete that key, and otherwise, replace the value with the @v@
-- in the returned @Just v@.
alter ::
  Keyed k =>
  HashMap k v #-> (Maybe v -> Maybe v) ->
  k ->
  HashMap k v
alter = Unsafe.toLinear unsafeAlter
  where
    unsafeAlter :: Keyed k =>
      HashMap k v -> (Maybe v -> Maybe v) -> k -> HashMap k v
    unsafeAlter hmap f k =
      case lookup hmap k of
        (hmap', Unrestricted maybeV) -> case f maybeV of
          Nothing -> delete hmap' k
          Just v -> insert hmap' k v

-- | If the key is present, this update the value,
-- otherwise insert a new key-value pair.
insert :: Keyed k => HashMap k v #-> k -> v -> HashMap k v
insert hmap k v = insertFromQuery v $ queryIndex (maybeResize hmap) k
  where
    insertFromQuery :: Keyed k =>
      v -> (HashMap k v, RobinQuery k) #-> HashMap k v
    insertFromQuery v (HashMap (!size, !count) arr, IndexToInsert (k, psl) ix) =
      HashMap (size, count + 1) (write arr ix (k, v, psl))
    insertFromQuery v (HashMap sizes arr, IndexToUpdate (k, psl) ix) =
      HashMap sizes (write arr ix (k, v, psl))
    insertFromQuery v (HashMap sizes arr, IndexToSwap (k, psl) ix) =
      (Unsafe.toLinear swapFromRead) (read arr ix) sizes ix (k, v, psl)
    -- XXX: This re-does the probe sequence of (k',v')
    swapFromRead ::
      Keyed k =>
      (RobinArr k v, RobinVal k v) ->
      (Int, Int) ->
      Int ->
      RobinVal k v ->
      HashMap k v
    swapFromRead (arr', (k', v', _)) sizes ix (k, v, psl) =
      insert (HashMap sizes (write arr' ix (k, v, psl))) k' v'

-- | This deletes the key-value pair if it is present, and otherwise does
-- nothing.
delete :: Keyed k => HashMap k v #-> k -> HashMap k v
delete hmap k = deleteFromQuery $ queryIndex hmap k
  where
    deleteFromQuery :: Keyed k => (HashMap k v, RobinQuery k) #-> HashMap k v
    deleteFromQuery (h, IndexToInsert _ _) = h
    deleteFromQuery (h, IndexToSwap _ _) = h
    deleteFromQuery (h, IndexToUpdate _ ix) =
      shiftBack h ix
    -- Continue to shift back the next element and decrease the PSL
    -- until we see an element with PSL 0 (or an empty cell, with PSL -1).
    -- The arguments are: `deleteShiftFrom (hashmap, size) index`
    shiftBack :: Keyed k => HashMap k v #-> Int -> HashMap k v
    shiftBack (HashMap (!size, !count) arr) ix =
      HashMap (size, count -1) ((Unsafe.toLinear shiftBackArr) arr size ix)
    -- XXX: make this linear
    shiftBackArr :: Keyed k => RobinArr k v -> Int -> Int -> RobinArr k v
    shiftBackArr arr size ix =
      case read arr ((ix + 1) `mod` size) of
        (arr', (_, _, PSL p)) | p <= 0 ->
          case read arr' ix of
            (arr'', (k', v', _)) -> write arr'' ix (k', v', -1)
        (arr', (k, v, PSL p)) ->
          let arr'' = (write arr' ix (k, v, PSL (p -1))) in
            shiftBackArr arr''  size ((ix + 1) `mod` size)

-- | If the load is too high resize by tripling the array.
-- XXX: This is ugly, but algorithmically simple and correct.
maybeResize :: Keyed k => HashMap k v #-> HashMap k v
maybeResize (HashMap (!size, !count) arr)
  | count * 3 < size = HashMap (size, count) arr
  | otherwise =
    (Unsafe.toLinear withAssocList) $
      (Unsafe.toLinear assocList) (HashMap (size, count) arr)
  where
    withAssocList :: Keyed k => (HashMap k v, [(k, v)]) -> HashMap k v
    withAssocList (HashMap _ arr, kvs@((k, v) : _)) =
      let resizedArr = resizeSeed (size * 3) (k, v, -1) arr in
        insertAll kvs (HashMap (size * 3, 0) resizedArr)
    assocList :: Keyed k => HashMap k v -> (HashMap k v, [(k, v)])
    assocList h@(HashMap _ arr) = (h, filterValidPSL (snd (toList arr)))
      where
        filterValidPSL [] = []
        filterValidPSL ((k, v, psl) : xs)
          | psl < 0 = filterValidPSL xs
          | otherwise = (k, v) : filterValidPSL xs

-- | 'insert' (in the provided order) a list of key-value pairs to a given
-- hashmap.
insertAll :: Keyed k => [(k, v)] -> HashMap k v #-> HashMap k v
insertAll [] hmap = hmap
insertAll ((k, v) : xs) hmap = insertAll xs (insert hmap k v)

-- # Querying
--------------------------------------------------

size :: HashMap k v #-> (HashMap k v, Int)
size (HashMap (!size, !count) arr) = (HashMap (size, count) arr, count)

member :: Keyed k => HashMap k v #-> k -> (HashMap k v, Bool)
member hmap k = memberFromQuery (queryIndex hmap k)
  where
    memberFromQuery ::
      Keyed k =>
      (HashMap k v, RobinQuery k) #-> (HashMap k v, Bool)
    memberFromQuery (h, IndexToInsert _ _) = (h, False)
    memberFromQuery (h, IndexToSwap _ _) = (h, False)
    memberFromQuery (h, IndexToUpdate _ _) = (h, True)

lookup :: Keyed k => HashMap k v #-> k -> (HashMap k v, Unrestricted (Maybe v))
lookup hmap k = (Unsafe.toLinear lookupFromIx) $ queryIndex hmap k
  where
    lookupFromIx :: (HashMap k v, RobinQuery k) -> (HashMap k v, Unrestricted (Maybe v))
    lookupFromIx (h, IndexToInsert _ _) = (h, Unrestricted Nothing)
    lookupFromIx (h@(HashMap _ arr), IndexToUpdate _ ix) =
      case read arr ix of (_, (_, v, _)) -> (h, Unrestricted (Just v))
    lookupFromIx (h, IndexToSwap _ _) = (h, Unrestricted Nothing)

-- | Internal function:
-- Find the index a key ought to hash into, and the PSL it should have.
-- NOTE: if the underlying array is too small, this never terminates.
queryIndex :: Keyed k => HashMap k v #-> k -> (HashMap k v, RobinQuery k)
queryIndex = Unsafe.toLinear unsafeQueryIx
  where
    unsafeQueryIx :: Keyed k => HashMap k v -> k -> (HashMap k v, RobinQuery k)
    unsafeQueryIx h@(HashMap (size, _) arr) key =
      (h, walkDownArr (arr, (hash key) `mod` size) (key, 0) size)
    walkDownArr :: Keyed k =>
      (RobinArr k v, Int) -> (k, PSL) -> Int -> RobinQuery k
    walkDownArr (arr, ix) (key, psl) size = case read arr ix of
      (_, (_, _, psl_ix)) | psl_ix == (-1) -> IndexToInsert (key, psl) ix
      (_, (k_ix, _, _)) | key == k_ix -> IndexToUpdate (key, psl) ix
      (_, (_, _, psl_ix)) | psl_ix < psl -> IndexToSwap (key, psl) ix
      _ -> walkDownArr (arr, (ix + 1) `mod` size) (key, psl + PSL 1) size

-- # Instances
--------------------------------------------------

instance Consumable (HashMap k v) where
  consume :: HashMap k v #-> ()
  consume (HashMap _ arr) = consume arr

-- # This is provided for debugging only.
instance (Show k, Show v) => Show (HashMap k v) where
  show (HashMap _ robinArr) = show robinArr

-- # Internal Library
--------------------------------------------------

-- | Default intial size of underlying array.
-- We initially assume space for 8 pairs, and allocate 8*3 slots.
-- We aim to make the number of pairs some power of two.
defaultSize :: Int
defaultSize = 24
