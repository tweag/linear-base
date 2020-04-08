{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
--   A mutable hashmap with a linear interface.
--   Internally, this uses robin hood hashing on mutable,
--   linear arrays.
--
--   TODO:
--
--   * Make everything linear, I don't need to use Unsafe.toLinear
--   * Add singletonWithSize
--   * swapFromRead repeats the probe sequence on the recurive insert call
--     and this could be done away with
--   * We don't resize to make the array smaller when we have too much space
--   * Make the lookup use smart search by probing from the mean PSL:
--     probe at the mean, mean-1, mean+1, mean-2, mean+2, ...
--   * Other functions from Data.Map

module Data.HashMap.Linear
  ( singleton,
    alter,
    insert,
    delete,
    size,
    member,
    lookup
  )
where

import Data.Hashable
import Data.Unrestricted.Linear
import Data.Array.Mutable.Linear
import Prelude.Linear hiding (read, lookup, (+))
import Prelude ((+))
import qualified Prelude
import qualified Unsafe.Linear as Unsafe

-- # Core Data Types
--------------------------------------------------

-- | Robin values are triples of the key, value and PSL
-- (the probe sequence length).
type RobinVal k v = (k,v,PSL)

-- | A probe sequence length
newtype PSL = PSL Int
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Num)

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
  (k,v) -> (HashMap k v #-> Unrestricted b) -> Unrestricted b
singleton (k :: k, v :: v) (f :: HashMap k v #-> Unrestricted b) =
  alloc defaultSize (k,v,-1) applyHM
  where
    applyHM :: Array (RobinVal k v) #-> Unrestricted b
    applyHM arr = f $
      HashMap (defaultSize, 1) (write arr (hash k `mod` defaultSize) (k,v,0))

-- XXX: re-write linearly
alter ::  Keyed k =>
  HashMap k v #-> (Maybe v -> Maybe v) -> k -> HashMap k v
alter = Unsafe.toLinear unsafeAlter
  where
    unsafeAlter :: Keyed k =>
      HashMap k v -> (Maybe v -> Maybe v) -> k -> HashMap k v
    unsafeAlter hmap f k =
      case lookup hmap k of
        (hmap', maybeV) -> case f maybeV of
          Nothing -> delete hmap' k
          Just v -> insert hmap' k v

-- | If present, update value, otherwise insert new mapping
insert :: Keyed k => HashMap k v #-> k -> v -> HashMap k v
insert hmap k v = insertFromQuery v $ queryIndex (maybeResize hmap) k
  where
    insertFromQuery :: Keyed k =>
      v -> (HashMap k v, RobinQuery k) #-> HashMap k v
    insertFromQuery v (HashMap (size,count) arr, IndexToInsert (k,psl) ix) =
      HashMap (size,count+1) (write arr ix (k,v,psl))
    insertFromQuery v (HashMap sizes arr, IndexToUpdate (k,psl) ix) =
      HashMap sizes (write arr ix (k,v,psl))
    insertFromQuery v (HashMap sizes arr, IndexToSwap (k,psl) ix) =
      (Unsafe.toLinear swapFromRead) (read arr ix) sizes ix (k,v,psl)

    -- XXX: This re-does the probe sequence of (k',v')
    swapFromRead :: Keyed k => (RobinArr k v, RobinVal k v) ->
      (Int, Int) -> Int -> RobinVal k v -> HashMap k v
    swapFromRead (arr', (k',v',_)) sizes ix (k,v,psl) =
      insert (HashMap sizes (write arr' ix (k,v,psl))) k' v'

-- | If present, deletes key-value pair, otherwise does nothing
delete :: Keyed k => HashMap k v #-> k -> HashMap k v
delete hmap k = deleteFromQuery $ queryIndex hmap k
  where
    deleteFromQuery :: Keyed k =>
      (HashMap k v, RobinQuery k) #-> HashMap k v
    deleteFromQuery (h, IndexToInsert _ _) = h
    deleteFromQuery (h, IndexToSwap _ _) = h
    deleteFromQuery (h, IndexToUpdate _ ix) =
      shiftBack h ix

    -- Continue to shift back the next element and decrease the PSL
    -- until we see an element with PSL 0. The arguments are:
    -- `deleteShiftFrom (hashmap, size) index`
    shiftBack :: Keyed k => HashMap k v #-> Int -> HashMap k v
    shiftBack (HashMap (size,count) arr) ix =
      HashMap (size,count-1) ((Unsafe.toLinear shiftBackArr) arr size ix)

    -- XXX: make this linear
    shiftBackArr :: Keyed k =>
      RobinArr k v -> Int -> Int -> RobinArr k v
    shiftBackArr arr size ix =
      case read arr (ix + 1 `mod` size) of
        (arr', (_,_,PSL p)) | p == 0 ->
          case read arr' ix of
            (arr'', (k',v',_)) -> write arr'' ix (k',v',-1)
        (arr', (k,v,PSL p)) ->
          shiftBackArr (write arr' ix (k,v,PSL (p-1))) size (ix+1 `mod` size)

-- | If the load is too high resize by tripling the array.
maybeResize :: Keyed k => HashMap k v #-> HashMap k v
maybeResize (HashMap (size, count) arr)
   | count*3 < size = HashMap (size, count) arr
   | otherwise = HashMap (size*3, count) (Unsafe.toLinear tripleArr arr)
  where
    tripleArr :: Keyed k => RobinArr k v -> RobinArr k v
    tripleArr arr = case read arr 0 of
      (arr', (k,v,_)) ->
        moveHanging size (resizeSeed (size*3) (k,v,-1) arr', 0)

    -- | After copying over to an array trice the original size,
    -- we need to move all "hanging elements", i.e., those triples (k,v,psl)
    -- where the psl is greater than the index of the cell.
    -- Theorem. The hanging elements of a robin array form a prefix.
    -- The proof is left as an exercise to the reader.
    -- Arguments: `moveHanging original-size (arr,ix)`
    moveHanging :: Keyed k =>
      Int -> (RobinArr k v, Int) -> RobinArr k v
    moveHanging size (arr,ix) = case read arr ix of
      (arr', (_,_,PSL p)) | p <= ix -> arr'
      (arr', (k,v,PSL p)) -> case write arr' ix (k,v,-1) of
        arr'' ->
          moveHanging size ((write arr'' (size + ix) (k,v,PSL p)), ix+1)

-- # Querying
--------------------------------------------------

size :: HashMap k v #-> (HashMap k v, Int)
size (HashMap (size,count) arr) = (HashMap (size,count) arr, count)

member :: Keyed k => HashMap k v #-> k -> (HashMap k v, Bool)
member hmap k = memberFromQuery (queryIndex hmap k)
  where
    memberFromQuery :: Keyed k =>
      (HashMap k v, RobinQuery k) #-> (HashMap k v, Bool)
    memberFromQuery (h, IndexToInsert _ _) = (h, False)
    memberFromQuery (h, IndexToSwap _ _) = (h, False)
    memberFromQuery (h, IndexToUpdate _ _) = (h, True)

lookup :: Keyed k => HashMap k v #-> k -> (HashMap k v, Maybe v)
lookup hmap k = (Unsafe.toLinear lookupFromIx) $ queryIndex hmap k
  where
    lookupFromIx :: (HashMap k v, RobinQuery k) -> (HashMap k v, Maybe v)
    lookupFromIx (h, IndexToInsert _ _) = (h, Nothing)
    lookupFromIx (h@(HashMap _ arr), IndexToUpdate _ ix) =
      case read arr ix of (_,(_,v,_)) -> (h, Just v)
    lookupFromIx (h, IndexToSwap _ _) = (h, Nothing)


-- | Internal function:
-- Find the index a key ought to hash into, and the PSL it should have.
-- NOTE: if the underlying array is too small, this never terminates.
queryIndex :: Keyed k => HashMap k v #-> k -> (HashMap k v, RobinQuery k)
queryIndex = Unsafe.toLinear unsafeQueryIx
  where
    unsafeQueryIx :: Keyed k =>
      HashMap k v -> k -> (HashMap k v, RobinQuery k)
    unsafeQueryIx h@(HashMap (size, _) arr) key =
      (h, walkDownArr (arr, hash key `mod` size) (key, 0) size )

    walkDownArr :: Keyed k =>
      (RobinArr k v, Int) -> (k, PSL) -> Int -> RobinQuery k
    walkDownArr (arr, ix) (key, psl) size = case read arr ix of
      (_, (_, _, psl_ix)) | psl_ix == (-1) -> IndexToInsert (key, psl) ix
      (_, (k_ix, _, _)) | key == k_ix -> IndexToUpdate (key, psl) ix
      (_, (_, _, psl_ix)) | psl_ix < psl -> IndexToSwap (key, psl) ix
      _ ->
        walkDownArr (arr, (ix + 1) `mod` size) (key, psl + PSL 1) size

-- # Internal Library
--------------------------------------------------

-- | Default intial size of underlying array.
-- We initially assume space for 8 pairs, and allocate 8*3 slots.
-- We aim to make the number of pairs some power of two.
defaultSize :: Int
defaultSize = 24
