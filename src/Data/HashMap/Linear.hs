{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
--   A mutable hashmap with a linear interface.
--   Internally, this uses robin hood hashing on mutable,
--   linear arrays.
--
-- TODO:
--
-- - Make everything linear, I don't need to use Unsafe.toLinear
-- - Add singletonWithSize
-- - Much more stuff from Data.Map

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
  deriving ( Prelude.Eq, Prelude.Ord,
             Prelude.Num, Consumable
           )

-- | An array of Robin values
type RobinArr k v = Array (RobinVal k v)

-- | At minimum, we need to store hashable
-- and identifiable keys
type EqHashable k = (Eq k, Hashable k)

-- | A mutable hashmap with a linear API
data HashMap k v where
  -- | HashMap (size,count) array-of-robin-values
  -- The size is the amount of memory the array takes up.
  -- The count is the number of stored mappings.
  -- Our sparseness invariant: count*3 <= size.
  HashMap :: EqHashable k => (Int, Int) -> RobinArr k v #-> HashMap k v

-- INVARIANTS:
--   * Cells are empty iff the PSL is -1.
--   * We are sparse: count*3 <= size, with correct values of count and size
--   * Each (RobinVal k v) has the correct PSL

-- | The results of searching for where to insert a key
data RobinQuery k where
  -- | A key, PSL pair to insert at an index, that has not
  -- yet been touched. Invariant: the PSL must be -1 at that index.
  IndexToInsert :: EqHashable k => (k, PSL) -> Int -> RobinQuery k
  -- A key, PSL pair to update the value at an index.
  IndexToUpdate :: EqHashable k => (k, PSL) -> Int -> RobinQuery k
  -- | A key, PSL pair for a swap at an index.
  -- The swapped-out pair will then need to be inserted downstream.
  IndexToSwap :: EqHashable k => (k, PSL) -> Int -> RobinQuery k

-- # Construction and Modification
--------------------------------------------------

-- | Run a computation using a singleton hashmap
singleton :: EqHashable k =>
  (k,v) -> (HashMap k v #-> Unrestricted b) -> Unrestricted b
singleton (k :: k, v :: v) (f :: HashMap k v #-> Unrestricted b) =
  alloc defaultSize (k,v,-1) applyHM
  where
    applyHM :: Array (RobinVal k v) #-> Unrestricted b
    applyHM arr = f $
      HashMap (defaultSize, 1) (write arr (hash k `mod` defaultSize) (k,v,0))

-- XXX: re-write linearly
alter ::  EqHashable k => HashMap k v #-> (Maybe v -> Maybe v) -> k -> HashMap k v
alter = Unsafe.toLinear unsafeAlter
  where
    unsafeAlter :: EqHashable k =>
      HashMap k v -> (Maybe v -> Maybe v) -> k -> HashMap k v
    unsafeAlter hmap f k =
      case lookup hmap k of
        (hmap', maybeV) -> case f maybeV of
          Nothing -> delete hmap' k
          Just v -> insert hmap' k v

-- | If present, update value, otherwise insert new mapping
insert :: EqHashable k => HashMap k v #-> k -> v -> HashMap k v
insert hmap k v = insertFromQuery v $ queryIndex hmap k
  where
    insertFromQuery :: EqHashable k =>
      v #-> (HashMap k v, RobinQuery k) #-> HashMap k v
    insertFromQuery v (HashMap sizes arr, IndexToInsert (k,psl) ix) =
      HashMap sizes (write (maybeResize arr sizes) ix (k,v,psl))
    insertFromQuery v (HashMap sizes arr, IndexToUpdate (k,psl) ix) =
      HashMap sizes (write arr ix (k,v,psl))
    insertFromQuery v (HashMap sizes arr, IndexToSwap (k,psl) ix) =
      (Unsafe.toLinear swapFromRead) (read arr ix) sizes ix (k,v,psl)

    swapFromRead :: EqHashable k => (RobinArr k v, RobinVal k v) ->
      (Int, Int) -> Int -> RobinVal k v #-> HashMap k v
    swapFromRead (arr', (k',v',_)) sizes ix (k,v,psl) =
      insert (HashMap sizes (write arr' ix (k,v,psl))) k' v'

-- | If present, deletes key-value pair, otherwise does nothing
delete :: EqHashable k => HashMap k v #-> k -> HashMap k v
delete = undefined

-- If the load is too high, resize
maybeResize :: EqHashable k => RobinArr k v #-> (Int, Int) -> RobinArr k v
maybeResize = undefined

-- # Querying
--------------------------------------------------

size :: HashMap k v #-> (HashMap k v, Int)
size (HashMap (size,count) arr) = (HashMap (size,count) arr, count)

member :: EqHashable k => HashMap k v #-> k -> (HashMap k v, Bool)
member hmap k = memberFromQuery (queryIndex hmap k)
  where
    memberFromQuery :: EqHashable k =>
      (HashMap k v, RobinQuery k) #-> (HashMap k v, Bool)
    memberFromQuery (h, IndexToInsert _ _) = (h, False)
    memberFromQuery (h, IndexToSwap _ _) = (h, False)
    memberFromQuery (h, IndexToUpdate _ _) = (h, True)

lookup :: EqHashable k => HashMap k v #-> k -> (HashMap k v, Maybe v)
lookup hmap k = (Unsafe.toLinear lookupFromIx) $ queryIndex hmap k
  where
    lookupFromIx :: (HashMap k v, RobinQuery k) -> (HashMap k v, Maybe v)
    lookupFromIx (h, IndexToInsert _ _) = (h, Nothing)
    lookupFromIx (h@(HashMap _ arr), IndexToUpdate _ ix) =
      case read arr ix of (_,(_,v,_)) -> (h, Just v)
    lookupFromIx (h, IndexToSwap _ _) = (h, Nothing)


-- | Internal function:
-- Find the index a key ought to hash into, and the PSL it should have.
queryIndex :: EqHashable k => HashMap k v #-> k -> (HashMap k v, RobinQuery k)
queryIndex = Unsafe.toLinear unsafeQueryIx
  where
    unsafeQueryIx :: EqHashable k => HashMap k v -> k -> (HashMap k v, RobinQuery k)
    unsafeQueryIx h@(HashMap (size, _) arr) key =
      (h, walkDownArr (arr, hash key `mod` size) (key, 0))

    walkDownArr :: EqHashable k => (RobinArr k v, Int) -> (k, PSL) -> RobinQuery k
    walkDownArr (arr, ix) (key, psl) = case read arr ix of
      (_, (k_ix, v_ix, psl_ix)) | psl_ix == (-1) -> IndexToInsert (key, psl) ix
      (_, (k_ix, v_ix, psl_ix)) | key == k_ix -> IndexToUpdate (key, psl) ix
      (_, (k_ix, v_ix, psl_ix)) | psl_ix < psl -> IndexToSwap (key, psl) ix
      (_, (k_ix, v_ix, psl_ix)) -> walkDownArr (arr, ix + 1) (key, psl + PSL 1)

-- # Internal Library
--------------------------------------------------

-- | Default intial size of underlying array.
-- We initially assume space for 8 pairs, and allocate
-- 8*2. We aim to make the number of pairs some power of two.
defaultSize :: Int
defaultSize = (2^3) * 3
