{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- This module provides mutable hashmaps with a linear interface.
--
-- To use mutable hashmaps, create a linear computation of type
-- @HashMap k v #-> Ur b@ and feed it to 'empty'.
--
-- This hashmap is implemented with robin hood hashing which has good average
-- case performance.
module Data.HashMap.Linear
  ( -- * A mutable hashmap
    HashMap,
    Keyed,
    Size(..),
    -- * Run a computation using a 'HashMap'
    empty,
    -- * Modifiers and Constructors
    insert,
    delete,
    insertAll,
    -- * Accessors
    size,
    lookup,
    member
  )
where

import Data.Array.Mutable.Linear hiding (size)
import Data.Hashable
import Data.Unrestricted.Linear
import Prelude.Linear hiding ((+), lookup, read)
import Prelude ((+))
import qualified Prelude
import GHC.Stack

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

-- | A mutable hashmap with a linear API
data HashMap k v where
  -- | HashMap (size,count) array-of-robin-values
  -- The size is the amount of memory the array takes up.
  -- The count is the number of stored mappings.
  -- Our sparseness invariant: count*3 <= size.
  HashMap :: Size -> Count -> RobinArr k v #-> HashMap k v

-- INVARIANTS:
--   * Cells are empty iff the PSL is -1.
--   * Each (RobinVal k v) has the correct PSL
--   * We NEVER evaluate the key-val pair for PSL -1
--     since it might be (error "some message")

-- | The size allocated for the array
newtype Size = Size Int
  deriving (Prelude.Num)

-- | The number of pairs stored in the hashmap
newtype Count = Count Int
  deriving (Prelude.Num)

-- | An array of Robin values
type RobinArr k v = Array (Maybe (RobinVal k v))

-- | Robin values are triples of the key, value and PSL
-- (the probe sequence length).
data RobinVal k v = RobinVal {-# UNPACK #-} !PSL k v
  deriving (Show)

incRobinValPSL :: RobinVal k v -> RobinVal k v
incRobinValPSL (RobinVal (PSL p) k v) = RobinVal (PSL (p+1)) k v

decRobinValPSL :: RobinVal k v -> RobinVal k v
decRobinValPSL (RobinVal (PSL p) k v) = RobinVal (PSL (p-1)) k v

-- | A probe sequence length
newtype PSL = PSL Int
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Num, Prelude.Show)

-- | At minimum, we need to store hashable
-- and identifiable keys
type Keyed k = (Eq k, Hashable k)

-- | The results of searching for where to insert a key
data ProbeResult k v where
  -- | An empty cell at index to insert a new element with PSL.
  IndexToInsert :: !PSL -> !Int -> ProbeResult k v
  -- | A matching cell at index with a PSL and a value to update.
  IndexToUpdate :: v -> !PSL -> !Int -> ProbeResult k v
  -- | An occupied, richer, cell which should be evicted when inserting
  -- the new element. The swapped-out cell will then need to be inserted
  -- with a higher PSL.
  IndexToSwap :: RobinVal k v -> !PSL -> !Int -> ProbeResult k v

-- # Construction and Modification
--------------------------------------------------

-- | Run a computation given an empty hashmap
empty :: forall k v b.
  Keyed k => Size -> (HashMap k v #-> Ur b) #-> Ur b
empty sz@(Size s) scope =
  alloc
    s
    Nothing
    (\arr -> scope (HashMap sz (Count 0) arr))

-- | If the key is present, this update the value.
-- If not, if there's enough space, insert a new pair.
-- If there isn't enough space, resize and insert
insert :: (Keyed k, HasCallStack) => HashMap k v #-> k -> v -> HashMap k v
insert (HashMap sz@(Size s) ct@(Count c) arr) k v = case c < s of
  False -> resizeMap (HashMap sz ct arr) & \case
    !hmap' -> insert hmap' k v
  True ->
    tryInsertAtIndex
      (HashMap sz ct arr)
      ((hash k) `mod` s)
      (RobinVal (PSL 0) k v)

-- | Try to insert at a given index with a given PSL. So the
-- probing starts from the given index (with the given PSL).
tryInsertAtIndex :: Keyed k =>
  HashMap k v #-> Int -> RobinVal k v -> HashMap k v
tryInsertAtIndex hmap ix (RobinVal psl key val) =
  probeFrom (key, psl) ix hmap & \case
    (HashMap sz ct arr, IndexToUpdate _ psl' ix') ->
      HashMap sz ct (write arr ix' (Just $ RobinVal psl' key val))
    (HashMap sz (Count c) arr, IndexToInsert psl' ix') ->
      HashMap sz (Count (c + 1)) (write arr ix' (Just $ RobinVal psl' key val))
    (HashMap (Size s) ct arr, IndexToSwap oldVal psl' ix') ->
      tryInsertAtIndex
        (HashMap (Size s) ct (write arr ix' (Just $ RobinVal psl' key val)))
        ((ix' + 1) `mod` s)
        (incRobinValPSL oldVal)

-- | Resizes the hashmap to be about 2.5 times the previous size
resizeMap :: Keyed k => HashMap k v #-> HashMap k v
resizeMap (HashMap (Size s) _ arr) = extractPairs arr & \case
  (arr', Ur kvs) ->
    allocBeside (s*2 + s`div`2) Nothing arr' & \case
      (oldArr, arr'') ->
        oldArr `lseq`
          insertAll kvs (HashMap (Size (s*2 + s`div`2)) (Count 0) arr'')
  where
    extractPairs :: Keyed k =>
      RobinArr k v #-> (RobinArr k v, Ur [(k,v)])
    extractPairs arr = walk arr (s - 1) []

    walk :: Keyed k =>
      RobinArr k v #-> Int -> [(k,v)] -> (RobinArr k v, Ur [(k,v)])
    walk arr ix kvs
      | ix < 0 = (arr, Ur kvs)
      | otherwise = read arr ix & \case
        (arr', Ur Nothing) ->
          walk arr' (ix-1) kvs
        (arr', Ur (Just (RobinVal _ k v))) ->
          walk arr' (ix-1) ((k,v):kvs)

-- | 'delete h k' deletes key k and its value if it is present.
-- If it's not present, this does nothing.
delete :: Keyed k => HashMap k v #-> k -> HashMap k v
delete (HashMap sz@(Size s) ct@(Count c) arr) k =
  probeFrom (k,0) ((hash k) `mod` s) (HashMap sz ct arr) & \case
    (h, IndexToInsert _ _) -> h
    (h, IndexToSwap _ _ _) -> h
    (HashMap sz _ arr', IndexToUpdate _ _ ix) ->
      write arr' ix Nothing & \case
        arr'' -> shiftSegmentBackward sz arr'' ((ix + 1) `mod` s) & \case
          arr''' -> HashMap sz (Count (c - 1)) arr'''

-- | Shift all cells with PSLs > 0 in a continuous segment
-- following the deleted cell, backwards by one and decrement
-- their PSLs.
shiftSegmentBackward :: Keyed k =>
  Size -> RobinArr k v #-> Int -> RobinArr k v
shiftSegmentBackward (Size s) arr ix = read arr ix & \case
  (arr', Ur Nothing) -> arr'
  (arr', Ur (Just (RobinVal 0 _ _))) -> arr'
  (arr', Ur (Just val)) ->
    write arr' ix Nothing & \arr'' ->
      shiftSegmentBackward
        (Size s)
        (write arr'' ((ix-1) `mod` s) (Just $ decRobinValPSL val))
        ((ix+1) `mod` s)

-- | 'insert' (in the provided order) a list of key-value pairs
-- to a given hashmap.
insertAll :: Keyed k => [(k, v)] -> HashMap k v #-> HashMap k v
insertAll [] hmap = hmap
insertAll ((k, v) : xs) hmap = insertAll xs (insert hmap k v)

-- | Given a key, psl of the probe so far, current unread index, and
-- a full hashmap, return a probe result: the place the key already
-- exists, a place to swap from, or an unfilled cell to write over.
probeFrom :: Keyed k =>
  (k, PSL) -> Int -> HashMap k v #-> (HashMap k v, ProbeResult k v)
probeFrom (k, p) ix (HashMap sz@(Size s) ct arr) = read arr ix & \case
  (arr', Ur Nothing) ->
    (HashMap sz ct arr', IndexToInsert p ix)
  (arr', Ur (Just robinVal'@(RobinVal psl k' v'))) ->
    case k == k' of
      -- Note: in the True case, we must have p == psl
      True -> (HashMap sz ct arr', IndexToUpdate v' psl ix)
      False -> case psl < p of
        True -> (HashMap sz ct arr', IndexToSwap robinVal' p ix)
        False -> probeFrom (k, p+1) ((ix+1)`mod` s) (HashMap sz ct arr')

-- # Querying
--------------------------------------------------

size :: HashMap k v #-> (HashMap k v, Ur Int)
size (HashMap sz ct@(Count c) arr) = (HashMap sz ct arr, Ur c)

member :: Keyed k => HashMap k v #-> k -> (HashMap k v, Ur Bool)
member (HashMap (Size s) ct arr) k =
  probeFrom (k,0) ((hash k) `mod` s) (HashMap (Size s) ct arr) & \case
    (h, IndexToUpdate _ _ _) -> (h, Ur True)
    (h, IndexToInsert _ _) -> (h, Ur False)
    (h, IndexToSwap _ _ _) -> (h, Ur False)

lookup :: Keyed k => HashMap k v #-> k -> (HashMap k v, Ur (Maybe v))
lookup (HashMap (Size s) ct arr) k =
  probeFrom (k,0) ((hash k) `mod` s) (HashMap (Size s) ct arr) & \case
    (HashMap sz ct arr, IndexToUpdate v _ _) ->
      (HashMap sz ct arr, Ur (Just v))
    (h, IndexToInsert _ _) ->
      (h, Ur Nothing)
    (h, IndexToSwap _ _ _) ->
      (h, Ur Nothing)

-- # Instances
--------------------------------------------------

instance Consumable (HashMap k v) where
  consume :: HashMap k v #-> ()
  consume (HashMap _ _ arr) = consume arr

_debugShow :: (Show k, Show v) => HashMap k v #-> String
_debugShow (HashMap _ _ robinArr) =
  toList robinArr & \(arr, Ur xs) ->
    arr `lseq` show xs
