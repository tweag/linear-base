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

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
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
  -- |
  -- The count is the number of stored mappings.
  -- Our sparseness invariant: count*3 <= size arr.
  HashMap :: Count -> RobinArr k v #-> HashMap k v

-- INVARIANTS:
--   * Cells are empty iff the PSL is -1.
--   * Each (RobinVal k v) has the correct PSL
--   * We NEVER evaluate the key-val pair for PSL -1
--     since it might be (error "some message")

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
  Keyed k => Int -> (HashMap k v #-> Ur b) #-> Ur b
empty size scope =
  Array.alloc
    size
    Nothing
    (\arr -> scope (HashMap (Count 0) arr))

-- | If the key is present, this update the value.
-- If not, if there's enough space, insert a new pair.
-- If there isn't enough space, resize and insert
insert :: (Keyed k, HasCallStack) => HashMap k v #-> k -> v -> HashMap k v
insert hm k v =
  capacity hm & \(hm', Ur cap) ->
    size hm' & \(hm'', Ur size) ->
      case size < cap of
        False -> resizeMap hm'' & \hm''' ->
          insert hm''' k v
        True ->
          idealIndexForKey k hm'' & \(hm''', Ur idx) ->
            tryInsertAtIndex hm''' idx (RobinVal (PSL 0) k v)

-- | Try to insert at a given index with a given PSL. So the
-- probing starts from the given index (with the given PSL).
tryInsertAtIndex :: Keyed k =>
  HashMap k v #-> Int -> RobinVal k v -> HashMap k v
tryInsertAtIndex hmap ix (RobinVal psl key val) =
  probeFrom (key, psl) ix hmap & \case
    (HashMap ct arr, IndexToUpdate _ psl' ix') ->
      HashMap ct (Array.write arr ix' (Just $ RobinVal psl' key val))
    (HashMap (Count c) arr, IndexToInsert psl' ix') ->
      HashMap (Count (c + 1)) (Array.write arr ix' (Just $ RobinVal psl' key val))
    (hm, IndexToSwap oldVal psl' ix') ->
      capacity hm  & \(HashMap ct arr, Ur cap) ->
        tryInsertAtIndex
          (HashMap ct (Array.write arr ix' (Just $ RobinVal psl' key val)))
          ((ix' + 1) `mod` cap)
          (incRobinValPSL oldVal)

-- | Resizes the hashmap to be about 2.5 times the previous size
resizeMap :: Keyed k => HashMap k v #-> HashMap k v
resizeMap hm =
  capacity hm & \(HashMap _ arr, Ur cap) ->
    extractPairs cap arr & \(arr', Ur kvs) ->
      let newCap = cap*2 + cap`div`2
      in Array.allocBeside newCap Nothing arr' & \(oldArr, newArr) ->
           oldArr `lseq`
             insertAll kvs (HashMap (Count 0) newArr)
  where
    extractPairs :: Keyed k =>
      Int -> RobinArr k v #-> (RobinArr k v, Ur [(k,v)])
    extractPairs size arr = walk arr (size - 1) []

    walk :: Keyed k =>
      RobinArr k v #-> Int -> [(k,v)] -> (RobinArr k v, Ur [(k,v)])
    walk arr ix kvs
      | ix < 0 = (arr, Ur kvs)
      | otherwise = Array.read arr ix & \case
        (arr', Ur Nothing) ->
          walk arr' (ix-1) kvs
        (arr', Ur (Just (RobinVal _ k v))) ->
          walk arr' (ix-1) ((k,v):kvs)

-- | 'delete h k' deletes key k and its value if it is present.
-- If it's not present, this does nothing.
delete :: Keyed k => HashMap k v #-> k -> HashMap k v
delete hm k =
  capacity hm & \(HashMap ct@(Count c) arr, Ur cap) ->
    probeFrom (k,0) ((hash k) `mod` cap) (HashMap ct arr) & \case
      (h, IndexToInsert _ _) -> h
      (h, IndexToSwap _ _ _) -> h
      (HashMap _ arr', IndexToUpdate _ _ ix) ->
        Array.write arr' ix Nothing & \arr'' ->
          shiftSegmentBackward cap arr'' ((ix + 1) `mod` cap) & \arr''' ->
            HashMap (Count (c - 1)) arr'''

-- | Shift all cells with PSLs > 0 in a continuous segment
-- following the deleted cell, backwards by one and decrement
-- their PSLs.
shiftSegmentBackward :: Keyed k =>
  Int -> RobinArr k v #-> Int -> RobinArr k v
shiftSegmentBackward s arr ix = Array.read arr ix & \case
  (arr', Ur Nothing) -> arr'
  (arr', Ur (Just (RobinVal 0 _ _))) -> arr'
  (arr', Ur (Just val)) ->
    Array.write arr' ix Nothing & \arr'' ->
      shiftSegmentBackward
        s
        (Array.write arr'' ((ix-1) `mod` s) (Just $ decRobinValPSL val))
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
probeFrom (k, p) ix (HashMap ct arr) = Array.read arr ix & \case
  (arr', Ur Nothing) ->
    (HashMap ct arr', IndexToInsert p ix)
  (arr', Ur (Just robinVal'@(RobinVal psl k' v'))) ->
    case k == k' of
      -- Note: in the True case, we must have p == psl
      True -> (HashMap ct arr', IndexToUpdate v' psl ix)
      False -> case psl < p of
        True -> (HashMap ct arr', IndexToSwap robinVal' p ix)
        False ->
          capacity (HashMap ct arr') & \(HashMap ct' arr'', Ur cap) ->
            probeFrom (k, p+1) ((ix+1)`mod` cap) (HashMap ct' arr'')

-- # Querying
--------------------------------------------------

size :: HashMap k v #-> (HashMap k v, Ur Int)
size (HashMap ct@(Count c) arr) = (HashMap ct arr, Ur c)

capacity :: HashMap k v #-> (HashMap k v, Ur Int)
capacity (HashMap ct arr) =
  Array.size arr & \(arr', len) ->
    (HashMap ct arr', len)

member :: Keyed k => HashMap k v #-> k -> (HashMap k v, Ur Bool)
member hm k =
  idealIndexForKey k hm & \(hm', Ur idx) ->
    probeFrom (k, 0) idx hm' & \case
      (h, IndexToUpdate _ _ _) -> (h, Ur True)
      (h, IndexToInsert _ _) -> (h, Ur False)
      (h, IndexToSwap _ _ _) -> (h, Ur False)

lookup :: Keyed k => HashMap k v #-> k -> (HashMap k v, Ur (Maybe v))
lookup hm k =
  idealIndexForKey k hm & \(hm', Ur idx) ->
    probeFrom (k,0) idx hm' & \case
      (h, IndexToUpdate v _ _) ->
        (h, Ur (Just v))
      (h, IndexToInsert _ _) ->
        (h, Ur Nothing)
      (h, IndexToSwap _ _ _) ->
        (h, Ur Nothing)

idealIndexForKey
  :: Keyed k
  => k -> HashMap k v #-> (HashMap k v, Ur Int)
idealIndexForKey k hm =
  capacity hm & \(hm', Ur cap) ->
    (hm', Ur (hash k `mod` cap))

-- # Instances
--------------------------------------------------

instance Consumable (HashMap k v) where
  consume :: HashMap k v #-> ()
  consume (HashMap _ arr) = consume arr

_debugShow :: (Show k, Show v) => HashMap k v #-> String
_debugShow (HashMap _ robinArr) =
  Array.toList robinArr & \(Ur xs) -> show xs
