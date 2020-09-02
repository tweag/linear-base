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
-- @HashMap k v #-> Unrestricted b@ and feed it to 'empty'.
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

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import Data.Hashable
import Data.Unrestricted.Linear
import Prelude.Linear hiding ((+), lookup, read)
import Prelude ((+))
import qualified Prelude
import qualified Unsafe.Linear as Unsafe
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
type RobinArr k v = Array (RobinVal k v)

-- | Robin values are triples of the key, value and PSL
-- (the probe sequence length).
type RobinVal k v = (PSL, (k, v))

-- | A probe sequence length
newtype PSL = PSL Int
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Num, Prelude.Show)

-- | At minimum, we need to store hashable
-- and identifiable keys
type Keyed k = (Eq k, Hashable k)

-- | The results of searching for where to insert a key
data ProbeResult k where
  -- | A key, PSL pair to insert at an index, that has not
  -- yet been touched. Invariant: the PSL must be -1 at that index.
  IndexToInsert :: (k, PSL) -> Int -> ProbeResult k
  -- A key, PSL pair to update the value at an index.
  IndexToUpdate :: (k, PSL) -> Int -> ProbeResult k
  -- | A key, PSL pair for a swap at an index.
  -- The swapped-out pair will then need to be inserted downstream.
  IndexToSwap :: (k, PSL) -> Int -> ProbeResult k


-- # Construction and Modification
--------------------------------------------------

-- | Run a computation given an empty hashmap
empty :: forall k v b.
  Keyed k => Size -> (HashMap k v #-> Unrestricted b) -> Unrestricted b
empty sz@(Size s) scope =
  Array.alloc
    s
    ( -1
    , ( error "reading error hashmap key"
      , error "reading error hashmap val"
      )
    )
    scope'
  where
    scope' :: RobinArr k v #-> Unrestricted b
    scope' arr = scope $ HashMap sz (Count 0) arr

-- | If the key is present, this update the value.
-- If not, if there's enough space, insert a new pair.
-- If there isn't enough space, resize and insert
insert :: (Keyed k, HasCallStack) => HashMap k v #-> k -> v -> HashMap k v
insert (HashMap sz@(Size s) ct@(Count c) arr) k v = case c < s of
  False -> resizeMap (HashMap sz ct arr) & \case
    !hmap' -> insert hmap' k v
  True -> tryInsertAtIndex (HashMap sz ct arr) ((hash k) `mod` s) (k,0) v

-- | Try to insert at a given index with a given PSL. So the
-- probing starts from the given index (with the given PSL).
tryInsertAtIndex :: Keyed k =>
  HashMap k v #-> Int -> (k, PSL) -> v -> HashMap k v
tryInsertAtIndex hmap ix kPSL v = probeFrom kPSL ix hmap & \case
  (HashMap sz ct arr, IndexToUpdate (k',psl') ix') ->
    HashMap sz ct (Array.write ix' (psl', (k',v)) arr)
  (HashMap sz (Count c) arr, IndexToInsert (k',psl') ix') ->
    HashMap sz (Count (c + 1)) (Array.write ix' (psl', (k',v)) arr)
  (HashMap (Size s) ct arr, IndexToSwap (k',psl') ix') -> Array.read ix' arr & \case
    (arr'', Unrestricted (p_old, (k_old, v_old))) ->
      tryInsertAtIndex
        (HashMap (Size s) ct (Array.write ix' (psl', (k',v)) arr''))
        ((ix' + 1) `mod` s)
        (k_old, p_old + 1)
        v_old

-- | Resizes the hashmap to be about 2.5 times the previous size
resizeMap :: Keyed k => HashMap k v #-> HashMap k v
resizeMap (HashMap (Size s) _ arr) = extractPairs arr & \case
  (arr', Unrestricted kvs) ->
    Array.allocBeside (s*2 + s`div`2) (-1, errKV) arr' & \(old, new) ->
      old `lseq` insertAll kvs (HashMap (Size (s*2 + s`div`2)) (Count 0) new)
  where
    errKV :: Keyed k => (k,v)
    errKV =
      ( error "reading error hashmap key"
      , error "reading error hashmap val"
      )
    extractPairs :: Keyed k =>
      RobinArr k v #-> (RobinArr k v, Unrestricted [(k,v)])
    extractPairs arr = walk arr (s - 1) []

    walk :: Keyed k =>
      RobinArr k v #-> Int -> [(k,v)] -> (RobinArr k v, Unrestricted [(k,v)])
    walk arr ix kvs
      | ix < 0 = (arr, Unrestricted kvs)
      | otherwise = Array.read ix arr & \case
        (arr', Unrestricted (-1, _)) -> walk arr' (ix-1) kvs
        (arr', Unrestricted (_, (!k,!v))) -> walk arr' (ix-1) ((k,v):kvs)

-- | 'delete h k' deletes key k and its value if it is present.
-- If it's not present, this does nothing.
delete :: Keyed k => HashMap k v #-> k -> HashMap k v
delete (HashMap sz@(Size s) ct@(Count c) arr) k =
  probeFrom (k,0) ((hash k) `mod` s) (HashMap sz ct arr) & \case
    (h, IndexToInsert _ _) -> h
    (h, IndexToSwap _ _) -> h
    (HashMap sz _ arr', IndexToUpdate _ ix) ->
      Array.write ix (-1, (error "key", error "val")) arr' & \case
        arr'' -> shiftSegmentBackward sz arr'' ((ix + 1) `mod` s) & \case
          arr''' -> HashMap sz (Count (c - 1)) arr'''

-- | Shift all cells with PSLs > 0 in a continuous segment
-- following the deleted cell, backwards by one and decrement
-- their PSLs.
shiftSegmentBackward :: Keyed k =>
  Size -> RobinArr k v #-> Int -> RobinArr k v
shiftSegmentBackward (Size s) arr ix = Array.read ix arr & \case
  (arr', Unrestricted (psl, kv)) ->
    case psl <= 0 of
      True -> arr'
      False -> Array.write ix (-1, (error "key", error "val")) arr' & \case
        arr'' ->
          shiftSegmentBackward
            (Size s)
            (Array.write ((ix+s-1) `mod` s) (psl + (-1),kv) arr'')
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
  (k, PSL) -> Int -> HashMap k v #-> (HashMap k v, ProbeResult k)
probeFrom (k, p) ix (HashMap sz@(Size s) ct arr) = Array.read ix arr & \case
  (arr', Unrestricted (PSL (-1), _)) ->
    (HashMap sz ct arr', IndexToInsert (k, p) ix)
  (arr', Unrestricted (psl,(k',_))) -> case k == k' of
    -- Note: in the True case, we must have p == psl
    True -> (HashMap sz ct arr', IndexToUpdate (k, psl) ix)
    False -> case psl < p of
      True -> (HashMap sz ct arr', IndexToSwap (k, p) ix)
      False -> probeFrom (k, p+1) ((ix+1)`mod` s) (HashMap sz ct arr')


-- # Querying
--------------------------------------------------

size :: HashMap k v #-> (HashMap k v, Int)
size (HashMap sz ct@(Count c) arr) = (HashMap sz ct arr, c)

member :: Keyed k => HashMap k v #-> k -> (HashMap k v, Bool)
member (HashMap (Size s) ct arr) k =
  probeFrom (k,0) ((hash k) `mod` s) (HashMap (Size s) ct arr) & \case
    (h, IndexToUpdate _ _) -> (h, True)
    (h, IndexToInsert _ _) -> (h, False)
    (h, IndexToSwap _ _) -> (h, False)

lookup :: Keyed k => HashMap k v #-> k -> (HashMap k v, Unrestricted (Maybe v))
lookup (HashMap (Size s) ct arr) k =
  probeFrom (k,0) ((hash k) `mod` s) (HashMap (Size s) ct arr) & \case
    (HashMap sz ct arr, IndexToUpdate _ ix) -> Array.read ix arr & \case
      (arr', Unrestricted (_,(_,v))) ->
        (HashMap sz ct arr', Unrestricted (Just v))
    (h, IndexToInsert _ _) -> (h, Unrestricted Nothing)
    (h, IndexToSwap _ _) -> (h, Unrestricted Nothing)


-- # Instances
--------------------------------------------------

instance Consumable (HashMap k v) where
  consume :: HashMap k v #-> ()
  consume (HashMap _ _ arr) = consume arr

-- # This is provided for debugging only.
instance (Show k, Show v) => Show (HashMap k v) where
  show (HashMap _ _ robinArr) = Array.toList robinArr & \case
    (arr, Unrestricted xs) -> display (lseq arr xs)

display :: (Show k, Show v) => [RobinVal k v] #-> String
display = Unsafe.toLinear wrapper
  where
    wrapper :: (Show k, Show v) => [RobinVal k v] -> String
    wrapper xs = "[" ++ display' xs ++ "]"

    display' :: (Show k, Show v) => [RobinVal k v] -> String
    display' [] = ""
    display' ((p,kv):xs) = p == (-1) & \case
      True -> ("(" ++ show p ++ ", null)") ++ ", " ++ display' xs
      False -> show (p,kv) ++ ", " ++ display' xs

