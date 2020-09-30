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
-- It is implemented with Robin Hood hashing which has amortized
-- constant time lookups and updates.
module Data.HashMap.Linear
  ( -- * A mutable hashmap
    HashMap,
    Keyed,
    -- * Constructors
    empty,
    fromList,
    -- * Modifiers
    insert,
    insertAll,
    delete,
    filter,
    filterWithKey,
    mapMaybe,
    mapMaybeWithKey,
    shrinkToFit,
    -- * Accessors
    size,
    capacity,
    lookup,
    member,
    toList,
    -- * Combining maps
    union,
    unionWith,
    intersectionWith
  )
where

import Data.Array.Mutable.Linear (Array)
import qualified Data.Functor.Linear as Data
import qualified Data.Array.Mutable.Linear as Array
import Data.Hashable
import Data.Unrestricted.Linear
import Prelude.Linear hiding ((+), lookup, read, filter, mapMaybe, insert)
import Prelude ((+))
import qualified Data.Maybe as NonLinear
import qualified Data.Function as NonLinear
import qualified Prelude
import qualified Unsafe.Linear as Unsafe

-- # Implementation Notes
-- This is a simple implementatation of robin hood hashing.
--
-- See these links:
--
-- * https://programming.guide/robin-hood-hashing.html
-- * https://andre.arko.net/2017/08/24/robin-hood-hashing/
-- * https://cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf
--

-- # Constants
--------------------------------------------------

-- | When to trigger a resize.
--
-- A high load factor usually is not desirable because it makes operations
-- do more probes. A very low one is also not desirable since there're some
-- operations which take time relative to the 'capacity'.
--
-- This should be between (0, 1)
--
-- The value 0.75 is what Java uses:
-- https://docs.oracle.com/javase/10/docs/api/java/util/HashMap.html
constMaxLoadFactor :: Float
constMaxLoadFactor = 0.75

-- | When resizing, the capacity will be multiplied by this amount.
--
-- This should be greater than one.
constGrowthFactor :: Int
constGrowthFactor = 2

-- # Core Data Types
--------------------------------------------------

-- | A mutable hashmap with a linear interface.
data HashMap k v where
  -- |
  -- @loadFactor m = size m / cap m@
  --
  -- Invariants:
  -- - array is non-empty
  -- - (count / capacity) <= constMaxLoadFactor.
  HashMap
    :: Int -- ^ The number of stored (key, value) pairs.
    -> RobinArr k v -- ^ Underlying array.
    #-> HashMap k v

-- | An array of Robin values
--
-- Each cell is Nothing if empty and is a RobinVal with the correct
-- PSL otherwise.
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

-- | The results of searching for where to insert a key.
--
-- PSL's on the constructors are the probes spent from the query, this
-- might be different than PSL's of the cell at the returned index
-- (in case of `IndexToSwap` constructor).
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

-- | Run a computation with an empty 'HashMap' with given capacity.
empty :: forall k v b.
  Keyed k => Int -> (HashMap k v #-> Ur b) #-> Ur b
empty size scope =
  Array.alloc
    (max 1 size)
    Nothing
    (\arr -> scope (HashMap 0 arr))

-- | Create an empty HashMap, using another as a uniqueness proof.
allocBeside :: Keyed k => Int -> HashMap k' v' #-> (HashMap k v, HashMap k' v')
allocBeside size (HashMap s' arr) =
  Array.allocBeside (max 1 size) Nothing arr & \(arr', arr'') ->
    (HashMap size arr', HashMap s' arr'')

-- | Run a computation with an 'HashMap' containing given key-value pairs.
fromList :: forall k v b.
  Keyed k => [(k, v)] -> (HashMap k v #-> Ur b) #-> Ur b
fromList xs scope =
  Array.alloc
    (max
      1
      (ceiling @Float @Int (fromIntegral (Prelude.length xs) / constMaxLoadFactor)))
    Nothing
    (\arr -> scope (insertAll xs (HashMap 0 arr)))

-- | The most general modification function; which can insert, update or delete
-- a value of the key.
alter :: Keyed k => HashMap k v #-> k -> (Maybe v -> Maybe v) -> HashMap k v
alter hm key f =
  idealIndexForKey key hm & \(Ur idx, hm') ->
    probeFrom (key, 0) idx hm' & \case
      -- The key does not exist, and there is an empty cell to insert.
      (HashMap count arr, IndexToInsert psl ix) ->
        case f Nothing of
          -- We don't need to insert anything.
          Nothing -> HashMap count arr
          -- We need to insert a new key.
          Just v->
            HashMap
             (count+1)
             (Array.write arr ix (Just (RobinVal psl key v)))
             & growMapIfNecessary
      -- The key exists.
      (hm'', IndexToUpdate v psl ix) ->
        capacity hm'' & \(Ur cap, HashMap count arr) ->
          case f (Just v) of
            -- We need to delete it.
            Nothing ->
              Array.write arr ix Nothing & \arr' ->
                shiftSegmentBackward cap arr' ((ix + 1) `mod` cap) & \arr'' ->
                  HashMap
                    (count - 1)
                    arr''
            -- We need to update it.
            Just new->
              HashMap
                count
                (Array.write arr ix (Just (RobinVal psl key new)))
      -- The key does not exist, but there is a key to evict.
      (hm, IndexToSwap evicted psl ix) ->
        case f Nothing of
          -- We don't need to insert anything.
          Nothing -> hm
          -- We need to insert a new key.
          Just v->
            capacity hm & \(Ur cap, HashMap count arr) ->
              tryInsertAtIndex
                (HashMap
                  count
                  (Array.write arr ix (Just (RobinVal psl key v))))
                ((ix + 1) `mod` cap)
                (incRobinValPSL evicted)
              & growMapIfNecessary

-- | Insert a key value pair to a 'HashMap'. It overwrites the previous
-- value if it exists.
insert :: Keyed k => HashMap k v #-> k -> v -> HashMap k v
insert hm k v = alter hm k (\_ -> Just v)

-- | Delete a key from a 'HashMap'. Does nothing if the key does not
-- exist.
delete :: Keyed k => HashMap k v #-> k -> HashMap k v
delete hm k = alter hm k (\_ -> Nothing)

-- | 'insert' (in the provided order) the given key-value pairs to
-- the hashmap.
insertAll :: Keyed k => [(k, v)] -> HashMap k v #-> HashMap k v
insertAll [] hmap = hmap
insertAll ((k, v) : xs) hmap = insertAll xs (insert hmap k v)
-- TODO: Do a resize first on the length of the input.

-- | A version of 'fmap' which can throw out the elements.
--
-- Complexity: O(capacity hm)
mapMaybe :: Keyed k => (v -> Maybe v') -> HashMap k v #-> HashMap k v'
mapMaybe f = mapMaybeWithKey (\_k v -> f v)

-- | Same as 'mapMaybe', but also has access to the keys.
mapMaybeWithKey :: Keyed k => (k -> v -> Maybe v') -> HashMap k v #-> HashMap k v'
mapMaybeWithKey (f :: k -> v -> Maybe v') (HashMap _ arr') =
  Array.size arr' & \case
    (Ur 0, arr'') ->
      recurOrReturn True 0 0 0 0 arr'' & \(arr''', Ur _) ->
        HashMap 0 arr'''
    (Ur sz, arr'') ->
      numSpillovers arr'' & \(Ur spillovers, arr''') ->
        go
          spillovers
          ((sz - 1 + spillovers) `mod` sz)
          sz
          0
          arr'''
          & \(arr'''', Ur b) -> HashMap b arr''''
 where
  go :: Int -- ^ Current index
     -> Int -- ^ Last index to check
     -> Int -- ^ Size of the array
     -> Int -- ^ Accumulated count of pairs that are present after the map.
     -> RobinArr k v
     #-> (RobinArr k v', Ur Int)
  go curr end sz ret arr =
    Array.read arr curr & \case
      (Ur Nothing, arr') ->
        recurOrReturn True curr end sz ret arr'
      (Ur (Just (RobinVal psl k v)), arr') ->
        case f k v of
          Just v' ->
           Array.write arr' curr
             (Just (RobinVal psl k (Unsafe.coerce @v' @v v')))
             & recurOrReturn True curr end sz (ret + 1)
          Nothing ->
            Array.write arr' curr Nothing
              & \arr'' -> shiftSegmentBackward sz arr'' ((curr + 1) `mod` sz)
              & recurOrReturn False curr end sz ret

  -- Takes the same parameter as 'go', used to control when to end
  -- the recursion.
  recurOrReturn :: Bool -- ^ Whether the curr should be increased
                -> Int -> Int -> Int -> Int
                -> RobinArr k v #-> (RobinArr k v', Ur Int)
  recurOrReturn incr curr end sz ret arr
    | curr == end =
      ( Unsafe.coerce @(RobinArr k v) @(RobinArr k v') arr
      , Ur ret
      )
    | otherwise =
      go
        (if incr then (curr + 1) `mod` sz else curr)
        end sz ret arr

-- | Complexity: O(capacity hm)
filterWithKey :: Keyed k => (k -> v -> Bool) -> HashMap k v #-> HashMap k v
filterWithKey f =
  mapMaybeWithKey
    (\k v -> if f k v then Just v else Nothing)

-- | Complexity: O(capacity hm)
filter :: Keyed k => (v -> Bool) -> HashMap k v #-> HashMap k v
filter f = filterWithKey (\_k v -> f v)

-- | Union of two maps using the provided function on conflicts.
--
-- Complexity: O(min(capacity hm1, capacity hm2)
unionWith
  :: Keyed k => (v -> v -> v)
  -> HashMap k v #-> HashMap k v #-> HashMap k v
unionWith onConflict (hm1 :: HashMap k v) hm2 =
  -- To insert the elements in smaller map to the larger map, we
  -- compare their capacities, and flip the arguments if necessary.
  capacity hm1 & \(Ur cap1, hm1') ->
    capacity hm2 & \(Ur cap2, hm2') ->
      if cap1 > cap2
      then go onConflict hm1' (toList hm2')
      else go (\v2 v1 -> onConflict v1 v2) hm2' (toList hm1')
  where
    go :: (v -> v -> v)
       -> HashMap k v -- ^ larger map
       #-> Ur [(k, v)] -- ^ contents of the smaller map
       #-> HashMap k v
    go _ hm (Ur []) = hm
    go f hm (Ur ((k, vr):xs)) =
      alter hm k (\case
        Nothing -> Just vr
        Just vl -> Just (f vl vr))
        & \hm -> go f hm (Ur xs)

-- | A right-biased union.
--
-- Complexity: O(min(capacity hm1, capacity hm2)
union :: Keyed k => HashMap k v #-> HashMap k v #-> HashMap k v
union hm1 hm2 = unionWith (\_v1 v2 -> v2) hm1 hm2

-- | Intersection of two maps with the provided combine function.
--
-- Complexity: O(min(capacity hm1, capacity hm2)
intersectionWith
  :: Keyed k
  => (a -> b -> c)
  -> HashMap k a #-> HashMap k b #-> HashMap k c
intersectionWith combine (hm1 :: HashMap k a') hm2 =
  allocBeside 0 hm1 & \(hmNew, hm1') ->
    capacity hm1' & \(Ur cap1, hm1'') ->
      capacity hm2 & \(Ur cap2, hm2') ->
        if cap1 > cap2
        then go combine hm1'' (toList hm2') hmNew
        else go (\v2 v1 -> combine v1 v2) hm2' (toList hm1'') hmNew
 where
   -- Iterate over the smaller map, while checking for the matches
   -- on the bigger map; and accumulate results on a third map.
   go :: (a -> b -> c)
      -> HashMap k a #-> Ur [(k, b)]
      #-> HashMap k c #-> HashMap k c
   go _ hm (Ur []) acc = hm `lseq` acc
   go f hm (Ur ((k, b):xs)) acc =
     lookup hm k & \case
       (Ur Nothing, hm') -> go f hm' (Ur xs) acc
       (Ur (Just a), hm') -> go f hm' (Ur xs) (insert acc k (f a b))

-- |
-- Reduce the 'HashMap' 'capacity' to decrease wasted memory. Returns
-- a semantically identical 'HashMap'.
--
-- This is only useful after a lot of deletes.
--
-- Complexity: O(capacity hm)
shrinkToFit :: Keyed k => HashMap k a #-> HashMap k a
shrinkToFit hm =
  size hm & \(Ur size, hm') ->
    let targetSize = ceiling
          (Prelude.max 1 (fromIntegral size Prelude./ constMaxLoadFactor))
    in  resize targetSize hm'

-- # Querying
--------------------------------------------------

-- | Number of key-value pairs inside the 'HashMap'
size :: HashMap k v #-> (Ur Int, HashMap k v)
size (HashMap ct arr) = (Ur ct, HashMap ct arr)

-- | Maximum number of elements the HashMap can store without
-- resizing. However, for performance reasons, the 'HashMap' might be
-- before full.
--
-- Use 'shrinkToFit' to reduce the wasted space.
capacity :: HashMap k v #-> (Ur Int, HashMap k v)
capacity (HashMap ct arr) =
  Array.size arr & \(len, arr') ->
    (len, HashMap ct arr')

-- | Look up a value from a 'HashMap'.
lookup :: Keyed k => HashMap k v #-> k -> (Ur (Maybe v), HashMap k v)
lookup hm k =
  idealIndexForKey k hm & \(Ur idx, hm') ->
    probeFrom (k,0) idx hm' & \case
      (h, IndexToUpdate v _ _) ->
        (Ur (Just v), h)
      (h, IndexToInsert _ _) ->
        (Ur Nothing, h)
      (h, IndexToSwap _ _ _) ->
        (Ur Nothing, h)

-- | Check if the given key exists.
member :: Keyed k => HashMap k v #-> k -> (Ur Bool, HashMap k v)
member hm k =
  lookup hm k & \case
    (Ur Nothing, hm') -> (Ur False, hm')
    (Ur (Just _), hm') -> (Ur True, hm')

-- | Converts a HashMap to a lazy list.
toList :: HashMap k v #-> Ur [(k, v)]
toList (HashMap _ arr) =
  Array.toList arr & \(Ur elems) ->
    elems
      NonLinear.& NonLinear.catMaybes
      NonLinear.& Prelude.map (\(RobinVal _ k v) -> (k, v))
      NonLinear.& Ur

-- # Instances
--------------------------------------------------

instance Consumable (HashMap k v) where
  consume :: HashMap k v #-> ()
  consume (HashMap _ arr) = consume arr

instance Dupable (HashMap k v) where
  dup2 (HashMap i arr) = dup2 arr & \(a1, a2) ->
    (HashMap i a1, HashMap i a2)

instance Data.Functor (HashMap k) where
  fmap f (HashMap c arr) =
    HashMap c $
      Data.fmap
        (\case
          Nothing -> Nothing
          Just (RobinVal p k v) -> Just (RobinVal p k (f v))
        )
        arr

instance Prelude.Semigroup (HashMap k v) where
  (<>) = error "Prelude.<>: invariant violation, unrestricted HashMap"

instance Keyed k => Semigroup (HashMap k v) where
  (<>) = union

-- # Internal library
--------------------------------------------------

_debugShow :: (Show k, Show v) => HashMap k v #-> String
_debugShow (HashMap _ robinArr) =
  Array.toList robinArr & \(Ur xs) -> show xs

-- | When using Robin-Hood hashing, the beginning of the array
-- can contain elements "spilled over" from the end of the array.
--
-- This function finds the number of spillovers. The spillovers can
-- be determined by the length of the prefix of the array where
-- PSL > index. This works since they form a contiguous segment.
numSpillovers :: RobinArr k v #-> (Ur Int, RobinArr k v)
numSpillovers arr =
  Array.size arr & \(Ur sz, arr') ->
    go 0 sz arr'
 where
  go :: Int -> Int -> RobinArr k v #-> (Ur Int, RobinArr k v)
  go curr sz arr
    | curr == sz = (Ur 0, arr) -- This should only happen when the
                               -- Array is empty.
    | otherwise =
      Array.read arr curr & \case
        (Ur Nothing, arr') -> (Ur curr, arr')
        (Ur (Just (RobinVal (PSL psl) _ _)), arr')
          | psl <= curr -> (Ur curr, arr')
          | otherwise -> go (curr+1) sz arr'

idealIndexForKey
  :: Keyed k
  => k -> HashMap k v #-> (Ur Int, HashMap k v)
idealIndexForKey k hm =
  capacity hm & \(Ur cap, hm') ->
    (Ur (mod (hash k) cap), hm')

-- | Given a key, psl of the probe so far, current unread index, and
-- a full hashmap, return a probe result: the place the key already
-- exists, a place to swap from, or an unfilled cell to write over.
probeFrom :: Keyed k =>
  (k, PSL) -> Int -> HashMap k v #-> (HashMap k v, ProbeResult k v)
probeFrom (k, p) ix (HashMap ct arr) = Array.read arr ix & \case
  (Ur Nothing, arr') ->
    (HashMap ct arr', IndexToInsert p ix)
  (Ur (Just robinVal'@(RobinVal psl k' v')), arr') ->
    case k == k' of
      -- Note: in the True case, we must have p == psl
      True -> (HashMap ct arr', IndexToUpdate v' psl ix)
      False -> case psl Prelude.< p of
        True -> (HashMap ct arr', IndexToSwap robinVal' p ix)
        False ->
          capacity (HashMap ct arr') & \(Ur cap, HashMap ct' arr'') ->
            probeFrom (k, p+1) ((ix+1)`mod` cap) (HashMap ct' arr'')

-- | Try to insert at a given index with a given PSL. So the
-- probing starts from the given index (with the given PSL).
tryInsertAtIndex :: Keyed k =>
  HashMap k v #-> Int -> RobinVal k v -> HashMap k v
tryInsertAtIndex hmap ix (RobinVal psl key val) =
  probeFrom (key, psl) ix hmap & \case
    (HashMap ct arr, IndexToUpdate _ psl' ix') ->
      HashMap ct (Array.write arr ix' (Just $ RobinVal psl' key val))
    (HashMap c arr, IndexToInsert psl' ix') ->
      HashMap (c + 1) (Array.write arr ix' (Just $ RobinVal psl' key val))
    (hm, IndexToSwap oldVal psl' ix') ->
      capacity hm  & \(Ur cap, HashMap ct arr) ->
        tryInsertAtIndex
          (HashMap ct (Array.write arr ix' (Just $ RobinVal psl' key val)))
          ((ix' + 1) `mod` cap)
          (incRobinValPSL oldVal)

-- | Shift all cells with PSLs > 0 in a continuous segment
-- following the deleted cell, backwards by one and decrement
-- their PSLs.
shiftSegmentBackward :: Keyed k =>
  Int -> RobinArr k v #-> Int -> RobinArr k v
shiftSegmentBackward s arr ix = Array.read arr ix & \case
  (Ur Nothing, arr') -> arr'
  (Ur (Just (RobinVal 0 _ _)), arr') -> arr'
  (Ur (Just val), arr') ->
    Array.write arr' ix Nothing & \arr'' ->
      shiftSegmentBackward
        s
        (Array.write arr'' ((ix-1) `mod` s) (Just $ decRobinValPSL val))
        ((ix+1) `mod` s)
-- TODO: This does twice as much writes than necessary, it first empties
-- the cell, just to update it again at the next call. We can save some
-- writes by only emptying the last cell.

-- | Makes sure that the map is not exceeding its utilization threshold
-- (constMaxUtilization), resizes (constGrowthFactor) if necessary.
growMapIfNecessary :: Keyed k => HashMap k v #-> HashMap k v
growMapIfNecessary hm =
  capacity hm & \(Ur cap, hm') ->
   size hm' & \(Ur sz, hm'') ->
    let load = fromIntegral sz / fromIntegral cap
    in if load Prelude.< constMaxLoadFactor
       then hm''
       else
         let newCap = max 1 (cap * constGrowthFactor)
         in  resize newCap hm''

-- | Resizes the HashMap to given capacity.
--
-- Invariant: Given capacity should be greater than the size, this is not
-- checked.
resize :: Keyed k => Int -> HashMap k v #-> HashMap k v
resize targetSize (HashMap _ arr) =
  Array.allocBeside targetSize Nothing arr & \(newArr, oldArr) ->
    Array.toList oldArr & \(Ur elems) ->
      let xs =
            elems
              NonLinear.& NonLinear.catMaybes
              NonLinear.& Prelude.map (\(RobinVal _ k v) -> (k, v))
       in  insertAll xs (HashMap 0 newArr)
-- TODO: 'insertAll' keeps checking capacity on each insert. We should
-- replace it with a faster unsafe variant.
