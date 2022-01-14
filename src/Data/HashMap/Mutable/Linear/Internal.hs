{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.HashMap.Mutable.Linear.Internal where

import qualified Control.Functor.Linear as Control
import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import qualified Data.Function as NonLinear
import Data.Functor.Identity hiding (runIdentity)
import qualified Data.Functor.Linear as Data
import Data.Hashable
import qualified Data.Maybe as NonLinear
import Data.Unrestricted.Linear
import Prelude.Linear hiding (filter, insert, lookup, mapMaybe, read, (+))
import Unsafe.Coerce (unsafeCoerce)
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
  HashMap ::
    -- | The number of stored (key, value) pairs.
    !Int ->
    -- | Capacity of the underlying array (cached here)
    !Int ->
    -- | Underlying array.
    !(RobinArr k v) %1 ->
    HashMap k v

-- | An array of Robin values
--
-- Each cell is Nothing if empty and is a RobinVal with the correct
-- PSL otherwise.
type RobinArr k v = Array (Maybe (RobinVal k v))

-- | Robin values are triples of the key, value and PSL
-- (the probe sequence length).
data RobinVal k v = RobinVal !PSL !k v
  deriving (Show)

incRobinValPSL :: RobinVal k v -> RobinVal k v
incRobinValPSL (RobinVal (PSL p) k v) = RobinVal (PSL (p + 1)) k v

decRobinValPSL :: RobinVal k v -> RobinVal k v
decRobinValPSL (RobinVal (PSL p) k v) = RobinVal (PSL (p - 1)) k v

-- | A probe sequence length
newtype PSL = PSL Int
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Num, Prelude.Show)

-- | At minimum, we need to store hashable
-- and identifiable keys
type Keyed k = (Prelude.Eq k, Hashable k)

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
empty ::
  forall k v b.
  Keyed k =>
  Int ->
  (HashMap k v %1 -> Ur b) %1 ->
  Ur b
empty size scope =
  let cap = max 1 size
   in Array.alloc cap Nothing (\arr -> scope (HashMap 0 cap arr))

-- | Create an empty HashMap, using another as a uniqueness proof.
allocBeside :: Keyed k => Int -> HashMap k' v' %1 -> (HashMap k v, HashMap k' v')
allocBeside size (HashMap s' c' arr) =
  let cap = max 1 size
   in Array.allocBeside cap Nothing arr & \(arr', arr'') ->
        (HashMap size cap arr', HashMap s' c' arr'')

-- | Run a computation with an 'HashMap' containing given key-value pairs.
fromList ::
  forall k v b.
  Keyed k =>
  [(k, v)] ->
  (HashMap k v %1 -> Ur b) %1 ->
  Ur b
fromList xs scope =
  let cap =
        max
          1
          (ceiling @Float @Int (fromIntegral (Prelude.length xs) / constMaxLoadFactor))
   in Array.alloc
        cap
        Nothing
        (\arr -> scope (insertAll xs (HashMap 0 cap arr)))

-- | The most general modification function; which can insert, update or delete
-- a value of the key, while collecting an effect in the form of an arbitrary
-- 'Control.Functor'.
alterF :: (Keyed k, Control.Functor f) => (Maybe v -> f (Ur (Maybe v))) -> k -> HashMap k v %1 -> f (HashMap k v)
alterF f key hm =
  idealIndexForKey key hm & \(Ur idx, hm') ->
    probeFrom key 0 idx hm' `chainU` \case
      -- The key does not exist, and there is an empty cell to insert.
      (# HashMap count cap arr, IndexToInsert psl ix #) ->
        f Nothing Control.<&> \case
          -- We don't need to insert anything.
          Ur Nothing -> HashMap count cap arr
          -- We need to insert a new key.
          Ur (Just v) ->
            HashMap
              (count + 1)
              cap
              (Array.unsafeWrite arr ix (Just (RobinVal psl key v)))
              & growMapIfNecessary
      -- The key exists.
      (# HashMap count cap arr, IndexToUpdate v psl ix #) ->
        f (Just v) Control.<&> \case
          -- We need to delete it.
          Ur Nothing ->
            Array.unsafeWrite arr ix Nothing & \arr' ->
              shiftSegmentBackward 1 cap arr' ((ix + 1) `mod` cap) & \arr'' ->
                HashMap
                  (count - 1)
                  cap
                  arr''
          -- We need to update it.
          Ur (Just new) ->
            HashMap
              count
              cap
              (Array.unsafeWrite arr ix (Just (RobinVal psl key new)))
      -- The key does not exist, but there is a key to evict.
      (# HashMap count cap arr, IndexToSwap evicted psl ix #) ->
        f Nothing Control.<&> \case
          -- We don't need to insert anything.
          Ur Nothing -> HashMap count cap arr
          -- We need to insert a new key.
          Ur (Just v) ->
            tryInsertAtIndex
              ( HashMap
                  count
                  cap
                  (Array.unsafeWrite arr ix (Just (RobinVal psl key v)))
              )
              ((ix + 1) `mod` cap)
              (incRobinValPSL evicted)
              & growMapIfNecessary
{-# INLINE alterF #-}

-- aspiwack: I'm implementing `alter` in terms of `alterF`, because, at this
-- point, we may have some bug fixes and so on and so forth. And maintaining two
-- functions this size is quite a bit unpleasant. Nevertheless, the extra boxing
-- required by the intermediate `Ur` call, there, makes it so that the
-- specialisation of `alterF` to `Identity` doesn't quite yield the code that we
-- would like, it's a bit costlier than it should. So in an ideal word, we would
-- implement both manually. In the future probably.

-- | A general modification function; which can insert, update or delete
-- a value of the key. See 'alterF', for an even more general function.
alter :: Keyed k => (Maybe v -> Maybe v) -> k -> HashMap k v %1 -> HashMap k v
alter f key hm = runIdentity $ alterF (\v -> Identity (Ur (f v))) key hm
  where
    runIdentity :: Identity a %1 -> a
    runIdentity (Identity x) = x
{-# INLINE alter #-}

-- | Insert a key value pair to a 'HashMap'. It overwrites the previous
-- value if it exists.
insert :: Keyed k => k -> v -> HashMap k v %1 -> HashMap k v
insert k v = alter (\_ -> Just v) k

-- | Delete a key from a 'HashMap'. Does nothing if the key does not
-- exist.
delete :: Keyed k => k -> HashMap k v %1 -> HashMap k v
delete = alter (\_ -> Nothing)

-- | 'insert' (in the provided order) the given key-value pairs to
-- the hashmap.
insertAll :: Keyed k => [(k, v)] -> HashMap k v %1 -> HashMap k v
insertAll [] hmap = hmap
insertAll ((k, v) : xs) hmap = insertAll xs (insert k v hmap)

-- TODO: Do a resize first on the length of the input.

-- | A version of 'fmap' which can throw out the elements.
--
-- Complexity: O(capacity hm)
mapMaybe :: Keyed k => (v -> Maybe v') -> HashMap k v %1 -> HashMap k v'
mapMaybe f = mapMaybeWithKey (\_k v -> f v)

-- | Same as 'mapMaybe', but also has access to the keys.
mapMaybeWithKey ::
  forall k v v'.
  Keyed k =>
  (k -> v -> Maybe v') ->
  HashMap k v %1 ->
  HashMap k v'
mapMaybeWithKey _ (HashMap 0 cap arr) = HashMap 0 cap (Unsafe.coerce arr)
mapMaybeWithKey f (HashMap _ cap arr) =
  Array.size arr & \(Ur size, arr1) ->
    mapAndPushBack 0 (size - 1) (False, 0) 0 arr1 & \(Ur c, arr2) ->
      HashMap c cap (Unsafe.coerce arr2)
  where
    f' :: k -> v -> Maybe v
    f' k v = unsafeCoerce (f k v)

    -- Going from arr[0] to arr[size-1] map each element while
    -- simultaneously pushing elements back if some earlier element(s)
    -- were deleted in a contiguous segment and if the current
    -- element has PSL > 0. Maintain a counter of how
    -- far to push elements back. At arr[size-1] if needed, call
    -- shiftSegmentBackward with the counter at arr[0].
    mapAndPushBack ::
      Int -> -- Current index
      Int -> -- Last index of array which is (size-1)
      (Bool, Int) -> -- (b,n) s.t. b iff open space n cells before current cell
      Int -> -- Count of present key-value pairs
      RobinArr k v %1 ->
      (Ur Int, RobinArr k v) -- The new count and fully mapped array
    mapAndPushBack ix end (shift, dec) count arr
      | (ix > end) =
          if shift
            then (Ur count, shiftSegmentBackward dec (end + 1) arr 0)
            else (Ur count, arr)
      | otherwise =
          Array.unsafeRead arr ix & \case
            (Ur Nothing, arr1) ->
              mapAndPushBack (ix + 1) end (False, 0) count arr1
            (Ur (Just (RobinVal (PSL p) k v)), arr1) -> case f' k v of
              Nothing ->
                Array.unsafeWrite arr1 ix Nothing
                  & \arr2 -> mapAndPushBack (ix + 1) end (True, dec + 1) count arr2
              Just v' -> case shift of
                False ->
                  Array.unsafeWrite arr1 ix (Just (RobinVal (PSL p) k v'))
                    & \arr2 -> mapAndPushBack (ix + 1) end (False, 0) (count + 1) arr2
                True -> case dec <= p of
                  False ->
                    Array.unsafeWrite arr1 (ix - p) (Just (RobinVal 0 k v'))
                      & \arr2 -> case p == 0 of
                        False ->
                          Array.unsafeWrite arr2 ix Nothing
                            & \arr3 -> mapAndPushBack (ix + 1) end (True, p) (count + 1) arr3
                        True -> mapAndPushBack (ix + 1) end (False, 0) (count + 1) arr2
                  True ->
                    Array.unsafeWrite arr1 (ix - dec) (Just (RobinVal (PSL (p - dec)) k v'))
                      & \arr2 ->
                        Array.unsafeWrite arr2 ix Nothing
                          & \arr3 -> mapAndPushBack (ix + 1) end (True, dec) (count + 1) arr3

-- | Complexity: O(capacity hm)
filterWithKey :: Keyed k => (k -> v -> Bool) -> HashMap k v %1 -> HashMap k v
filterWithKey f =
  mapMaybeWithKey
    (\k v -> if f k v then Just v else Nothing)

-- | Complexity: O(capacity hm)
filter :: Keyed k => (v -> Bool) -> HashMap k v %1 -> HashMap k v
filter f = filterWithKey (\_k v -> f v)

-- | Union of two maps using the provided function on conflicts.
--
-- Complexity: O(min(capacity hm1, capacity hm2)
unionWith ::
  Keyed k =>
  (v -> v -> v) ->
  HashMap k v %1 ->
  HashMap k v %1 ->
  HashMap k v
unionWith onConflict (hm1 :: HashMap k v) hm2 =
  -- To insert the elements in smaller map to the larger map, we
  -- compare their capacities, and flip the arguments if necessary.
  capacity hm1 & \(Ur cap1, hm1') ->
    capacity hm2 & \(Ur cap2, hm2') ->
      if cap1 > cap2
        then go onConflict hm1' (toList hm2')
        else go (\v2 v1 -> onConflict v1 v2) hm2' (toList hm1')
  where
    go ::
      (v -> v -> v) ->
      HashMap k v %1 -> -- larger map
      Ur [(k, v)] %1 -> -- contents of the smaller map
      HashMap k v
    go _ hm (Ur []) = hm
    go f hm (Ur ((k, vr) : xs)) =
      alter
        ( \case
            Nothing -> Just vr
            Just vl -> Just (f vl vr)
        )
        k
        hm
        & \hm -> go f hm (Ur xs)

-- | A right-biased union.
--
-- Complexity: O(min(capacity hm1, capacity hm2)
union :: Keyed k => HashMap k v %1 -> HashMap k v %1 -> HashMap k v
union hm1 hm2 = unionWith (\_v1 v2 -> v2) hm1 hm2

-- | Intersection of two maps with the provided combine function.
--
-- Complexity: O(min(capacity hm1, capacity hm2)
intersectionWith ::
  Keyed k =>
  (a -> b -> c) ->
  HashMap k a %1 ->
  HashMap k b %1 ->
  HashMap k c
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
    go ::
      (a -> b -> c) ->
      HashMap k a %1 ->
      Ur [(k, b)] %1 ->
      HashMap k c %1 ->
      HashMap k c
    go _ hm (Ur []) acc = hm `lseq` acc
    go f hm (Ur ((k, b) : xs)) acc =
      lookup k hm & \case
        (Ur Nothing, hm') -> go f hm' (Ur xs) acc
        (Ur (Just a), hm') -> go f hm' (Ur xs) (insert k (f a b) acc)

-- |
-- Reduce the 'HashMap' 'capacity' to decrease wasted memory. Returns
-- a semantically identical 'HashMap'.
--
-- This is only useful after a lot of deletes.
--
-- Complexity: O(capacity hm)
shrinkToFit :: Keyed k => HashMap k a %1 -> HashMap k a
shrinkToFit hm =
  size hm & \(Ur size, hm') ->
    let targetSize =
          ceiling
            (Prelude.max 1 (fromIntegral size Prelude./ constMaxLoadFactor))
     in resize targetSize hm'

-- # Querying
--------------------------------------------------

-- | Number of key-value pairs inside the 'HashMap'
size :: HashMap k v %1 -> (Ur Int, HashMap k v)
size (HashMap ct cap arr) = (Ur ct, HashMap ct cap arr)

-- | Maximum number of elements the HashMap can store without
-- resizing. However, for performance reasons, the 'HashMap' might be
-- before full.
--
-- Use 'shrinkToFit' to reduce the wasted space.
capacity :: HashMap k v %1 -> (Ur Int, HashMap k v)
capacity (HashMap ct cap arr) = (Ur cap, HashMap ct cap arr)

-- | Look up a value from a 'HashMap'.
lookup :: Keyed k => k -> HashMap k v %1 -> (Ur (Maybe v), HashMap k v)
lookup k hm =
  idealIndexForKey k hm & \(Ur idx, hm') ->
    probeFrom k 0 idx hm' `chainU` \case
      (# h, IndexToUpdate v _ _ #) ->
        (Ur (Just v), h)
      (# h, IndexToInsert _ _ #) ->
        (Ur Nothing, h)
      (# h, IndexToSwap _ _ _ #) ->
        (Ur Nothing, h)

-- | Check if the given key exists.
member :: Keyed k => k -> HashMap k v %1 -> (Ur Bool, HashMap k v)
member k hm =
  lookup k hm & \case
    (Ur Nothing, hm') -> (Ur False, hm')
    (Ur (Just _), hm') -> (Ur True, hm')

-- | Converts a HashMap to a lazy list.
toList :: HashMap k v %1 -> Ur [(k, v)]
toList (HashMap _ _ arr) =
  Array.toList arr & \(Ur elems) ->
    elems
      NonLinear.& NonLinear.catMaybes
      NonLinear.& Prelude.map (\(RobinVal _ k v) -> (k, v))
      NonLinear.& Ur

-- # Instances
--------------------------------------------------

instance Consumable (HashMap k v) where
  consume :: HashMap k v %1 -> ()
  consume (HashMap _ _ arr) = consume arr

instance Dupable (HashMap k v) where
  dup2 (HashMap i c arr) =
    dup2 arr & \(a1, a2) ->
      (HashMap i c a1, HashMap i c a2)

instance Data.Functor (HashMap k) where
  fmap f (HashMap s c arr) =
    HashMap s c $
      Data.fmap
        ( \case
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

_debugShow :: (Show k, Show v) => HashMap k v %1 -> String
_debugShow (HashMap _ _ robinArr) =
  Array.toList robinArr & \(Ur xs) -> show xs

idealIndexForKey ::
  Keyed k =>
  k ->
  HashMap k v %1 ->
  (Ur Int, HashMap k v)
idealIndexForKey k (HashMap sz cap arr) =
  (Ur (mod (hash k) cap), HashMap sz cap arr)

-- | Given a key, psl of the probe so far, current unread index, and
-- a full hashmap, return a probe result: the place the key already
-- exists, a place to swap from, or an unfilled cell to write over.
probeFrom ::
  Keyed k =>
  k ->
  PSL ->
  Int ->
  HashMap k v %1 ->
  (# HashMap k v, ProbeResult k v #)
probeFrom k p ix (HashMap ct cap arr) =
  Array.unsafeRead arr ix `chainU'` \case
    (Ur Nothing, arr') ->
      (# HashMap ct cap arr', IndexToInsert p ix #)
    (Ur (Just robinVal'@(RobinVal psl k' v')), arr') ->
      case k Prelude.== k' of
        -- Note: in the True case, we must have p == psl
        True -> (# HashMap ct cap arr', IndexToUpdate v' psl ix #)
        False -> case psl Prelude.< p of
          True -> (# HashMap ct cap arr', IndexToSwap robinVal' p ix #)
          False ->
            probeFrom k (p + 1) ((ix + 1) `mod` cap) (HashMap ct cap arr')

-- | Try to insert at a given index with a given PSL. So the
-- probing starts from the given index (with the given PSL).
tryInsertAtIndex ::
  Keyed k =>
  HashMap k v %1 ->
  Int ->
  RobinVal k v ->
  HashMap k v
tryInsertAtIndex hmap ix (RobinVal psl key val) =
  probeFrom key psl ix hmap `chainU` \case
    (# HashMap ct cap arr, IndexToUpdate _ psl' ix' #) ->
      Array.unsafeWrite arr ix' (Just $ RobinVal psl' key val)
        & HashMap ct cap
    (# HashMap ct cap arr, IndexToInsert psl' ix' #) ->
      Array.unsafeWrite arr ix' (Just $ RobinVal psl' key val)
        & HashMap (ct + 1) cap
    (# HashMap ct cap arr, IndexToSwap oldVal psl' ix' #) ->
      Array.unsafeWrite arr ix' (Just $ RobinVal psl' key val)
        & HashMap ct cap
        & \hm -> tryInsertAtIndex hm ((ix' + 1) `mod` cap) (incRobinValPSL oldVal)

-- | Shift all cells with PSLs > 0 in a continuous segment
-- following the deleted cell, backwards by one and decrement
-- their PSLs.
shiftSegmentBackward ::
  Keyed k =>
  Int ->
  Int ->
  RobinArr k v %1 ->
  Int ->
  RobinArr k v
shiftSegmentBackward dec s arr ix =
  Array.unsafeRead arr ix & \case
    (Ur Nothing, arr') -> arr'
    (Ur (Just (RobinVal 0 _ _)), arr') -> arr'
    (Ur (Just val), arr') ->
      Array.unsafeWrite arr' ix Nothing & \arr'' ->
        shiftSegmentBackward
          dec
          s
          (Array.unsafeWrite arr'' ((ix - dec + s) `mod` s) (Just $ decRobinValPSL val))
          ((ix + 1) `mod` s)

-- TODO: This does twice as much writes than necessary, it first empties
-- the cell, just to update it again at the next call. We can save some
-- writes by only emptying the last cell.

-- | Makes sure that the map is not exceeding its utilization threshold
-- (constMaxUtilization), resizes (constGrowthFactor) if necessary.
growMapIfNecessary :: Keyed k => HashMap k v %1 -> HashMap k v
growMapIfNecessary (HashMap sz cap arr) =
  let load = fromIntegral sz / fromIntegral cap
   in if load Prelude.< constMaxLoadFactor
        then HashMap sz cap arr
        else
          let newCap = max 1 (cap * constGrowthFactor)
           in resize newCap (HashMap sz cap arr)

-- | Resizes the HashMap to given capacity.
--
-- Invariant: Given capacity should be greater than the size, this is not
-- checked.
resize :: Keyed k => Int -> HashMap k v %1 -> HashMap k v
resize targetSize (HashMap _ _ arr) =
  Array.allocBeside targetSize Nothing arr & \(newArr, oldArr) ->
    Array.toList oldArr & \(Ur elems) ->
      let xs =
            elems
              NonLinear.& NonLinear.catMaybes
              NonLinear.& Prelude.map (\(RobinVal _ k v) -> (k, v))
       in insertAll xs (HashMap 0 targetSize newArr)

-- TODO: 'insertAll' keeps checking capacity on each insert. We should
-- replace it with a faster unsafe variant.

-- TODO: Remove the below workarounds once we are on GHC 9.2.
--
-- We have to use these functions below because:
--

-- * GHC <9.2 does not allow linear `case` statements.

-- * LambdaCase workaround does not work, because (&) does not work with

--   unlifted types.
chainU :: (# a, b #) %1 -> ((# a, b #) %1 -> c) %1 -> c
chainU x f = f x

chainU' :: a %1 -> (a %1 -> (# b, c #)) %1 -> (# b, c #)
chainU' x f = f x
