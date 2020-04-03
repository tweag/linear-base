{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LinearTypes #-}

-- |
--   A mutable hashmap with a linear interface.
--   Internally, this uses robin hood hashing on mutable,
--   linear vectors.
--
-- Internally, the array stores (k,v,Int)s and if that slot is 
-- empty, the Int is a -1.
--
-- TODO:
-- - Add singletonWithSize
--

module Data.HashMap.Linear
  (
  )
where



import Data.Hashable
import Data.Unrestricted.Linear
import Data.Array.Mutable.Linear
import Prelude.Linear hiding (read)
import Prelude ()
import qualified Unsafe.Linear as Unsafe


-- # Core Data Types
--------------------------------------------------

-- | Robin values are triples of the key, value and PSL
-- (the probe sequence length).
type RobinVal k v = (k,v,Int)

data HashMap k v where
  -- | HashMap (size,count) array-of-robin-values
  -- The size is the amount of memory the array takes up.
  -- Note our spacing invariant: count*3 <= size.
  HashMap :: Hashable k => (Int, Int) -> Array (RobinVal k v) #-> HashMap k v

-- # Construction and Modification
--------------------------------------------------

-- | TODO
-- TODO: internally, everything but the first element has PSL -1
singleton :: Hashable k =>
  (k,v) -> (HashMap k v #-> Unrestricted b) -> Unrestricted b
singleton (k,v) = undefined

alter ::  Hashable k => (Maybe a -> Maybe a) -> k -> HashMap k v #-> HashMap k v
alter = undefined

insert :: Hashable k => HashMap k v #-> k -> v -> HashMap k v
insert hmap k v = alter (\_ -> Just v) k hmap

-- | If present, deletes key-value pair, otherwise does nothing
delete :: Hashable k => HashMap k v #-> k -> HashMap k v
delete hmap k = alter (\_ -> Nothing) k hmap


-- # Querying
--------------------------------------------------

size :: HashMap k v #-> (HashMap k v, Int)
size (HashMap (size,count) arr) = (HashMap (size,count) arr, count)

member :: Hashable k => HashMap k v #-> k -> (HashMap k v, Bool)
member hmap k = memberFromIndex $ findIndex hmap k
  where
    memberFromIndex :: Hashable k =>
      (HashMap k v, Int) #-> (HashMap k v, Bool)
    memberFromIndex (HashMap sizes arr, ix) =
      (Unsafe.toLinear checkPSL) (read arr ix) sizes

    -- XXX: make linear
    checkPSL :: Hashable k =>
      (RobinArr k v, RobinVal k v) -> (Int, Int) -> (HashMap k v, Bool)
    checkPSL (arr', (k',v',psl')) sizes = (HashMap sizes arr', psl' /= (-1))


lookup :: Hashable k => HashMap k v #-> k -> (HashMap k v, Maybe v)
lookup hmap k = (Unsafe.toLinear lookupFromIx) $ findIndex hmap k
  where
    -- XXX: make linear
    lookupFromIx :: (HashMap k v, Int) -> (HashMap k v, Maybe v)
    lookupFromIx (HashMap sizes arr, ix) = 
      case read arr ix of
        (arr', (k',v',psl')) | psl' >= 0 -> (HashMap sizes arr', Just v')
        (arr',_) -> (HashMap sizes arr', Nothing)


-- # Internal Library
--------------------------------------------------

type RobinArr k v = Array (RobinVal k v)

-- | Find the index a key ought to hash into,
findIndex :: Hashable k => HashMap k v #-> k -> (HashMap k v, Int)
findIndex (HashMap (size, count) arr) k =
  wrapHashMap $ (Unsafe.toLinear findFrom) arr (k,0) (mod (hash k) size)
  where
    wrapHashMap :: Hashable k => (RobinArr k v, Int) #-> (HashMap k v, Int)
    wrapHashMap (arr, ix) = (HashMap (size, count) arr, ix)

    -- | (findFromExpected key key-psl array) gives the index at which
    -- the value of that key should be found or inserted.
    -- Internal precondition: count*3 <= size.
    -- XXX: make this linear
    findFrom :: Hashable k => RobinArr k v -> (k, Int) -> Int -> (RobinArr k v, Int)
    findFrom arr (k,psl) ix = case read arr ix of
      (arr', (_,_,-1)) -> (arr', ix)
      (arr',(_,_,psl')) | psl > psl' -> (arr', ix)
      (arr', _) -> findFrom arr' (k,psl+1) (mod (ix+1) size)

