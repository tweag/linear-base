{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}

-- |
--   A mutable hashmap with a linear interface.
--   Internally, this uses robin hood hashing on mutable,
--   linear vectors.
--

module Data.HashMap.Linear
  (
  )
where


{-

import Data.Hashable
import Data.Array.Mutable.Linear


-- # Core Data Types
--------------------------------------------------

-- | Robin values are triples of the key, value and PSL
-- (the probe sequence length).
type RobinVal k v = (k,v,Int)

data HashMap k v where
  -- | HashMap size array-of-robin-values
  HashMap :: Int -> Array (RobinVal k v) #-> HashMap k v


-- # Construction and Modification
--------------------------------------------------

singleton :: Hashable k =>
  (k,v) -> (HashMap k v #-> Unrestricted b) -> Unrestricted b
singleton = undefined

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
size = undefined

member :: Hashable k => HashMap k v #-> k -> (HashMap k v, Bool)
member = undefined

lookup :: Hashable k => HashMap k v #-> k -> (HashMap k v, Maybe v)
lookup = undefined


-- # Internal Library
--------------------------------------------------


-}




