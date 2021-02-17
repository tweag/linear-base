{-# LANGUAGE NoImplicitPrelude #-}

-- | Mutable vectors with a linear API.
--
-- Vectors are arrays that grow automatically, that you can append to with
-- 'push'. They never shrink automatically to reduce unnecessary copying,
-- use 'shrinkToFit' to get rid of the wasted space.
--
-- To use mutable vectors, create a linear computation of type
-- @Vector a %1-> Ur b@ and feed it to 'constant' or 'fromList'.
--
-- == Example
--
-- >>> :set -XLinearTypes
-- >>> import Prelude.Linear
-- >>> import qualified Data.Vector.Mutable.Linear as Vector
-- >>> :{
--  isFirstZero :: Vector.Vector Int %1-> Ur Bool
--  isFirstZero vec =
--    Vector.get 0 vec
--      & \(Ur ret, vec) -> vec `lseq` Ur (ret == 0)
-- :}
--
-- >>> unur $ Vector.fromList [0..10] isFirstZero
-- True
-- >>> unur $ Vector.fromList [1,2,3] isFirstZero
-- False
module Data.Vector.Mutable.Linear
  ( -- * A mutable vector
    Vector,
    -- * Run a computation with a vector
    empty,
    constant,
    fromList,
    -- * Mutators
    set,
    unsafeSet,
    modify,
    modify_,
    push,
    pop,
    filter,
    mapMaybe,
    slice,
    shrinkToFit,
    -- * Accessors
    get,
    unsafeGet,
    size,
    capacity,
    toList,
    freeze,
    -- * Mutable-style interface
    read,
    unsafeRead,
    write,
    unsafeWrite
  )
where

import Data.Vector.Mutable.Linear.Internal
