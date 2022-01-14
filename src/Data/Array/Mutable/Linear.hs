{-# LANGUAGE LinearTypes #-}

-- |
-- This module provides a pure linear interface for arrays with in-place
-- mutation.
--
-- To use these mutable arrays, create a linear computation of type
-- @Array a %1-> Ur b@ and feed it to 'alloc' or 'fromList'.
--
-- == A Tiny Example
--
-- >>> :set -XLinearTypes
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude.Linear
-- >>> import qualified Data.Array.Mutable.Linear as Array
-- >>> :{
--  isFirstZero :: Array.Array Int %1-> Ur Bool
--  isFirstZero arr =
--    Array.get 0 arr
--      & \(Ur val, arr') -> arr' `lseq` Ur (val == 0)
-- :}
--
-- >>> unur $ Array.fromList [0..10] isFirstZero
-- True
-- >>> unur $ Array.fromList [1,2,3] isFirstZero
-- False
module Data.Array.Mutable.Linear
  ( -- * Mutable Linear Arrays
    Array,

    -- * Performing Computations with Arrays
    alloc,
    allocBeside,
    fromList,

    -- * Modifications
    set,
    unsafeSet,
    resize,
    map,

    -- * Accessors
    get,
    unsafeGet,
    size,
    slice,
    toList,
    freeze,

    -- * Mutable-style interface
    read,
    unsafeRead,
    write,
    unsafeWrite,
  )
where

import Data.Array.Mutable.Linear.Internal
import Prelude hiding (map, read)
