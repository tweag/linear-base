-- | Unsafe coercions for linearly typed code.
--
-- Use this module to coerce non-linear functions to be linear or values
-- bound linearly to be another type. /All/ functions in this module are
-- unsafe.
--
-- Hence:
--
-- * Import this module qualifed as Unsafe.
-- * Do not use this unless you have to. Specifically, if you can write a
-- linear function @f :: A %1-> B@, do not write a non-linear version and coerce
-- it.

module Unsafe.Linear
  ( -- * Unsafe Coercions
    coerce,
    toLinear,
    toLinear2,
    toLinear3,
    toLinearN,
    ToLinearN (..),
    -- * Generics
    Generically(..),
    unGenerically,
    to,
    from,
    Generically1(..),
    unGenerically1,
    to1,
    from1,
    genericTraverse
  )
  where

import Unsafe.Linear.Internal
import Unsafe.Linear.Internal.Generically
