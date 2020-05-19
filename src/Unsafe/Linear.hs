{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE LinearTypes #-}

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
-- linear function @f :: A #-> B@, do not write a non-linear version and coerce
-- it.

module Unsafe.Linear
  ( -- * Unsafe Coersions
    coerce,
    toLinear,
    toLinear2,
    toLinear3,
  )
  where

import qualified Unsafe.Coerce as NonLinear
import GHC.Exts (TYPE, RuntimeRep)

-- | Linearly typed @unsafeCoerce@
coerce :: a #-> b
coerce = NonLinear.unsafeCoerce NonLinear.unsafeCoerce

-- | Converts an unrestricted function into a linear function
toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2).
     (a -> b) #-> (a #-> b)
toLinear = coerce

-- | Like 'toLinear' but for two-argument functions
toLinear2
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3).
     (a -> b -> c) #-> (a #-> b #-> c)
toLinear2 = coerce

-- | Like 'toLinear' but for three-argument functions
toLinear3 :: (a -> b -> c -> d) #-> (a #-> b #-> c #-> d)
toLinear3 = coerce
