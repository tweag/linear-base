{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  ( -- * Unsafe Coersions
    coerce,
    toLinear,
    toLinear2,
    toLinear3,
  )
  where

import Unsafe.Coerce (UnsafeEquality (..), unsafeEqualityProof)
import GHC.Exts (TYPE, RuntimeRep)
import GHC.Types (Multiplicity (..))

-- | Linearly typed @unsafeCoerce@
coerce :: forall a b. a %1-> b
coerce a =
  case unsafeEqualityProof @a @b of
    UnsafeRefl -> a
{-# INLINE coerce #-}

-- | Converts an unrestricted function into a linear function
toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) p.
     (a %p-> b) %1-> (a %1-> b)
toLinear f = case unsafeEqualityProof @p @'One of
  UnsafeRefl -> f

-- | Like 'toLinear' but for two-argument functions
toLinear2
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3) p q.
     (a %p-> b %q-> c) %1-> (a %1-> b %1-> c)
toLinear2 f = case unsafeEqualityProof @'(p,q) @'( 'One, 'One) of
  UnsafeRefl -> f

-- | Like 'toLinear' but for three-argument functions
toLinear3
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (r3 :: RuntimeRep) (r4 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3) (d :: TYPE r4) p q r.
     (a %p-> b %q-> c %r-> d) %1-> (a %1-> b %1-> c %1-> d)
toLinear3 f = case unsafeEqualityProof @'(p,q,r) @'( 'One, 'One, 'One) of
  UnsafeRefl -> f
