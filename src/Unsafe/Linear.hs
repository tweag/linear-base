{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

-- | Unsafe coercions for linearly typed code.
--
-- This module is intended to be imported qualified. /All/ functions in this
-- module are unsafe.

module Unsafe.Linear where

import qualified Unsafe.Coerce as NonLinear
import GHC.Exts (TYPE, RuntimeRep)

-- * Linearly typed @unsafeCoerce@, and useful variant to deal with linear types

-- | Linearly typed @unsafeCoerce@
coerce :: a ->. b
coerce = NonLinear.unsafeCoerce NonLinear.unsafeCoerce

-- | Converts an unrestricted function into a linear function
toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2).
     (a -> b) ->. (a ->. b)
toLinear = coerce

-- | Like 'toLinear' but for two-argument functions
toLinear2
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3).
     (a -> b -> c) ->. (a ->. b ->. c)
toLinear2 = coerce
