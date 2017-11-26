{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

-- | This module declares unsafe coercions for linearly typed code.
--
-- This functions in this module are intended to be qualified
--
-- @
--      import qualified Linear.Unsafe as Unsafe
-- @

module Unsafe.Linear where

import qualified Unsafe.Coerce as NonLinear
import GHC.Exts (TYPE, RuntimeRep)

-- * Linearly typed @unsafeCoerce@, and useful variant to deal with linear types

-- | Linearly typed @unsafeCoerce@
coerce :: a ->. b
coerce = NonLinear.unsafeCoerce NonLinear.unsafeCoerce

-- | Converts an unrestricted function into a linear function
castLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2).
     (a -> b) ->. (a ->. b)
castLinear = coerce

-- | Like 'castLinear' but for two-argument functions
castLinear2
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3).
     (a -> b -> c) ->. (a ->. b ->. c)
castLinear2 = coerce
