{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE LinearTypes #-}

module Unsafe.Linear.Internal where

import qualified Unsafe.Coerce as NonLinear
import GHC.Exts (TYPE, RuntimeRep)

-- | Linearly typed @unsafeCoerce@
coerce :: a %1-> b
coerce = NonLinear.unsafeCoerce NonLinear.unsafeCoerce

-- | Converts an unrestricted function into a linear function
toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) p.
     (a %p-> b) %1-> (a %1-> b)
toLinear = coerce

-- | Like 'toLinear' but for two-argument functions
toLinear2
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3) p q.
     (a %p-> b %q-> c) %1-> (a %1-> b %1-> c)
toLinear2 = coerce

-- | Like 'toLinear' but for three-argument functions
toLinear3
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (r3 :: RuntimeRep) (r4 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3) (d :: TYPE r4) p q r.
     (a %p-> b %q-> c %r-> d) %1-> (a %1-> b %1-> c %1-> d)
toLinear3 = coerce
