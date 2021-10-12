{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
  )
  where

import Data.Type.Equality (type (~~))
import Data.Kind (Constraint)
import Unsafe.Coerce (UnsafeEquality (..), unsafeEqualityProof)
import GHC.Exts (TYPE, RuntimeRep (..))
import GHC.TypeNats
import Unsafe.Linear.Internal
import Unsafe.Linear.Internal.Generically

-- | Linearly typed @unsafeCoerce@
coerce :: forall a b. a %1-> b
coerce a =
  case unsafeEqualityProof @a @b of
    UnsafeRefl -> a
{-# INLINE coerce #-}

-- | Converts an unrestricted function into a linear function
toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) p x.
     (a %p-> b) %1-> (a %x-> b)
toLinear f = case unsafeEqualityProof @p @x of
  UnsafeRefl -> f

-- | Like 'toLinear' but for two-argument functions
toLinear2
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3) p q x y.
     (a %p-> b %q-> c) %1-> (a %x-> b %y-> c)
toLinear2 f = case unsafeEqualityProof @'(p,q) @'(x, y) of
  UnsafeRefl -> f

-- | Like 'toLinear' but for three-argument functions
toLinear3
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (r3 :: RuntimeRep) (r4 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) (c :: TYPE r3) (d :: TYPE r4) p q r x y z.
     (a %p-> b %q-> c %r-> d) %1-> (a %x-> b %y-> c %z-> d)
toLinear3 f = case unsafeEqualityProof @'(p,q,r) @'(x,y,z) of
  UnsafeRefl -> f

-- | @toLinearN@ subsumes the functionality of 'toLinear1', 'toLinear2', and
-- 'toLinear3'. In particular, @toLinearN \@n@ unsafely changes the
-- multiplicities of the first @n@ arrows from any multiplicity to any
-- other multiplicity. To be explicit about how each multiplicity is
-- being changed, you can use additional type arguments.
--
-- === Examples
--
-- @
-- toLinearN \@2 :: (a %m-> b %n-> Int) %1-> a %x-> b %y-> Int
-- toLinearN \@3 \@(_ %m-> _ -> _ %1-> _) \@(_ %1-> _ %1-> _ %x-> _)
--   :: (a %m-> b -> c %1-> d) %1-> (a %1-> b %1-> c %x-> d)
-- 'toLinear3' = toLinearN \@3
-- @
toLinearN :: forall n f g. ToLinearN n f g => f %1-> g
-- See Note: Core size
toLinearN f = case unsafeLinearityProofN @n @f @g of
  UnsafeRefl -> f

-- | @ToLinearN n f g@ means that @f@ and @g@ are the same with the
-- possible exception of the multiplicities of the first @n@ arrows.
type ToLinearN :: forall {rep :: RuntimeRep}. Nat -> TYPE rep -> TYPE rep -> Constraint
class ToLinearN n f g where
  -- | Given that @f@ and @g@ are the same, with the possible exception of the
  -- multiplicities of the first @n@ arrows, @unsafeLinearityProofN \@n \@f \@g@
  -- is a fake proof that @f@ and @g@ are identical. This is used primarily in the
  -- definition of 'toLinearN', but it can also be used, for example, to coerce
  -- a container of functions:
  --
  -- @
  -- linearMany :: forall a b c. [a -> b -> c] %1-> [a %1-> b %1-> c]
  -- linearMany = castWithUnsafe (applyUnsafe (UnsafeRefl @[]) $
  --   unsafeLinearityProofN @2 @(a -> b -> c) @(a %1-> b %1-> c))
  --
  -- applyUnsafe :: UnsafeEquality f g -> UnsafeEquality x y -> UnsafeEquality (f x) (g y)
  -- applyUnsafe UnsafeRefl UnsafeRefl = UnsafeRefl
  --
  -- castWithUnsafe :: UnsafeEquality x y -> x %1-> y
  -- castWithUnsafe UnsafeRefl x = x
  -- @
  --
  -- The rather explicit handling of coercions seems to be necessary,
  -- unfortunately, presumably due to the way GHC eagerly rejects equality
  -- constraints it sees as definitely unsatisfiable.
  unsafeLinearityProofN :: UnsafeEquality f g

instance (ToLinearN' ni f g, ni ~ ToINat n) => ToLinearN n f g where
  unsafeLinearityProofN = prf @ni

-- | Plain old inductive natural numbers.
data INat = Z | S INat

-- | Convert a GHC 'Nat' to a real inductive natural number.
-- We use this because GHC 'Nat' offers a friendly API but
-- it's a terrible pain for implementation.
type ToINat :: Nat -> INat
type family ToINat n where
  ToINat 0 = 'Z
  ToINat n = 'S (ToINat (n - 1))

-- | The actual implementation of 'ToLinearN', using the inductive natural
-- number it's handed.
type ToLinearN' :: forall {rep :: RuntimeRep}. INat -> TYPE rep -> TYPE rep -> Constraint
class ToLinearN' arrs f g where
  prf :: UnsafeEquality f g

instance a ~ b => ToLinearN' 'Z (a :: TYPE rep) (b :: TYPE rep) where
  prf = UnsafeRefl

-- We use heterogeneous equality here to shift @rep ~ 'LiftedRep@ to the left
-- side of the fat arrow, so that seeing @'S k@ lets GHC /infer/ that @x@ and
-- @y@ are lifted types, rather than needing that information to come from
-- elsewhere.
instance
  ( ToLinearN' k fb gb
  , x ~~ ((a :: TYPE repa) %p-> (fb :: TYPE repb))
  , y ~~ (a %q-> (gb :: TYPE repb))
  ) => ToLinearN' ('S k) (x :: TYPE rep) (y :: TYPE rep) where
  prf = case prf @k @fb @gb of
    UnsafeRefl -> case unsafeEqualityProof @p @q of
      UnsafeRefl -> UnsafeRefl

-- Note: Core size
--
-- In GHC 9.0, at least, using toLinearN to implement toLinear2 and toLinear3
-- produces more Core terms, though fewer types and coercions. Does this hamper
-- inlining? If not, we could actually use it so. Alternatively, we *might* be
-- able to reduce unsafeLinearityProofN to one case expression by using some
-- (quantified constraint?) tricks. But that will be complicated, if possible,
-- and probably not worth the trouble.
