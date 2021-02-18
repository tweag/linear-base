{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Unrestricted.Internal.Dupable
  (
  -- * Dupable
    Dupable(..)
  , dup
  , dup3
  , dup2default
  , GDupable(..)
  ) where

import Data.Unrestricted.Internal.Consumable
import GHC.Generics
import GHC.TypeLits
import Data.Type.Equality
import Data.V.Linear.Internal.V (V)
import qualified Data.V.Linear.Internal.V as V
import qualified Unsafe.Linear as Unsafe
import Prelude.Linear.Internal ((&))

-- | The laws of @Dupable@ are dual to those of 'Monoid':
--
-- * @first consume (dup2 a) ≃ a ≃ second consume (dup2 a)@ (neutrality)
-- * @first dup2 (dup2 a) ≃ (second dup2 (dup2 a))@ (associativity)
--
-- Where the @(≃)@ sign represents equality up to type isomorphism.
--
-- When implementing 'Dupable' instances for composite types, using 'dupV'
-- should be more convenient since 'V' has a zipping 'Applicative' instance.
class Consumable a => Dupable a where
  dupV :: forall n. KnownNat n => a %1-> V n a
  dupV a =
    case V.caseNat @n of
      Prelude.Left Refl -> a `lseq` V.make @0 @a
      Prelude.Right Refl -> V.iterate dup2 a

  dup2 :: a %1-> (a, a)
  default dup2 :: (Generic a, GDupable (Rep a)) => a %1-> (a, a)
  dup2 a = gdup2 (Unsafe.toLinear from a) &
    \case (x, y) -> (Unsafe.toLinear to x, Unsafe.toLinear to y)

dup3 :: Dupable a => a %1-> (a, a, a)
dup3 x = V.elim (dupV @_ @3 x) (,,)

dup :: Dupable a => a %1-> (a, a)
dup = dup2

dup2default :: forall a. Dupable a => a %1-> (a, a)
dup2default a = V.elim (dupV @a @2 a) (,)

class GDupable f where
  gdup2 :: f x %1-> (f x, f x)
