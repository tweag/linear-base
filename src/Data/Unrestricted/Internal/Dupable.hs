{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
module Data.Unrestricted.Internal.Dupable
  (
  -- * Dupable
    Dupable(..)
  , dup
  , dup3
  ) where

import Data.Unrestricted.Internal.Consumable
import GHC.TypeLits
import Data.Type.Equality
import Data.V.Linear (V)
import qualified Data.V.Linear as V

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
  {-# MINIMAL dupV | dup2 #-}

  dupV :: forall n. KnownNat n => a %1-> V n a
  dupV a =
    case V.caseNat @n of
      Prelude.Left Refl -> a `lseq` V.make @0 @a
      Prelude.Right Refl -> V.iterate dup2 a

  dup2 :: a %1-> (a, a)
  dup2 a = V.elim (dupV @a @2 a) (,)

dup3 :: Dupable a => a %1-> (a, a, a)
dup3 x = V.elim (dupV @_ @3 x) (,,)

dup :: Dupable a => a %1-> (a, a)
dup = dup2

