{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Unrestricted.Internal.Dupable
  ( -- * Dupable
    Dupable (..),
    dup,
    dup3,
    dup4,
    dup5,
    dup6,
    dup7,
  )
where

import Data.Replicator.Linear.Internal (Replicator (..))
import qualified Data.Replicator.Linear.Internal as Replicator
import Data.Replicator.Linear.Internal.ReplicationStream (ReplicationStream (..))
import Data.Unrestricted.Internal.Consumable
import Prelude.Linear.Internal

-- | The laws of @Dupable@ are dual to those of 'Monoid':
--
-- * @first consume (dup2 a) ≃ a ≃ second consume (dup2 a)@ (neutrality)
-- * @first dup2 (dup2 a) ≃ (second dup2 (dup2 a))@ (associativity)
--
-- Where the @(≃)@ sign represents equality up to type isomorphism.
--
-- Implementation of @Dupable@ for @Movable@ types should be done with
-- @AsMovable@ and @DerivingVia@, whereas implementation for non-@Movable@
-- types should be done using either @dupR@ (for data types composed of
-- @Dupable@ members) or @dup2@.
class Consumable a => Dupable a where
  {-# MINIMAL dupR | dup2 #-}

  -- | Creates a @Replicator@ for the given @a@. This @Replicator@ type has
  -- a @Data.Applicative@ instance, which allows for an easy composition.
  dupR :: a %1 -> Replicator a
  dupR x = Streamed $ ReplicationStream x id dup2 consume

  -- | Creates two copies of @a@ from a @Dupable a@, in a linear fashion.
  dup2 :: a %1 -> (a, a)
  dup2 x = Replicator.elim (,) (dupR x)

-- | Creates 3 copies of @a@ from a @Dupable a@, in a linear fashion.
dup3 :: Dupable a => a %1 -> (a, a, a)
dup3 x = Replicator.elim (,,) (dupR x)

-- | Creates 4 copies of @a@ from a @Dupable a@, in a linear fashion.
dup4 :: Dupable a => a %1 -> (a, a, a, a)
dup4 x = Replicator.elim (,,,) (dupR x)

-- | Creates 5 copies of @a@ from a @Dupable a@, in a linear fashion.
dup5 :: Dupable a => a %1 -> (a, a, a, a, a)
dup5 x = Replicator.elim (,,,,) (dupR x)

-- | Creates 6 copies of @a@ from a @Dupable a@, in a linear fashion.
dup6 :: Dupable a => a %1 -> (a, a, a, a, a, a)
dup6 x = Replicator.elim (,,,,,) (dupR x)

-- | Creates 7 copies of @a@ from a @Dupable a@, in a linear fashion.
dup7 :: Dupable a => a %1 -> (a, a, a, a, a, a, a)
dup7 x = Replicator.elim (,,,,,,) (dupR x)

-- | Creates two copies of @a@ from a @Dupable a@. Same function as @dup2@.
dup :: Dupable a => a %1 -> (a, a)
dup = dup2
