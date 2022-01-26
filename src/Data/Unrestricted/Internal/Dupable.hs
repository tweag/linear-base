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
-- When implementing 'Dupable' instances for composite types, using 'dupR'
-- should be more convenient since 'Replicator' has an 'Applicative' instance.
class Consumable a => Dupable a where
  {-# MINIMAL dupR | dup2 #-}

  dupR :: a %1 -> Replicator a
  dupR x = Streamed $ ReplicationStream x id dup2 consume

  dup2 :: a %1 -> (a, a)
  dup2 x = Replicator.elim (,) (dupR x)

dup3 :: Dupable a => a %1 -> (a, a, a)
dup3 x = Replicator.elim (,,) (dupR x)

dup4 :: Dupable a => a %1 -> (a, a, a, a)
dup4 x = Replicator.elim (,,,) (dupR x)

dup5 :: Dupable a => a %1 -> (a, a, a, a, a)
dup5 x = Replicator.elim (,,,,) (dupR x)

dup6 :: Dupable a => a %1 -> (a, a, a, a, a, a)
dup6 x = Replicator.elim (,,,,,) (dupR x)

dup7 :: Dupable a => a %1 -> (a, a, a, a, a, a, a)
dup7 x = Replicator.elim (,,,,,,) (dupR x)

dup :: Dupable a => a %1 -> (a, a)
dup = dup2
