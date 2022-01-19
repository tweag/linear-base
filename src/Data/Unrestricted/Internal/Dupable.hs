{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

import Data.Unrestricted.Internal.Consumable
import Data.Unrestricted.Internal.ReplicationStream (ReplicationStream (..))
import Data.Unrestricted.Internal.Replicator (Replicator (..))
import qualified Data.Unrestricted.Internal.Replicator as Replicator
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
  dupR x = Streamed $ ReplicationStream id dup2 consume x

  dup2 :: a %1 -> (a, a)
  dup2 x = Replicator.elim @2 (,) (dupR x)

dup3 :: Dupable a => a %1 -> (a, a, a)
dup3 x = Replicator.elim @3 (,,) (dupR x)

dup4 :: Dupable a => a %1 -> (a, a, a, a)
dup4 x = Replicator.elim @4 (,,,) (dupR x)

dup5 :: Dupable a => a %1 -> (a, a, a, a, a)
dup5 x = Replicator.elim @5 (,,,,) (dupR x)

dup6 :: Dupable a => a %1 -> (a, a, a, a, a, a)
dup6 x = Replicator.elim @6 (,,,,,) (dupR x)

dup7 :: Dupable a => a %1 -> (a, a, a, a, a, a, a)
dup7 x = Replicator.elim @7 (,,,,,,) (dupR x)

dup :: Dupable a => a %1 -> (a, a)
dup = dup2
