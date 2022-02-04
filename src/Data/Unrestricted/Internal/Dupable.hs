{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Unrestricted.Internal.Dupable
  ( Dupable (..),
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

-- | The laws of 'Dupable' are dual to those of 'Monoid':
--
-- * 1. @first consume (dup2 a) ≃ a ≃ second consume (dup2 a)@ ('dup2' neutrality)
-- * 2. @first dup2 (dup2 a) ≃ (second dup2 (dup2 a))@ ('dup2' associativity)
--
-- where the @(≃)@ sign represents equality up to type isomorphism.
--
-- * 3. @dup2 = Replicator.elim (,) . dupR@ (coherence between 'dup2' and 'dupR')
-- * 4. @consume = Replicator.elim () . dupR@ (coherence between 'consume' and 'dupR')
--
-- * 5. @Replicator.extract . dupR = id@ ('dupR' identity)
-- * 6. @dupR . dupR = (Replicator.map dupR) . dupR@ ('dupR' interchange)
--
-- (Laws 1-2 and 5-6 are equivalent)
--
-- Implementation of 'Dupable' for 'Data.Unrestricted.Movable' types should
-- be done with @deriving via 'Data.Unrestricted.AsMovable'@.
class Consumable a => Dupable a where
  {-# MINIMAL dupR | dup2 #-}

  -- | Creates a 'Replicator' for the given @a@.
  --
  -- You usually want to define this method using 'Replicator'\'s
  -- 'Data.Functor.Linear.Applicative' instance. For instance, here is an
  -- implementation of @'Dupable' [a]@:
  --
  -- > instance Dupable a => Dupable [a] where
  -- >   dupR [] = pure []
  -- >   dupR (a : as) = (:) <$> dupR a <*> dupR as
  dupR :: a %1 -> Replicator a
  dupR x = Streamed $ ReplicationStream x id dup2 consume

  -- | Creates two @a@s from a @'Dupable' a@, in a linear fashion.
  dup2 :: a %1 -> (a, a)
  dup2 x = Replicator.elim (,) (dupR x)

-- | Creates 3 @a@s from a @'Dupable' a@, in a linear fashion.
dup3 :: Dupable a => a %1 -> (a, a, a)
dup3 x = Replicator.elim (,,) (dupR x)

-- | Creates 4 @a@s from a @'Dupable' a@, in a linear fashion.
dup4 :: Dupable a => a %1 -> (a, a, a, a)
dup4 x = Replicator.elim (,,,) (dupR x)

-- | Creates 5 @a@s from a @'Dupable' a@, in a linear fashion.
dup5 :: Dupable a => a %1 -> (a, a, a, a, a)
dup5 x = Replicator.elim (,,,,) (dupR x)

-- | Creates 6 @a@s from a @'Dupable' a@, in a linear fashion.
dup6 :: Dupable a => a %1 -> (a, a, a, a, a, a)
dup6 x = Replicator.elim (,,,,,) (dupR x)

-- | Creates 7 @a@s from a @'Dupable' a@, in a linear fashion.
dup7 :: Dupable a => a %1 -> (a, a, a, a, a, a, a)
dup7 x = Replicator.elim (,,,,,,) (dupR x)

-- | Creates two @a@s from a @'Dupable' a@. Same function as 'dup2'.
dup :: Dupable a => a %1 -> (a, a)
dup = dup2
