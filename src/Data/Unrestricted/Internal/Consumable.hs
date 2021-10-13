{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Unrestricted.Internal.Consumable
  (
  -- * Consumable
    Consumable(..)
  , lseq
  , seqUnit

  -- * Generic deriving
  , GConsumable
  , genericConsume
  )
  where

import Prelude.Linear.Generically
import Prelude.Linear.Internal
import Generics.Linear
import GHC.Types (Multiplicity (..))

class Consumable a where
  consume :: a %1-> ()

-- | Consume the unit and return the second argument.
-- This is like 'seq' but since the first argument is restricted to be of type
-- @()@ it is consumed, hence @seqUnit@ is linear in its first argument.
seqUnit :: () %1-> b %1-> b
seqUnit () b = b

-- | Consume the first argument and return the second argument.
-- This is like 'seq' but the first argument is restricted to be 'Consumable'.
lseq :: Consumable a => a %1-> b %1-> b
lseq a b = seqUnit (consume a) b

-- ----------------
-- Generic deriving

instance (Generic a, GConsumable (Rep a)) => Consumable (Generically a) where
  consume (Generically x) = genericConsume x

genericConsume :: (Generic a, GConsumable (Rep a)) => a %1-> ()
genericConsume = gconsume . from
{-# INLINABLE genericConsume #-}

-- | A class for generic representations that can be consumed.
class GConsumable f where
  gconsume :: f p %1-> ()
instance GConsumable V1 where
  gconsume = \case
  {-# INLINE gconsume #-}
instance GConsumable U1 where
  gconsume U1 = ()
  {-# INLINE gconsume #-}
instance (GConsumable f, GConsumable g) => GConsumable (f :+: g) where
  gconsume (L1 a) = gconsume a
  gconsume (R1 a) = gconsume a
  {-# INLINE gconsume #-}
instance (GConsumable f, GConsumable g) => GConsumable (f :*: g) where
  gconsume (a :*: b) = gconsume a `seqUnit` gconsume b
  {-# INLINE gconsume #-}
instance Consumable c => GConsumable (K1 i c) where
  gconsume (K1 c) = consume c
  {-# INLINE gconsume #-}
instance GConsumable f => GConsumable (M1 i t f) where
  gconsume (M1 a) = gconsume a
  {-# INLINE gconsume #-}

-- This split is a bit awkward. We'd like to be able to *default*
-- the multiplicity to `Many` when it's polymorphic. We'll be able
-- to do that once the Exportable Named Defaults Proposal
-- (https://github.com/ghc-proposals/ghc-proposals/pull/409#issuecomment-931839874)
-- has been implemented. The same goes for Dupable and Movable.
instance GConsumable (MP1 'Many f) where
  gconsume (MP1 _) = ()
  {-# INLINE gconsume #-}
instance GConsumable f => GConsumable (MP1 'One f) where
  gconsume (MP1 x) = gconsume x
  {-# INLINE gconsume #-}
