{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Unrestricted.Consumable
  ( Consumable(..)
  , lseq
  , void
  )
  where


import Data.List.NonEmpty
import qualified Prelude
import GHC.Types hiding (Any)
import qualified Unsafe.Linear as Unsafe
import qualified Data.Functor.Linear.Internal as Data

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




instance Consumable () where
  consume () = ()

instance Consumable Bool where
  consume True = ()
  consume False = ()

instance Consumable Int where
  -- /!\ 'Int#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int#' and using it several times. /!\
  consume (I# i) = Unsafe.toLinear (\_ -> ()) i


instance Consumable Double where
  -- /!\ 'Double#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Double#' and using it several times. /!\
  consume (D# i) = Unsafe.toLinear (\_ -> ()) i


instance Consumable Char where
  consume (C# c) = Unsafe.toLinear (\_ -> ()) c


instance (Consumable a, Consumable b) => Consumable (a, b) where
  consume (a, b) = consume a `lseq` consume b


instance (Consumable a, Consumable b, Consumable c) => Consumable (a, b, c) where
  consume (a, b, c) = consume a `lseq` consume b `lseq` consume c


instance Consumable a => Consumable (Prelude.Maybe a) where
  consume Prelude.Nothing = ()
  consume (Prelude.Just x) = consume x


instance (Consumable a, Consumable b) => Consumable (Prelude.Either a b) where
  consume (Prelude.Left a) = consume a
  consume (Prelude.Right b) = consume b


instance Consumable a => Consumable [a] where
  consume [] = ()
  consume (a:l) = consume a `lseq` consume l

instance Consumable a => Consumable (NonEmpty a) where
  consume (x :| xs) = consume x `lseq` consume xs

-- | Discard a consumable value stored in a data functor.
void :: (Data.Functor f, Consumable a) => f a %1-> f ()
void = Data.fmap consume

