{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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

-- | This module exports instances of Consumable, Dupable and Movable
--
-- We export instances in this module to avoid a circular dependence
-- and keep things clean. Movable depends on the defintion of Ur yet
-- many instances of Movable which we might have put in the module with
-- Movable depend on Ur. So, we just put the instances of Movable and the
-- other classes (for cleanness) in this module to avoid this dependence.
module Data.Unrestricted.Internal.Instances where

import Data.Unrestricted.Internal.Consumable
import Data.Unrestricted.Internal.Dupable
import Data.Unrestricted.Internal.Movable
import Data.Unrestricted.Internal.Ur
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Applicative as Data
import GHC.Types hiding (Any)
import GHC.Word
import GHC.Int
import Data.Monoid.Linear
import Data.List.NonEmpty
import qualified Prelude
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import Data.V.Linear ()
import Data.V.Linear.Internal.V (V(..), theLength)
import qualified Data.Vector as Vector
import GHC.TypeLits

instance Consumable () where
  consume () = ()

instance Dupable () where
  dupV () = Data.pure ()

instance Movable () where
  move () = Ur ()

instance Consumable Bool where
  consume True = ()
  consume False = ()

instance Dupable Bool where
  dupV True = Data.pure True
  dupV False = Data.pure False

instance Movable Bool where
  move True = Ur True
  move False = Ur False

instance Consumable Int where
  -- /!\ 'Int#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int#' and using it several times. /!\
  consume (I# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Int where
  -- /!\ 'Int#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int#' and using it several times. /!\
  dupV (I# i) = Unsafe.toLinear (\j -> Data.pure (I# j)) i

instance Movable Int where
  -- /!\ 'Int#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int#' and using it several times. /!\
  move (I# i) = Unsafe.toLinear (\j -> Ur (I# j)) i

instance Consumable Int8 where
  -- /!\ 'Int8#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int8#' and using it several times. /!\
  consume (I8# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Int8 where
  -- /!\ 'Int8#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int8#' and using it several times. /!\
  dupV (I8# i) = Unsafe.toLinear (\j -> Data.pure (I8# j)) i

instance Movable Int8 where
  -- /!\ 'Int8#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int8#' and using it several times. /!\
  move (I8# i) = Unsafe.toLinear (\j -> Ur (I8# j)) i

instance Consumable Int16 where
  -- /!\ 'Int16#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int16#' and using it several times. /!\
  consume (I16# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Int16 where
  -- /!\ 'Int16#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int16#' and using it several times. /!\
  dupV (I16# i) = Unsafe.toLinear (\j -> Data.pure (I16# j)) i

instance Movable Int16 where
  -- /!\ 'Int16#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int16#' and using it several times. /!\
  move (I16# i) = Unsafe.toLinear (\j -> Ur (I16# j)) i

instance Consumable Int32 where
  -- /!\ 'Int32#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int32#' and using it several times. /!\
  consume (I32# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Int32 where
  -- /!\ 'Int32#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int32#' and using it several times. /!\
  dupV (I32# i) = Unsafe.toLinear (\j -> Data.pure (I32# j)) i

instance Movable Int32 where
  -- /!\ 'Int32#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int32#' and using it several times. /!\
  move (I32# i) = Unsafe.toLinear (\j -> Ur (I32# j)) i

instance Consumable Int64 where
  -- /!\ 'Int64#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int64#' and using it several times. /!\
  consume (I64# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Int64 where
  -- /!\ 'Int64#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int64#' and using it several times. /!\
  dupV (I64# i) = Unsafe.toLinear (\j -> Data.pure (I64# j)) i

instance Movable Int64 where
  -- /!\ 'Int64#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int64#' and using it several times. /!\
  move (I64# i) = Unsafe.toLinear (\j -> Ur (I64# j)) i

instance Consumable Double where
  -- /!\ 'Double#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Double#' and using it several times. /!\
  consume (D# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Double where
  -- /!\ 'Double#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Double#' and using it several times. /!\
  dupV (D# i) = Unsafe.toLinear (\j -> Data.pure (D# j)) i

instance Movable Double where
  -- /!\ 'Double#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Double#' and using it several times. /!\
  move (D# i) = Unsafe.toLinear (\j -> Ur (D# j)) i

instance Consumable Word where
  -- /!\ 'Word#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word#' and using it several times. /!\
  consume (W# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Word where
  -- /!\ 'Word#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word#' and using it several times. /!\
  dupV (W# i) = Unsafe.toLinear (\j -> Data.pure (W# j)) i

instance Movable Word where
  -- /!\ 'Word#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word#' and using it several times. /!\
  move (W# i) = Unsafe.toLinear (\j -> Ur (W# j)) i

instance Consumable Word8 where
  -- /!\ 'Word8#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word8#' and using it several times. /!\
  consume (W8# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Word8 where
  -- /!\ 'Word8#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word8#' and using it several times. /!\
  dupV (W8# i) = Unsafe.toLinear (\j -> Data.pure (W8# j)) i

instance Movable Word8 where
  -- /!\ 'Word8#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word8#' and using it several times. /!\
  move (W8# i) = Unsafe.toLinear (\j -> Ur (W8# j)) i

instance Consumable Word16 where
  -- /!\ 'Word16#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word16#' and using it several times. /!\
  consume (W16# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Word16 where
  -- /!\ 'Word16#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word16#' and using it several times. /!\
  dupV (W16# i) = Unsafe.toLinear (\j -> Data.pure (W16# j)) i

instance Movable Word16 where
  -- /!\ 'Word16#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word16#' and using it several times. /!\
  move (W16# i) = Unsafe.toLinear (\j -> Ur (W16# j)) i

instance Consumable Word32 where
  -- /!\ 'Word32#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word32#' and using it several times. /!\
  consume (W32# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Word32 where
  -- /!\ 'Word32#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word32#' and using it several times. /!\
  dupV (W32# i) = Unsafe.toLinear (\j -> Data.pure (W32# j)) i

instance Movable Word32 where
  -- /!\ 'Word32#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word32#' and using it several times. /!\
  move (W32# i) = Unsafe.toLinear (\j -> Ur (W32# j)) i

instance Consumable Word64 where
  -- /!\ 'Word64#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word64#' and using it several times. /!\
  consume (W64# i) = Unsafe.toLinear (\_ -> ()) i

instance Dupable Word64 where
  -- /!\ 'Word64#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word64#' and using it several times. /!\
  dupV (W64# i) = Unsafe.toLinear (\j -> Data.pure (W64# j)) i

instance Movable Word64 where
  -- /!\ 'Word64#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word64#' and using it several times. /!\
  move (W64# i) = Unsafe.toLinear (\j -> Ur (W64# j)) i

instance Consumable Char where
  consume (C# c) = Unsafe.toLinear (\_ -> ()) c

instance Dupable Char where
  dupV (C# c) = Unsafe.toLinear (\x -> Data.pure (C# x)) c

instance Movable Char where
  move (C# c) = Unsafe.toLinear (\x -> Ur (C# x)) c

instance Consumable Ordering where
  consume LT = ()
  consume GT = ()
  consume EQ = ()

instance Dupable Ordering where
  dup2 LT = (LT, LT)
  dup2 GT = (GT, GT)
  dup2 EQ = (EQ, EQ)

instance Movable Ordering where
  move LT = Ur LT
  move GT = Ur GT
  move EQ = Ur EQ

-- TODO: instances for longer primitive tuples
-- TODO: default instances based on the Generic framework

instance (Consumable a, Consumable b) => Consumable (a, b) where
  consume (a, b) = consume a `lseq` consume b

instance (Dupable a, Dupable b) => Dupable (a, b) where
  dupV (a, b) = (,) Data.<$> dupV a Data.<*> dupV b

instance (Movable a, Movable b) => Movable (a, b) where
  move (a, b) = (,) Data.<$> move a Data.<*> move b

instance (Consumable a, Consumable b, Consumable c) => Consumable (a, b, c) where
  consume (a, b, c) = consume a `lseq` consume b `lseq` consume c

instance (Dupable a, Dupable b, Dupable c) => Dupable (a, b, c) where
  dupV (a, b, c) = (,,) Data.<$> dupV a Data.<*> dupV b Data.<*> dupV c

instance (Movable a, Movable b, Movable c) => Movable (a, b, c) where
  move (a, b, c) = (,,) Data.<$> move a Data.<*> move b Data.<*> move c

-- XXX: The Consumable and Dupable instances for V will be easier to define (in
-- fact direct, we may consider adding a deriving-via combinator) when we have a
-- traversable-by-a-data-applicative class see #190.

instance (KnownNat n, Consumable a) => Consumable (V n a) where
  consume (V xs) = consume (Unsafe.toLinear Vector.toList xs)

instance (KnownNat n, Dupable a) => Dupable (V n a) where
  dupV (V xs) =
    V . Unsafe.toLinear (Vector.fromListN (theLength @n)) Data.<$>
    dupV (Unsafe.toLinear Vector.toList xs)

instance Consumable a => Consumable (Prelude.Maybe a) where
  consume Prelude.Nothing = ()
  consume (Prelude.Just x) = consume x

instance Dupable a => Dupable (Prelude.Maybe a) where
  dupV Prelude.Nothing = Data.pure Prelude.Nothing
  dupV (Prelude.Just x) = Data.fmap Prelude.Just (dupV x)

instance Movable a => Movable (Prelude.Maybe a) where
  move (Prelude.Nothing) = Ur Prelude.Nothing
  move (Prelude.Just x) = Data.fmap Prelude.Just (move x)

instance (Consumable a, Consumable b) => Consumable (Prelude.Either a b) where
  consume (Prelude.Left a) = consume a
  consume (Prelude.Right b) = consume b

instance (Dupable a, Dupable b) => Dupable (Prelude.Either a b) where
  dupV (Prelude.Left a) = Data.fmap Prelude.Left (dupV a)
  dupV (Prelude.Right b) = Data.fmap Prelude.Right (dupV b)

instance (Movable a, Movable b) => Movable (Prelude.Either a b) where
  move (Prelude.Left a) = Data.fmap Prelude.Left (move a)
  move (Prelude.Right b) = Data.fmap Prelude.Right (move b)

instance Consumable a => Consumable [a] where
  consume [] = ()
  consume (a:l) = consume a `lseq` consume l

instance Dupable a => Dupable [a] where
  dupV [] = Data.pure []
  dupV (a:l) = (:) Data.<$> dupV a Data.<*> dupV l

instance Movable a => Movable [a] where
  move [] = Ur []
  move (a:l) = (:) Data.<$> move a Data.<*> move l

instance Consumable a => Consumable (NonEmpty a) where
  consume (x :| xs) = consume x `lseq` consume xs

instance Dupable a => Dupable (NonEmpty a) where
  dupV (x :| xs) = (:|) Data.<$> dupV x Data.<*> dupV xs

instance Movable a => Movable (NonEmpty a) where
  move (x :| xs) = (:|) Data.<$> move x Data.<*> move xs

instance Consumable (Ur a) where
  consume (Ur _) = ()

instance Dupable (Ur a) where
  dupV (Ur a) = Data.pure (Ur a)

instance Movable (Ur a) where
  move (Ur a) = Ur (Ur a)

instance Prelude.Functor Ur where
  fmap f (Ur a) = Ur (f a)

instance Prelude.Applicative Ur where
  pure = Ur
  Ur f <*> Ur x = Ur (f x)

instance Data.Functor Ur where
  fmap f (Ur a) = Ur (f a)

instance Data.Applicative Ur where
  pure = Ur
  Ur f <*> Ur x = Ur (f x)

instance Prelude.Foldable Ur where
  foldMap f (Ur x) = f x

instance Prelude.Traversable Ur where
  sequenceA (Ur x) = Prelude.fmap Ur x

-- Some stock instances
deriving instance Consumable a => Consumable (Sum a)
deriving instance Dupable a => Dupable (Sum a)
deriving instance Movable a => Movable (Sum a)
deriving instance Consumable a => Consumable (Product a)
deriving instance Dupable a => Dupable (Product a)
deriving instance Movable a => Movable (Product a)
deriving instance Consumable All
deriving instance Dupable All
deriving instance Movable All
deriving instance Consumable Any
deriving instance Dupable Any
deriving instance Movable Any

newtype MovableMonoid a = MovableMonoid a
  deriving (Prelude.Semigroup, Prelude.Monoid)

instance (Movable a, Prelude.Semigroup a) => Semigroup (MovableMonoid a) where
  MovableMonoid a <> MovableMonoid b = MovableMonoid (combine (move a) (move b))
    where combine :: Prelude.Semigroup a => Ur a %1-> Ur a %1-> a
          combine (Ur x) (Ur y) = x Prelude.<> y
instance (Movable a, Prelude.Monoid a) => Monoid (MovableMonoid a)

