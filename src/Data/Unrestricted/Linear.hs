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

-- | This module provides essential tools for doing non-linear things
-- in linear code.
--
-- = /Critical/ Definition: Restricted
--
-- In a linear function @f :: a %1-> b@, the argument @a@ must
-- be used in a linear way. Its use is __restricted__ while
-- an argument in a non-linear function is __unrestricted__.
--
-- Hence, a linear function with an argument of @Ur a@ (@Ur@ is short for
-- /unrestricted/) can use the @a@ in an unrestricted way. That is, we have
-- the following equivalence:
--
-- @
-- (Ur a %1-> b) ≌ (a -> b)
-- @
--
-- = Consumable, Dupable, Moveable classes
--
-- Use these classes to perform some non-linear action on linearly bound values.
--
-- If a type is 'Consumable', you can __consume__ it in a linear function that
-- doesn't need that value to produce it's result:
--
-- > first :: Consumable b => (a,b) %1-> a
-- > first (a,b) = withConsume (consume b) a
-- >   where
-- >     withConsume :: () %1-> a %1-> a
-- >     withConsume () x = x
--
-- If a type is 'Dupable', you can __duplicate__ it as much as you like.
--
-- > -- checkIndex ix size_of_array
-- > checkIndex :: Int %1-> Int %1-> Bool
-- > checkIndex ix size = withDuplicate (dup2 ix) size
-- >   where
-- >     withDuplicate :: (Int, Int) %1-> Int %1-> Bool
-- >     withDuplicate (ix,ix') size = (0 <= ix) && (ix < size)
-- >     (<) :: Int %1-> Int %1-> Bool
-- >     (<) = ...
-- >
-- >     (<=) :: Int %1-> Int %1-> Bool
-- >     (<=) = ...
-- >
-- >     (&&) :: Bool %1-> Bool %1-> Bool
-- >     (&&) = ...
--
-- If a type is 'Moveable', you can __move__ it inside 'Ur'
-- and use it in any non-linear way you would like.
--
-- > diverge :: Int %1-> Bool
-- > diverge ix = fromMove (move ix)
-- >   where
-- >     fromMove :: Ur Int %1-> Bool
-- >     fromMove (Ur 0) = True
-- >     fromMove (Ur 1) = True
-- >     fromMove (Ur x) = False
--
module Data.Unrestricted.Linear
  ( -- * Unrestricted
    Ur(..)
  , unur
  , lift
  , lift2
    -- * Performing non-linear actions on linearly bound values
  , Consumable(..)
  , Dupable(..)
  , Movable(..)
  , void
  , lseq
  , dup
  , dup3
  ) where

import Data.Unrestricted.Consumable
import qualified Data.Functor.Linear.Internal as Data
import qualified Data.Applicative.Linear as Data
import Data.Type.Equality
import Data.V.Linear (V)
import qualified Data.V.Linear as V
import GHC.TypeLits
import GHC.Types hiding (Any)
import Data.Monoid.Linear
import Data.List.NonEmpty
import qualified Prelude
import qualified Unsafe.Linear as Unsafe

-- | @Ur a@ represents unrestricted values of type @a@ in a linear
-- context. The key idea is that because the contructor holds @a@ with a
-- regular arrow, a function that uses @Ur a@ linearly can use @a@
-- however it likes.
-- > someLinear :: Ur a %1-> (a,a)
-- > someLinear (Ur a) = (a,a)
data Ur a where
  Ur :: a -> Ur a

-- | Get an @a@ out of an @Ur a@. If you call this function on a
-- linearly bound @Ur a@, then the @a@ you get out has to be used
-- linearly, for example:
--
-- > restricted :: Ur a %1-> b
-- > restricted x = f (unur x)
-- >   where
-- >     -- f __must__ be linear
-- >     f :: a %1-> b
-- >     f x = ...
unur :: Ur a %1-> a
unur (Ur a) = a

-- | Lifts a function on a linear @Ur a@.
lift :: (a -> b) -> Ur a %1-> Ur b
lift f (Ur a) = Ur (f a)

-- | Lifts a function to work on two linear @Ur a@.
lift2 :: (a -> b -> c) -> Ur a %1-> Ur b %1-> Ur c
lift2 f (Ur a) (Ur b) = Ur (f a b)


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

-- | The laws of the @Movable@ class mean that @move@ is compatible with
-- @consume@ and @dup@.
--
-- * @case move x of {Ur _ -> ()} = consume x@
-- * @case move x of {Ur x -> x} = x@
-- * @case move x of {Ur x -> (x, x)} = dup2 x@
class Dupable a => Movable a where
  move :: a %1-> Ur a

dup3 :: Dupable a => a %1-> (a, a, a)
dup3 x = V.elim (dupV @_ @3 x) (,,)

dup :: Dupable a => a %1-> (a, a)
dup = dup2

instance Dupable () where
  dupV () = Data.pure ()

instance Movable () where
  move () = Ur ()

instance Dupable Bool where
  dupV True = Data.pure True
  dupV False = Data.pure False

instance Movable Bool where
  move True = Ur True
  move False = Ur False

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

instance (Dupable a, Dupable b) => Dupable (a, b) where
  dupV (a, b) = (,) Data.<$> dupV a Data.<*> dupV b

instance (Movable a, Movable b) => Movable (a, b) where
  move (a, b) = (,) Data.<$> move a Data.<*> move b

instance (Dupable a, Dupable b, Dupable c) => Dupable (a, b, c) where
  dupV (a, b, c) = (,,) Data.<$> dupV a Data.<*> dupV b Data.<*> dupV c

instance (Movable a, Movable b, Movable c) => Movable (a, b, c) where
  move (a, b, c) = (,,) Data.<$> move a Data.<*> move b Data.<*> move c

instance Dupable a => Dupable (Prelude.Maybe a) where
  dupV Prelude.Nothing = Data.pure Prelude.Nothing
  dupV (Prelude.Just x) = Data.fmap Prelude.Just (dupV x)

instance Movable a => Movable (Prelude.Maybe a) where
  move (Prelude.Nothing) = Ur Prelude.Nothing
  move (Prelude.Just x) = Data.fmap Prelude.Just (move x)

instance (Dupable a, Dupable b) => Dupable (Prelude.Either a b) where
  dupV (Prelude.Left a) = Data.fmap Prelude.Left (dupV a)
  dupV (Prelude.Right b) = Data.fmap Prelude.Right (dupV b)

instance (Movable a, Movable b) => Movable (Prelude.Either a b) where
  move (Prelude.Left a) = Data.fmap Prelude.Left (move a)
  move (Prelude.Right b) = Data.fmap Prelude.Right (move b)

instance Dupable a => Dupable [a] where
  dupV [] = Data.pure []
  dupV (a:l) = (:) Data.<$> dupV a Data.<*> dupV l

instance Movable a => Movable [a] where
  move [] = Ur []
  move (a:l) = (:) Data.<$> move a Data.<*> move l

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
