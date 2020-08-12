{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- In a linear function @f :: a #-> b@, the argument @a@ must
-- be used in a linear way. Its use is __restricted__ while
-- an argument in a non-linear function is __unrestricted__.
--
-- Hence, a linear function with an argument of @Unrestricted a@ can use the
-- @a@ in an unrestricted way. That is, we have the following equivalence:
--
-- @
-- (Unrestricted a #-> b) ≌ (a -> b)
-- @
--
-- = Consumable, Dupable, Moveable classes
--
-- Use these classes to perform some non-linear action on linearly bound values.
--
-- If a type is 'Consumable', you can __consume__ it in a linear function that
-- doesn't need that value to produce it's result:
--
-- > first :: Consumable b => (a,b) #-> a
-- > first (a,b) = withConsume (consume b) a
-- >   where
-- >     withConsume :: () #-> a #-> a
-- >     withConsume () x = x
--
-- If a type is 'Dupable', you can __duplicate__ it as much as you like.
--
-- > -- checkIndex ix size_of_array
-- > checkIndex :: Int #-> Int #-> Bool
-- > checkIndex ix size = withDuplicate (dup2 ix) size
-- >   where
-- >     withDuplicate :: (Int, Int) #-> Int #-> Bool
-- >     withDuplicate (ix,ix') size = (0 <= ix) && (ix < size)
-- >     (<) :: Int #-> Int #-> Bool
-- >     (<) = ...
-- >
-- >     (<=) :: Int #-> Int #-> Bool
-- >     (<=) = ...
-- >
-- >     (&&) :: Bool #-> Bool #-> Bool
-- >     (&&) = ...
--
-- If a type is 'Moveable', you can __move__ it inside 'Unrestricted'
-- and use it in any non-linear way you would like.
--
-- > diverge :: Int #-> Bool
-- > diverge ix = fromMove (move ix)
-- >   where
-- >     fromMove :: Unrestricted Int #-> Bool
-- >     fromMove (Unrestricted 0) = True
-- >     fromMove (Unrestricted 1) = True
-- >     fromMove (Unrestricted x) = False
--
module Data.Unrestricted.Linear
  ( -- * Unrestricted
    Unrestricted(..)
  , unUnrestricted
  , lift
  , lift2
    -- * Performing non-linear actions on linearly bound values
  , Consumable(..)
  , Dupable(..)
  , Movable(..)
  , void
  , lseq
  , dup
  , dup2
  , dup3
  ) where

import qualified Data.Functor.Linear.Internal as Data
import Data.Vector.Linear (V)
import qualified Data.Vector.Linear as V
import GHC.TypeLits
import GHC.Types hiding (Any)
import Data.Monoid.Linear
import qualified Prelude
import qualified Data.Semigroup as Prelude
import qualified Unsafe.Linear as Unsafe



-- | @Unrestricted a@ represents unrestricted values of type @a@ in a linear
-- context. The key idea is that because the contructor holds @a@ with a
-- regular arrow, a function that uses @Unrestricted a@ linearly can use @a@
-- however it likes.
-- > someLinear :: Unrestricted a #-> (a,a)
-- > someLinear (Unrestricted a) = (a,a)
data Unrestricted a where
  Unrestricted :: a -> Unrestricted a

-- | Get an @a@ out of an @Unrestricted a@. If you call this function on a
-- linearly bound @Unrestricted a@, then the @a@ you get out has to be used
-- linearly, for example:
--
-- > restricted :: Unrestricted a #-> b
-- > restricted x = f (unUnrestricted x)
-- >   where
-- >     -- f __must__ be linear
-- >     f :: a #-> b
-- >     f x = ...
unUnrestricted :: Unrestricted a #-> a
unUnrestricted (Unrestricted a) = a

-- | Lifts a function on a linear @Unrestricted a@.
lift :: (a -> b) -> Unrestricted a #-> Unrestricted b
lift f (Unrestricted a) = Unrestricted (f a)

-- | Lifts a function to work on two linear @Unrestricted a@.
lift2 :: (a -> b -> c) -> Unrestricted a #-> Unrestricted b #-> Unrestricted c
lift2 f (Unrestricted a) (Unrestricted b) = Unrestricted (f a b)


class Consumable a where
  consume :: a #-> ()

-- | Consume the unit and return the second argument.
-- This is like 'seq' but since the first argument is restricted to be of type
-- @()@ it is consumed, hence @seqUnit@ is linear in its first argument.
seqUnit :: () #-> b #-> b
seqUnit () b = b

-- | Consume the first argument and return the second argument.
-- This is like 'seq' but the first argument is restricted to be 'Consumable'.
lseq :: Consumable a => a #-> b #-> b
lseq a b = seqUnit (consume a) b

-- | The laws of @Dupable@ are dual to those of 'Monoid':
--
-- * @first consume (dup2 a) ≃ a ≃ second consume (dup2 a)@ (neutrality)
-- * @first dup2 (dup2 a) ≃ (second dup2 (dup2 a))@ (associativity)
--
-- Where the @(≃)@ sign represents equality up to type isomorphism.
class Consumable a => Dupable a where
  dupV :: KnownNat n => a #-> V n a

-- | The laws of the @Movable@ class mean that @move@ is compatible with
-- @consume@ and @dup@.
--
-- * @case move x of {Unrestricted _ -> ()} = consume x@
-- * @case move x of {Unrestricted x -> x} = x@
-- * @case move x of {Unrestricted x -> (x, x)} = dup2 x@
class Dupable a => Movable a where
  move :: a #-> Unrestricted a

dup2 :: Dupable a => a #-> (a, a)
dup2 x = V.elim (dupV @_ @2 x) (,)

dup3 :: Dupable a => a #-> (a, a, a)
dup3 x = V.elim (dupV @_ @3 x) (,,)

dup :: Dupable a => a #-> (a, a)
dup = dup2

instance Consumable () where
  consume () = ()

instance Dupable () where
  dupV () = Data.pure ()

instance Movable () where
  move () = Unrestricted ()

instance Consumable Bool where
  consume True = ()
  consume False = ()

instance Dupable Bool where
  dupV True = Data.pure True
  dupV False = Data.pure False

instance Movable Bool where
  move True = Unrestricted True
  move False = Unrestricted False

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
  move (I# i) = Unsafe.toLinear (\j -> Unrestricted (I# j)) i

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
  move (D# i) = Unsafe.toLinear (\j -> Unrestricted (D# j)) i

instance Consumable Char where
  consume (C# c) = Unsafe.toLinear (\_ -> ()) c

instance Dupable Char where
  dupV (C# c) = Unsafe.toLinear (\x -> Data.pure (C# x)) c

instance Movable Char where
  move (C# c) = Unsafe.toLinear (\x -> Unrestricted (C# x)) c

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

instance Consumable a => Consumable (Prelude.Maybe a) where
  consume Prelude.Nothing = ()
  consume (Prelude.Just x) = consume x

instance Dupable a => Dupable (Prelude.Maybe a) where
  dupV Prelude.Nothing = Data.pure Prelude.Nothing
  dupV (Prelude.Just x) = Data.fmap Prelude.Just (dupV x)

instance Movable a => Movable (Prelude.Maybe a) where
  move (Prelude.Nothing) = Unrestricted Prelude.Nothing
  move (Prelude.Just x) = Data.fmap Prelude.Just (move x)

instance Consumable a => Consumable [a] where
  consume [] = ()
  consume (a:l) = consume a `lseq` consume l

instance Dupable a => Dupable [a] where
  dupV [] = Data.pure []
  dupV (a:l) = (:) Data.<$> dupV a Data.<*> dupV l

instance Movable a => Movable [a] where
  move [] = Unrestricted []
  move (a:l) = (:) Data.<$> move a Data.<*> move l

instance Consumable (Unrestricted a) where
  consume (Unrestricted _) = ()

instance Dupable (Unrestricted a) where
  dupV (Unrestricted a) = Data.pure (Unrestricted a)

instance Movable (Unrestricted a) where
  move (Unrestricted a) = Unrestricted (Unrestricted a)

instance Prelude.Functor Unrestricted where
  fmap f (Unrestricted a) = Unrestricted (f a)

instance Prelude.Applicative Unrestricted where
  pure = Unrestricted
  Unrestricted f <*> Unrestricted x = Unrestricted (f x)

instance Data.Functor Unrestricted where
  fmap f (Unrestricted a) = Unrestricted (f a)

instance Data.Applicative Unrestricted where
  pure = Unrestricted
  Unrestricted f <*> Unrestricted x = Unrestricted (f x)

instance Prelude.Foldable Unrestricted where
  foldMap f (Unrestricted x) = f x

instance Prelude.Traversable Unrestricted where
  sequenceA (Unrestricted x) = Prelude.fmap Unrestricted x

-- | Discard a consumable value stored in a data functor.
void :: (Data.Functor f, Consumable a) => f a #-> f ()
void = Data.fmap consume

-- Some stock instances
deriving instance Consumable a => Consumable (Prelude.Sum a)
deriving instance Dupable a => Dupable (Prelude.Sum a)
deriving instance Movable a => Movable (Prelude.Sum a)
deriving instance Consumable a => Consumable (Prelude.Product a)
deriving instance Dupable a => Dupable (Prelude.Product a)
deriving instance Movable a => Movable (Prelude.Product a)
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
    where combine :: Prelude.Semigroup a => Unrestricted a #-> Unrestricted a #-> a
          combine (Unrestricted x) (Unrestricted y) = x Prelude.<> y
instance (Movable a, Prelude.Monoid a) => Monoid (MovableMonoid a)
