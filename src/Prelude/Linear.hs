{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Prelude.Linear
  ( -- * Standard 'Prelude' function with linear types
    -- $linearized-prelude
    ($)
  , const
  , id
  , seq
  , curry
  , uncurry
  , (.)
  , either
  , maybe
    -- * Unrestricted
    -- $ unrestricted
  , Unrestricted(..)
  , unUnrestricted
    -- * Typeclasses for non-linear actions
    -- $ comonoid
  , Consumable(..)
  , Dupable(..)
  , Movable(..)
  , void
  , lseq
  , dup
  , dup2
  , dup3
    -- * Re-exports from the standard 'Prelude' for convenience
  , module Prelude
  ) where

import qualified Data.Functor.Linear as Data
import Data.Vector.Linear (V)
import qualified Data.Vector.Linear as V
import GHC.TypeLits
import GHC.Types
import Prelude hiding
  ( ($)
  , id
  , const
  , seq
  , curry
  , uncurry
  , either
  , maybe
  , (.)
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  )
import Prelude.Linear.Internal.Simple
import qualified Unsafe.Linear as Unsafe

-- | Linearly typed replacement for the standard `either` function, to allow
-- linear consumption of an @Either@.
either :: (a ->. c) -> (b ->. c) -> Either a b ->. c
either f _ (Left x) = f x
either _ g (Right y) = g y

-- | Linearly typed replacement for the standard `maybe` function, to allow
-- linear consumption of a @Maybe@.
maybe :: b -> (a ->. b) -> Maybe a ->. b
maybe x _ Nothing = x
maybe _ f (Just y) = f y

-- $ unrestricted

-- | @Unrestricted a@ represents unrestricted values of type @a@ in a linear context,
data Unrestricted a where
  Unrestricted :: a -> Unrestricted a

-- | Project an @a@ out of an @Unrestricted a@. If the @Unrestricted a@ is
-- linear, then we get only a linear value out.
unUnrestricted :: Unrestricted a ->. a
unUnrestricted (Unrestricted a) = a

-- $ comonoid

class Consumable a where
  consume :: a ->. ()

-- | Discard a consumable value stored in a data functor.
void :: (Data.Functor f, Consumable a) => f a ->. f ()
void = Data.fmap consume

-- | Like 'seq' but since the first argument is restricted to be of type @()@ it
-- is consumed, hence @seqUnit@ is linear in its first argument.
seqUnit :: () ->. b ->. b
seqUnit () b = b

-- | Like 'seq' but the first argument is restricted to be 'Consumable'. Hence the
-- first argument is 'consume'-ed and the result consumed.
lseq :: Consumable a => a ->. b ->. b
lseq a b = seqUnit (consume a) b

-- | The laws of @Dupable@ are dual to those of 'Monoid':
--
-- * @first consume (dup a) ≃ a ≃ first consume (dup a)@ (neutrality)
-- * @first dup (dup a) ≃ (second dup (dup a))@ (associativity)
--
-- Where the @(≃)@ sign represent equality up to type isomorphism
class Consumable a => Dupable a where
  dupV :: KnownNat n => a ->. V n a

-- | The laws of the @Movable@ class mean that @move@ is compatible with @consume@
-- and @dup@.
--
-- * @case move x of {Unrestricted _ -> ()} = consume x@ (this law is trivial)
-- * @case move x of {Unrestricted x -> x} = x@
-- * @case move x of {Unrestricted x -> (x, x)} = dup x@
class Dupable a => Movable a where
  move :: a ->. Unrestricted a

dup2 :: Dupable a => a ->. (a, a)
dup2 x = V.elim (dupV @_ @2 x) (,)

dup3 :: Dupable a => a ->. (a, a, a)
dup3 x = V.elim (dupV @_ @3 x) (,,)

dup :: Dupable a => a ->. (a, a)
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

instance Consumable a => Consumable [a] where
  consume [] = ()
  consume (a:l) = consume a `lseq` consume l

instance Dupable a => Dupable [a] where
  dupV = Data.traverse dupV

instance Movable a => Movable [a] where
  move = Data.traverse move

instance Consumable (Unrestricted a) where
  consume (Unrestricted _) = ()

instance Dupable (Unrestricted a) where
  dupV (Unrestricted a) = Data.pure (Unrestricted a)

instance Movable (Unrestricted a) where
  move (Unrestricted a) = Unrestricted (Unrestricted a)

instance Data.Functor Unrestricted where
  fmap f (Unrestricted a) = Unrestricted (f a)

instance Data.Applicative Unrestricted where
  pure = Unrestricted
  Unrestricted f <*> Unrestricted x = Unrestricted (f x)
