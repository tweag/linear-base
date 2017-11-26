{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Linear
  ( -- * Standard 'Prelude' function with linear types
    -- $linearized-prelude
    ($)
  , const
  , swap
  , seq
    -- * Unrestricted
    -- $ unrestricted
  , Unrestricted(..)
  , unUnrestricted
    -- * Typeclasses for non-linear actions
    -- $ comonoid
  , Consumable
  , Dupable
  , Movable
  , lseq

    -- * Re-exports from the standard 'Prelude' for convenience
  , module Prelude
  ) where

import qualified Unsafe.Linear as Unsafe
import Prelude hiding
  ( ($)
  , const
  , seq
  , swap
  )
import qualified Prelude

-- $linearized-prelude

-- A note on implementation: to avoid silly mistakes, very easy functions are
-- simply reimplemented here. For harder function, we reuse the Prelude
-- definition and make an unsafe cast.

-- | Beware: @($)@ is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
($) :: (a ->. b) ->. a ->. b
-- XXX: Temporary as `($)` should get its typing rule directly from the type
-- inference mechanism.
($) f x = f x

infixr 0 $

const :: a ->. b -> a
const x _ = x

swap :: (a,b) ->. (b,a)
swap (x,y) = (y,x)

-- | @seq x y@ only forces @x@ to head normal form, therefore is not guaranteed
-- to consume @x@ when the resulting computation is consumed. Therefore, @seq@
-- cannot be linear in it's first argument.
seq :: a -> b ->. b
seq x = Unsafe.castLinear (Prelude.seq x)

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
  dup :: a ->. (a, a)

-- | The laws of the @Movable@ class mean that @move@ is compatible with @consume@
-- and @dup@.
--
-- * @case move x of {Unrestricted _ -> ()} = consume x@ (this law is trivial)
-- * @case move x of {Unrestricted x -> x} = x@
-- * @case move x of {Unrestricted x -> (x, x)} = dup x@
class Dupable a => Movable a where
  move :: a ->. Unrestricted a

instance Consumable () where
  consume () = ()

instance Dupable () where
  dup () = ((), ())

instance Movable () where
  move () = Unrestricted ()

instance Consumable Bool where
  consume True = ()
  consume False = ()

instance Dupable Bool where
  dup True = (True, True)
  dup False = (False, False)

instance Movable Bool where
  move True = Unrestricted True
  move False = Unrestricted False

-- TODO: instances for Int, primitive tuples

instance Consumable a => Consumable [a] where
  consume [] = ()
  consume (a:l) = consume a `lseq` consume l

instance Dupable a => Dupable [a] where
  dup [] = ([], [])
  dup (a:l) = shuffle (dup a) (dup l)
    where
      shuffle :: (a, a) ->. ([a], [a]) ->. ([a], [a])
      shuffle (a, a') (l, l') = (a:l, a':l')

instance Movable a => Movable [a] where
  move [] = Unrestricted []
  move (a:l) = liftu (move a) (move l)
    where
       -- XXX: this is merely an application of 'Unrestricted' being a linear
       -- applicative functor of some sort.
      liftu :: Unrestricted a ->. Unrestricted [a] ->. Unrestricted [a]
      liftu (Unrestricted a) (Unrestricted l) = Unrestricted (a:l)

instance Consumable (Unrestricted a) where
  consume (Unrestricted _) = ()

instance Dupable (Unrestricted a) where
  dup (Unrestricted a) = (Unrestricted a, Unrestricted a)

instance Movable (Unrestricted a) where
  move (Unrestricted a) = Unrestricted (Unrestricted a)
