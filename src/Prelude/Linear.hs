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
  , swap
  , seq
  , curry
  , uncurry
  , (.)
    -- * Unrestricted
    -- $ unrestricted
  , Unrestricted(..)
  , unUnrestricted
    -- * Typeclasses for non-linear actions
    -- $ comonoid
  , Consumable(..)
  , Dupable(..)
  , Movable(..)
  , lseq
  , dup3
    -- * Re-exports from the standard 'Prelude' for convenience
  , module Prelude
  ) where

import qualified Unsafe.Linear as Unsafe
import GHC.Types
import Prelude hiding
  ( ($)
  , id
  , const
  , seq
  , curry
  , uncurry
  , (.)
  , Functor(..)
  , Applicative(..)
  , Monad(..)
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

id :: a ->. a
id x = x

const :: a ->. b -> a
const x _ = x

-- XXX: To be decided: In `base`, this is not a prelude function (it's in
-- `Data.Tuple`), maybe we don't want it to be in `Prelude.Linear`.
swap :: (a,b) ->. (b,a)
swap (x,y) = (y,x)

-- | @seq x y@ only forces @x@ to head normal form, therefore is not guaranteed
-- to consume @x@ when the resulting computation is consumed. Therefore, @seq@
-- cannot be linear in it's first argument.
seq :: a -> b ->. b
seq x = Unsafe.toLinear (Prelude.seq x)


-- | Beware, 'curry' is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
curry :: ((a, b) ->. c) ->. a ->. b ->. c
curry f x y = f (x, y)

-- | Beware, 'uncurry' is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
uncurry :: (a ->. b ->. c) ->. (a, b) ->. c
uncurry f (x,y) = f x y

-- | Beware: @(.)@ is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
(.) :: (b ->. c) -> (a ->. b) ->. a ->. c
f . g = \x -> f (g x)

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

dup3 :: Dupable a => a ->. (a, a, a)
dup3 x = oneMore $ dup x
  where
    oneMore :: Dupable a => (a, a) ->. (a, a, a)
    oneMore (y, z) = flatten (y, dup z)

    flatten :: (a, (a, a)) ->. (a, a, a)
    flatten (y, (z, w)) = (y, z, w)

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
  dup (I# i) = Unsafe.toLinear (\j -> (I# j, I# j)) i

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
  dup (a, b) = shuffle (dup a) (dup b)
    where
      shuffle :: (a, a) ->. (b, b) ->. ((a, b), (a, b))
      shuffle (a', a'') (b', b'') = ((a', b'), (a'', b''))

instance (Movable a, Movable b) => Movable (a, b) where
  move (a, b) = liftu (move a) (move b)
    where
       -- XXX: this is merely an application of 'Unrestricted' being a linear
       -- applicative functor of some sort.
      liftu :: Unrestricted a ->. Unrestricted b ->. Unrestricted (a, b)
      liftu (Unrestricted a') (Unrestricted b') = Unrestricted (a', b')

instance (Consumable a, Consumable b, Consumable c) => Consumable (a, b, c) where
  consume (a, b, c) = consume a `lseq` consume b `lseq` consume c

instance (Dupable a, Dupable b, Dupable c) => Dupable (a, b, c) where
  dup (a, b, c) = shuffle (dup a) (dup b) (dup c)
    where
      shuffle :: (a, a) ->. (b, b) ->. (c, c) ->. ((a, b, c), (a, b, c))
      shuffle (a', a'') (b', b'') (c', c'') = ((a', b', c'), (a'', b'', c''))

instance (Movable a, Movable b, Movable c) => Movable (a, b, c) where
  move (a, b, c) = liftu (move a) (move b) (move c)
    where
      liftu :: Unrestricted a ->. Unrestricted b ->. Unrestricted c ->. Unrestricted (a, b, c)
      liftu (Unrestricted a') (Unrestricted b') (Unrestricted c') = Unrestricted (a', b', c')

instance Consumable a => Consumable [a] where
  consume [] = ()
  consume (a:l) = consume a `lseq` consume l

instance Dupable a => Dupable [a] where
  dup [] = ([], [])
  dup (a:l) = shuffle (dup a) (dup l)
    where
      shuffle :: (a, a) ->. ([a], [a]) ->. ([a], [a])
      shuffle (a', a'') (l', l'') = (a':l', a'':l'')

instance Movable a => Movable [a] where
  move [] = Unrestricted []
  move (a:l) = liftu (move a) (move l)
    where
       -- XXX: this is merely an application of 'Unrestricted' being a linear
       -- applicative functor of some sort.
      liftu :: Unrestricted a ->. Unrestricted [a] ->. Unrestricted [a]
      liftu (Unrestricted a') (Unrestricted l') = Unrestricted (a':l')

instance Consumable (Unrestricted a) where
  consume (Unrestricted _) = ()

instance Dupable (Unrestricted a) where
  dup (Unrestricted a) = (Unrestricted a, Unrestricted a)

instance Movable (Unrestricted a) where
  move (Unrestricted a) = Unrestricted (Unrestricted a)
