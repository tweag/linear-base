{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | This module provides a linear 'Num' class with instances.
-- Import this module to use linear versions of @(+)@, @(-)@, etc, on numeric
-- types like 'Int' and 'Double'.
--
-- == The Typeclass Hierarchy
--
-- The 'Num' class is broken up into several instances. Here is the basic
-- hierarchy:
--
-- * Additive ⊆ AddIdentity ⊆ AdditiveGroup
-- * MultIdentity ⊆ MultIdentity
-- * (AddIdentity ∩ MultIdentity) ⊆ Semiring
-- * (AdditiveGroup ∩ Semiring) ⊆ Ring
-- * (FromInteger ∩ Ring) ⊆ Num
--
module Data.Num.Linear
  (
  -- * Num and sub-classes
    Num(..)
  , Additive(..)
  , AddIdentity(..)
  , AdditiveGroup(..)
  , Multiplicative(..)
  , MultIdentity(..)
  , Semiring
  , Ring
  , FromInteger(..)
  -- * Mechanisms for deriving instances
  , Adding(..), getAdded
  , Multiplying(..), getMultiplied
  )
  where

-- TODO: flesh out laws
import qualified Prelude
import Data.Unrestricted.Linear
import qualified Unsafe.Linear as Unsafe
import Data.Monoid.Linear

-- | A type that can be added linearly.  The operation @(+)@ is associative and
-- commutative, i.e., for all @a@, @b@, @c@
--
-- > (a + b) + c = a + (b + c)
-- > a + b = b + c
class Additive a where
  (+) :: a %1-> a %1-> a

-- | An 'Additive' type with an identity on @(+)@.
class Additive a => AddIdentity a where
  zero :: a

-- | An 'AddIdentity' with inverses that satisfies
-- the laws of an [abelian group](https://en.wikipedia.org/wiki/Abelian_group)
class AddIdentity a => AdditiveGroup a where
  {-# MINIMAL negate | (-) #-}
  negate :: a %1-> a
  negate x = zero - x
  (-) :: a %1-> a %1-> a
  x - y = x + negate y

-- | A numeric type with an associative @(*)@ operation
class Multiplicative a where
  (*) :: a %1-> a %1-> a

-- | A 'Multipcative' type with an identity for @(*)@
class Multiplicative a => MultIdentity a where
  one :: a

-- | A [semiring](https://en.wikipedia.org/wiki/Semiring) class. This is
-- basically a numeric type with mutliplication, addition and with identities
-- for each. The laws:
--
-- > zero * x = zero
-- > a * (b + c) = (a * b) + (a * c)
class (AddIdentity a, MultIdentity a) => Semiring a where

-- Note:
-- Having a linear (*) means we can't short-circuit multiplication by zero

-- | A 'Ring' instance is a numeric type with @(+)@, @(-)@, @(*)@ and all
-- the following properties: a group with @(+)@ and a 'MultIdentity' with @(*)@
-- along with distributive laws.
class (AdditiveGroup a, Semiring a) => Ring a where


-- | A numeric type that 'Integer's can be embedded into while satisfying
-- all the typeclass laws @Integer@s obey. That is, if there's some property
-- like commutivity of integers @x + y == y + x@, then we must have:
--
-- > fromInteger x + fromInteger y == fromInteger y + fromInteger x
--
-- For mathy folk: @fromInteger@ should be a homomorphism over @(+)@ and @(*)@.
class FromInteger a where
  fromInteger :: Prelude.Integer %1-> a

-- XXX: subclass of Prelude.Num? subclass of Eq?
class (Ring a, FromInteger a) => Num a where
  {-# MINIMAL abs, signum #-}
  -- XXX: is it fine to insist abs,signum are linear? I think it is
  abs :: a %1-> a
  signum :: a %1-> a

newtype MovableNum a = MovableNum a
  deriving (Consumable, Dupable, Movable, Prelude.Num)

instance (Movable a, Prelude.Num a) => Additive (MovableNum a) where
  (+) = liftU2 (Prelude.+)

instance (Movable a, Prelude.Num a) => AddIdentity (MovableNum a) where
  zero = MovableNum 0

instance (Movable a, Prelude.Num a) => AdditiveGroup (MovableNum a) where
  (-) = liftU2 (Prelude.-)

instance (Movable a, Prelude.Num a) => Multiplicative (MovableNum a) where
  (*) = liftU2 (Prelude.*)

instance (Movable a, Prelude.Num a) => MultIdentity (MovableNum a) where
  one = MovableNum 1

instance (Movable a, Prelude.Num a) => Semiring (MovableNum a) where
instance (Movable a, Prelude.Num a) => Ring (MovableNum a) where

instance (Movable a, Prelude.Num a) => FromInteger (MovableNum a) where
  fromInteger = Unsafe.toLinear Prelude.fromInteger

instance (Movable a, Prelude.Num a) => Num (MovableNum a) where
  abs = liftU Prelude.abs
  signum = liftU Prelude.signum

liftU :: (Movable a) => (a -> b) %1-> (a %1-> b)
liftU f x = lifted f (move x)
  where lifted :: (a -> b) %1-> (Ur a %1-> b)
        lifted g (Ur a) = g a

liftU2 :: (Movable a, Movable b) => (a -> b -> c) %1-> (a %1-> b %1-> c)
liftU2 f x y = lifted f (move x) (move y)
  where lifted :: (a -> b -> c) %1-> (Ur a %1-> Ur b %1-> c)
        lifted g (Ur a) (Ur b) = g a b

-- A newtype wrapper to give the underlying monoid for an additive structure.
newtype Adding a = Adding a
  deriving Prelude.Semigroup via NonLinear (Adding a)

getAdded :: Adding a %1-> a
getAdded (Adding x) = x

instance Additive a => Semigroup (Adding a) where
  Adding a <> Adding b = Adding (a + b)
instance AddIdentity a => Prelude.Monoid (Adding a) where
  mempty = Adding zero
instance AddIdentity a => Monoid (Adding a)

-- A newtype wrapper to give the underlying monoid for a multiplicative structure.
newtype Multiplying a = Multiplying a
  deriving Prelude.Semigroup via NonLinear (Multiplying a)

getMultiplied :: Multiplying a %1-> a
getMultiplied (Multiplying x) = x

instance Multiplicative a => Semigroup (Multiplying a) where
  Multiplying a <> Multiplying b = Multiplying (a * b)
instance MultIdentity a => Prelude.Monoid (Multiplying a) where
  mempty = Multiplying one
instance MultIdentity a => Monoid (Multiplying a)

deriving via MovableNum Prelude.Int instance Additive Prelude.Int
deriving via MovableNum Prelude.Double instance Additive Prelude.Double
deriving via MovableNum Prelude.Int instance AddIdentity Prelude.Int
deriving via MovableNum Prelude.Double instance AddIdentity Prelude.Double
deriving via MovableNum Prelude.Int instance AdditiveGroup Prelude.Int
deriving via MovableNum Prelude.Double instance AdditiveGroup Prelude.Double
deriving via MovableNum Prelude.Int instance Multiplicative Prelude.Int
deriving via MovableNum Prelude.Double instance Multiplicative Prelude.Double
deriving via MovableNum Prelude.Int instance MultIdentity Prelude.Int
deriving via MovableNum Prelude.Double instance MultIdentity Prelude.Double
deriving via MovableNum Prelude.Int instance Semiring Prelude.Int
deriving via MovableNum Prelude.Double instance Semiring Prelude.Double
deriving via MovableNum Prelude.Int instance Ring Prelude.Int
deriving via MovableNum Prelude.Double instance Ring Prelude.Double
deriving via MovableNum Prelude.Int instance FromInteger Prelude.Int
deriving via MovableNum Prelude.Double instance FromInteger Prelude.Double
deriving via MovableNum Prelude.Int instance Num Prelude.Int
deriving via MovableNum Prelude.Double instance Num Prelude.Double
