{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Num.Linear
  ( Additive(..)
  , AddIdentity(..)
  , AdditiveGroup(..)
  , Multiplicative(..)
  , MultIdentity(..)
  , Semiring
  , Ring
  , FromInteger(..)
  , Num(..)
  )
  where

-- TODO: underlying semigroups/monoids
-- TODO: flesh out laws
import qualified Prelude
import Data.Unrestricted.Linear
import qualified Unsafe.Linear as Unsafe

-- laws: associative, commutative
class Additive a where
  (+) :: a ->. a ->. a

-- laws: is an identity for +
class Additive a => AddIdentity a where
  zero :: a

-- usual abelian group laws
class AddIdentity a => AdditiveGroup a where
  {-# MINIMAL negate | (-) #-}
  negate :: a ->. a
  negate x = zero - x
  (-) :: a ->. a ->. a
  x - y = x + negate y

-- laws: associative
class Multiplicative a where
  (*) :: a ->. a ->. a

-- laws: is an identity for *
class Multiplicative a => MultIdentity a where
  one :: a

-- laws: distributivity, annihilation
-- | Having a linear (*) means we can't short-circuit multiplication by zero
class (AddIdentity a, MultIdentity a) => Semiring a where

-- no additional laws
class (AdditiveGroup a, Semiring a) => Ring a where

-- no laws on its own, but should be compatible with all above classes if both
-- are given. In particular, fromInteger should be a homomorphism to the
-- relevant structure
class FromInteger a where
  fromInteger :: Prelude.Integer ->. a

-- XXX: subclass of Prelude.Num? subclass of Eq?
class (Ring a, FromInteger a) => Num a where
  {-# MINIMAL abs, signum #-}
  -- XXX: is it fine to insist abs,signum are linear? I think it is
  abs :: a ->. a
  signum :: a ->. a

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

liftU :: (Movable a) => (a -> b) ->. (a ->. b)
liftU f x = lifted f (move x)
  where lifted :: (a -> b) ->. (Unrestricted a ->. b)
        lifted g (Unrestricted a) = g a

liftU2 :: (Movable a, Movable b) => (a -> b -> c) ->. (a ->. b ->. c)
liftU2 f x y = lifted f (move x) (move y)
  where lifted :: (a -> b -> c) ->. (Unrestricted a ->. Unrestricted b ->. c)
        lifted g (Unrestricted a) (Unrestricted b) = g a b

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
