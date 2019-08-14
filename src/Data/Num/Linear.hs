{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Num.Linear
  ( Num(..)
  )
  where

import qualified Prelude
import Data.Unrestricted.Linear
import qualified Unsafe.Linear as Unsafe

-- XXX: subclass of Prelude.Num? subclass of Eq?
class Num a where
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  -- | Having a linear (*) means we can't short-circut multiplication by zero
  (+), (-), (*) :: a ->. a ->. a
  negate :: a ->. a
  -- XXX: is it fine to insist abs,signum are linear? I think it is
  abs :: a ->. a
  signum :: a ->. a
  fromInteger :: Prelude.Integer ->. a

  x - y = x + negate y
  negate x = 0 - x

newtype MovableNum a = MovableNum a
  deriving (Consumable, Dupable, Movable, Prelude.Num)

instance (Movable a, Prelude.Num a) => Num (MovableNum a) where
  (+) = liftU2 (Prelude.+)
  (-) = liftU2 (Prelude.-)
  (*) = liftU2 (Prelude.*)
  abs = liftU Prelude.abs
  signum = liftU Prelude.signum
  fromInteger = Unsafe.toLinear Prelude.fromInteger

liftU :: (Movable a) => (a -> b) ->. (a ->. b)
liftU f x = lifted f (move x)
  where lifted :: (a -> b) ->. (Unrestricted a ->. b)
        lifted g (Unrestricted a) = g a

liftU2 :: (Movable a, Movable b) => (a -> b -> c) ->. (a ->. b ->. c)
liftU2 f x y = lifted f (move x) (move y)
  where lifted :: (a -> b -> c) ->. (Unrestricted a ->. Unrestricted b ->. c)
        lifted g (Unrestricted a) (Unrestricted b) = g a b

deriving via MovableNum Prelude.Int instance Num Prelude.Int
deriving via MovableNum Prelude.Double instance Num Prelude.Double
