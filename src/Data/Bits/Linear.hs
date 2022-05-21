{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Bits.Linear
  ( -- * Bits and sub-classes
    BitwiseAnd (..)
  )
where

import qualified Data.Bits as Bits
import Data.Unrestricted.Linear
import qualified Prelude


liftU2 :: (Movable a, Movable b) => (a -> b -> c) %1 -> (a %1 -> b %1 -> c)
liftU2 f x y = lifted f (move x) (move y)
  where
    lifted :: (a -> b -> c) %1 -> (Ur a %1 -> Ur b %1 -> c)
    lifted g (Ur a) (Ur b) = g a b

newtype MovableBits a = MovableBits a
  deriving (Consumable, Dupable, Movable, Prelude.Eq, Bits.Bits)

class BitwiseAnd a where
  (.&.) :: a %1 -> a %1 -> a
  infixl 7 .&.

instance (Movable a, Bits.Bits a) => BitwiseAnd (MovableBits a) where
  (.&.) = liftU2 (Bits..&.)

deriving via MovableBits Prelude.Int instance BitwiseAnd Prelude.Int

deriving via MovableBits Prelude.Word instance BitwiseAnd Prelude.Word
