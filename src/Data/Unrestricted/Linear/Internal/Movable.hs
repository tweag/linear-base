{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Unrestricted.Linear.Internal.Movable
  ( -- * Movable
    Movable (..),
    GMovable,
    genericMove,
  )
where

import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Semigroup as Semigroup
import Data.Unrestricted.Linear.Internal.Dupable
import Data.Unrestricted.Linear.Internal.Ur
import GHC.Tuple (Solo)
import GHC.Types (Multiplicity (..))
import Generics.Linear
import Prelude.Linear.Generically
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import Prelude (Bool (..), Char, Double, Float, Int, Ordering (..), Word)
import qualified Prelude as Prelude

-- | Use @'Movable' a@ to represent a type which can be used many times even
-- when given linearly. Simple data types such as 'Bool' or @[]@ are 'Movable'.
-- Though, bear in mind that this typically induces a deep copy of the value.
--
-- Formally, @'Movable' a@ is the class of
-- [coalgebras](https://ncatlab.org/nlab/show/coalgebra+over+a+comonad) of the
-- 'Ur' comonad. That is
--
-- * @unur (move x) = x@
-- * @move \@(Ur a) (move \@a x) = fmap (move \@a) $ move \@a x@
--
-- Additionally, a 'Movable' instance must be compatible with its 'Dupable' parent instance. That is:
--
-- * @case move x of {Ur _ -> ()} = consume x@
-- * @case move x of {Ur x -> (x, x)} = dup2 x@
class (Dupable a) => Movable a where
  move :: a %1 -> Ur a

-- -------------
-- Instances

deriving via
  Generically Bool
  instance
    Movable Bool

deriving via
  Generically Char
  instance
    Movable Char

deriving via
  Generically Double
  instance
    Movable Double

deriving via
  Generically Float
  instance
    Movable Float

deriving via
  Generically Int
  instance
    Movable Int

deriving via
  Generically Word
  instance
    Movable Word

deriving via
  Generically Prelude.Ordering
  instance
    Movable Prelude.Ordering

instance Movable () where
  move () = Ur ()

deriving via
  Generically (Solo a)
  instance
    (Movable a) => Movable (Solo a)

deriving via
  Generically (a, b)
  instance
    (Movable a, Movable b) => Movable (a, b)

deriving via
  Generically (a, b, c)
  instance
    (Movable a, Movable b, Movable c) => Movable (a, b, c)

deriving via
  Generically (a, b, c, d)
  instance
    (Movable a, Movable b, Movable c, Movable d) => Movable (a, b, c, d)

deriving via
  Generically (a, b, c, d, e)
  instance
    (Movable a, Movable b, Movable c, Movable d, Movable e) => Movable (a, b, c, d, e)

instance (Movable a) => Movable (Prelude.Maybe a) where
  move (Prelude.Nothing) = Ur Prelude.Nothing
  move (Prelude.Just x) = Data.fmap Prelude.Just (move x)

instance (Movable a, Movable b) => Movable (Prelude.Either a b) where
  move (Prelude.Left a) = Data.fmap Prelude.Left (move a)
  move (Prelude.Right b) = Data.fmap Prelude.Right (move b)

instance (Movable a) => Movable [a] where
  -- The explicit go function lets this specialize.
  move = go
    where
      go :: [a] %1 -> Ur [a]
      go [] = Ur []
      go (a : l) = (:) Data.<$> move a Data.<*> go l

instance (Movable a) => Movable (NonEmpty a) where
  move (x :| xs) = (:|) Data.<$> move x Data.<*> move xs

instance Movable (Ur a) where
  move (Ur a) = Ur (Ur a)

-- Some stock instances
deriving newtype instance (Movable a) => Movable (Semigroup.Sum a)

deriving newtype instance (Movable a) => Movable (Semigroup.Product a)

deriving newtype instance Movable Semigroup.All

deriving newtype instance Movable Semigroup.Any

-- -------------
-- Generic deriving

instance (Generic a, GMovable (Rep a)) => Movable (Generically a) where
  move = Data.fmap (Generically . to) . gmove . from . unGenerically

genericMove :: (Generic a, GMovable (Rep a)) => a %1 -> Ur a
genericMove = Data.fmap to . gmove . from

class (GDupable f) => GMovable f where
  gmove :: f p %1 -> Ur (f p)

instance GMovable V1 where
  gmove = \case {}

instance GMovable U1 where
  gmove U1 = Ur U1

instance (GMovable f, GMovable g) => GMovable (f :+: g) where
  gmove (L1 a) = case gmove a of Ur x -> Ur (L1 x)
  gmove (R1 a) = case gmove a of Ur x -> Ur (R1 x)

instance (GMovable f, GMovable g) => GMovable (f :*: g) where
  gmove (a :*: b) =
    case gmove a of
      Ur x ->
        case gmove b of
          Ur y -> Ur (x :*: y)

instance (Movable c) => GMovable (K1 i c) where
  gmove (K1 c) = lcoerce (move c)

instance (GMovable f) => GMovable (M1 i t f) where
  gmove (M1 a) = lcoerce (gmove a)

instance GMovable (MP1 'Many f) where
  gmove (MP1 x) = Ur (MP1 x)

instance (GMovable f) => GMovable (MP1 'One f) where
  gmove (MP1 a) = case gmove a of Ur x -> Ur (MP1 x)

instance GMovable UChar where
  gmove (UChar c) = Unsafe.toLinear (\x -> Ur (UChar x)) c

instance GMovable UDouble where
  gmove (UDouble c) = Unsafe.toLinear (\x -> Ur (UDouble x)) c

instance GMovable UFloat where
  gmove (UFloat c) = Unsafe.toLinear (\x -> Ur (UFloat x)) c

instance GMovable UInt where
  gmove (UInt c) = Unsafe.toLinear (\x -> Ur (UInt x)) c

instance GMovable UWord where
  gmove (UWord c) = Unsafe.toLinear (\x -> Ur (UWord x)) c
