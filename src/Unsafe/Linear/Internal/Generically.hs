{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Unsafe.Linear.Internal.Generically where

import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Traversable as Data
import qualified Control.Functor.Linear.Internal.Class as Control
import Data.Unrestricted.Internal.Ur (Ur(..))
import Data.Unrestricted.Internal.Consumable
import Data.Unrestricted.Internal.Dupable
import Data.Unrestricted.Internal.Movable
import Data.Unrestricted.Internal.Instances ()
import GHC.Generics hiding (to, from, to1, from1)
import qualified GHC.Generics as G
import qualified Unsafe.Linear.Internal as Unsafe
import Prelude.Linear.Internal
import Data.V.Linear ()

-- | A datatype whose instances are defined generically, using the
-- 'Generic' representation. 'Generically1' is a higher-kinded version
-- of 'Generically' that uses 'Generic1'.
--
-- Generic instances can be derived via @'Generically' A@ using
-- @-XDerivingVia@.
newtype Generically a = Generically a

unGenerically :: Generically a %1-> a
unGenerically (Generically a) = a

to :: Generic a => Rep a p %1-> a
to = Unsafe.toLinear G.to

from :: Generic a => a %1-> Rep a p
from = Unsafe.toLinear G.from

instance (Generic a, GConsumable (Rep a)) => Consumable (Generically a) where
  consume = gconsume . from . unGenerically

instance (Generic a, GDupable (Rep a)) => Dupable (Generically a) where
  dup2 (Generically a) = gdup2 (from a) & \case (r1, r2) -> (Generically (to r1), Generically (to r2))

instance (Generic a, GMovable (Rep a)) => Movable (Generically a) where
  move = Data.fmap (Generically . to) . gmove . from . unGenerically

-- | A type whose instances are defined generically, using the
-- 'Generic1' representation. 'Generically1' is a higher-kinded
-- version of 'Generically' that uses 'Generic'.
--
-- Generic instances can be derived for type constructors via
-- @'Generically1' F@ using @-XDerivingVia@.
newtype Generically1 f a = Generically1 (f a)

unGenerically1 :: Generically1 f a %1-> f a
unGenerically1 (Generically1 fa) = fa

to1 :: Generic1 f => Rep1 f p %1-> f p
to1 = Unsafe.toLinear G.to1

from1 :: Generic1 f => f p %1-> Rep1 f p
from1 = Unsafe.toLinear G.from1

instance (Generic1 f, Data.Functor (Rep1 f)) => Data.Functor (Generically1 f) where
  fmap f = Generically1 . to1 . Data.fmap f . from1 . unGenerically1

instance (Generic1 f, Control.Functor (Rep1 f)) => Control.Functor (Generically1 f) where
  fmap f = Generically1 . to1 . Control.fmap f . from1 . unGenerically1

genericTraverse
  :: (Generic1 t, Data.Traversable (Rep1 t), Control.Applicative f)
  => (a %1-> f b) -> t a %1-> f (t b)
genericTraverse f = Control.fmap to1 . Data.traverse f . from1


class GConsumable f where
  gconsume :: f p %1-> ()
instance GConsumable V1 where
  gconsume = \case
instance GConsumable U1 where
  gconsume U1 = ()
instance (GConsumable f, GConsumable g) => GConsumable (f :+: g) where
  gconsume (L1 a) = gconsume a
  gconsume (R1 a) = gconsume a
instance (GConsumable f, GConsumable g) => GConsumable (f :*: g) where
  gconsume (a :*: b) = gconsume a `seqUnit` gconsume b
instance Consumable c => GConsumable (K1 i c) where
  gconsume (K1 c) = consume c
instance GConsumable f => GConsumable (M1 i t f) where
  gconsume (M1 a) = gconsume a

class GConsumable f => GDupable f where
  gdup2 :: f p %1-> (f p, f p)
instance GDupable V1 where
  gdup2 = \case
instance GDupable U1 where
  gdup2 U1 = (U1, U1)
instance (GDupable f, GDupable g) => GDupable (f :+: g) where
  gdup2 (L1 a) = gdup2 a & \case (x, y) -> (L1 x, L1 y)
  gdup2 (R1 a) = gdup2 a & \case (x, y) -> (R1 x, R1 y)
instance (GDupable f, GDupable g) => GDupable (f :*: g) where
  gdup2 (a :*: b) = gdup2 a & \case
    (a1, a2) -> gdup2 b & \case
      (b1, b2) -> (a1 :*: b1, a2 :*: b2)
instance Dupable c => GDupable (K1 i c) where
  gdup2 (K1 c) = dup2 c & \case (x, y) -> (K1 x, K1 y)
instance GDupable f => GDupable (M1 i t f) where
  gdup2 (M1 a) = gdup2 a & \case (x, y) -> (M1 x, M1 y)

class GDupable f => GMovable f where
  gmove :: f p %1-> Ur (f p)
instance GMovable V1 where
  gmove = \case
instance GMovable U1 where
  gmove U1 = Ur U1
instance (GMovable f, GMovable g) => GMovable (f :+: g) where
  gmove (L1 a) = gmove a & \case (Ur x) -> Ur (L1 x)
  gmove (R1 a) = gmove a & \case (Ur x) -> Ur (R1 x)
instance (GMovable f, GMovable g) => GMovable (f :*: g) where
  gmove (a :*: b) = gmove a & \case
    (Ur x) -> gmove b & \case
      (Ur y) -> Ur (x :*: y)
instance Movable c => GMovable (K1 i c) where
  gmove (K1 c) = move c & \case (Ur x) -> Ur (K1 x)
instance GMovable f => GMovable (M1 i t f) where
  gmove (M1 a) = gmove a & \case (Ur x) -> Ur (M1 x)
