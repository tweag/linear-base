{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Profunctor.Linear
  ( Profunctor(..)
  , Monoidal(..)
  , Strong(..)
  , PWandering(..)
  , DWandering(..)
  , Traversing
  , LinearArrow(..), getLA
  , Exchange(..)
  , Market(..), runMarket
  , MyFunctor(..), runMyFunctor
  , OtherFunctor(..), runOtherFunctor
  ) where

import Control.Arrow (Kleisli(..))
import qualified Control.Monad.Linear as Control
import Data.Bifunctor.Linear hiding (first, second)
import Data.Functor.Const
import qualified Data.Functor.Linear as Data
import Data.Monoid.Linear
import Data.Void
import Prelude.Linear
import qualified Prelude

-- TODO: write laws

class Profunctor (arr :: * -> * -> *) where
  {-# MINIMAL dimap | lmap, rmap #-}

  dimap :: (s ->. a) -> (b ->. t) -> a `arr` b -> s `arr` t
  dimap f g x = lmap f (rmap g x)
  {-# INLINE dimap #-}

  lmap :: (s ->. a) -> a `arr` t -> s `arr` t
  lmap f = dimap f id
  {-# INLINE lmap #-}

  rmap :: (b ->. t) -> s `arr` b -> s `arr` t
  rmap = dimap id
  {-# INLINE rmap #-}

class (SymmetricMonoidal m u, Profunctor arr) => Monoidal m u arr where
  (***) :: a `arr` b -> x `arr` y -> (a `m` x) `arr` (b `m` y)
  unit :: u `arr` u

class (SymmetricMonoidal m u, Profunctor arr) => Strong m u arr where
  {-# MINIMAL first | second #-}

  first :: a `arr` b -> (a `m` c) `arr` (b `m` c)
  first arr = dimap swap swap (second arr)
  {-# INLINE first #-}

  second :: b `arr` c -> (a `m` b) `arr` (a `m` c)
  second arr = dimap swap swap (first arr)
  {-# INLINE second #-}

-- XXX: Just as Prelude.Functor/Data.Functor will combine into
-- > `class Functor (p :: Multiplicity) f`
-- so will Traversable, and then we would instead write
-- > class (...) => Wandering (p :: Multiplicity) arr where
-- >   wander :: Traversable p f => a `arr` b -> f a `arr` f b
-- For now, however, we cannot do this, so we use two classes instead:
-- PreludeWandering and DataWandering
class (Strong (,) () arr, Strong Either Void arr) => PWandering arr where
  pwander :: Prelude.Traversable f => a `arr` b -> f a `arr` f b
class (Strong (,) () arr, Strong Either Void arr) => DWandering arr where
  dwander :: Data.Traversable f => a `arr` b -> f a `arr` f b

class (Strong (,) () arr, Strong Either Void arr, Monoidal (,) () arr) => Traversing arr where

---------------
-- Instances --
---------------

newtype LinearArrow a b = LA (a ->. b)
-- | Temporary deconstructor since inference doesn't get it right
getLA :: LinearArrow a b ->. a ->. b
getLA (LA f) = f

instance Profunctor LinearArrow where
  dimap f g (LA h) = LA $ g . h . f

instance Strong (,) () LinearArrow where
  first  (LA f) = LA $ \(a,b) -> (f a, b)
  second (LA g) = LA $ \(a,b) -> (a, g b)

instance Strong Either Void LinearArrow where
  first  (LA f) = LA $ either (Left . f) Right
  second (LA g) = LA $ either Left (Right . g)

instance DWandering LinearArrow where
  dwander (LA f) = LA (Data.fmap f)

instance Monoidal (,) () LinearArrow where
  LA f *** LA g = LA $ \(a,x) -> (f a, g x)
  unit = LA id

instance Traversing LinearArrow

instance Profunctor (->) where
  dimap f g h x = g (h (f x))
instance Strong (,) () (->) where
  first f (x, y) = (f x, y)
instance Strong Either Void (->) where
  first f (Left x) = Left (f x)
  first _ (Right y) = Right y
instance PWandering (->) where
  pwander = Prelude.fmap
instance Monoidal (,) () (->) where
  (f *** g) (a,x) = (f a, g x)
  unit () = ()
instance Traversing (->)

data Exchange a b s t = Exchange (s ->. a) (b ->. t)
instance Profunctor (Exchange a b) where
  dimap f g (Exchange p q) = Exchange (p . f) (g . q)

instance Prelude.Functor f => Profunctor (Kleisli f) where
  dimap f g (Kleisli h) = Kleisli (\x -> forget g Prelude.<$> h (f x))

instance Prelude.Functor f => Strong (,) () (Kleisli f) where
  first  (Kleisli f) = Kleisli (\(a,b) -> (,b) Prelude.<$> f a)
  second (Kleisli g) = Kleisli (\(a,b) -> (a,) Prelude.<$> g b)

instance Prelude.Applicative f => Strong Either Void (Kleisli f) where
  first  (Kleisli f) = Kleisli $ \case
                                   Left  x -> Prelude.fmap Left (f x)
                                   Right y -> Prelude.pure (Right y)

instance Prelude.Applicative f => PWandering (Kleisli f) where
  pwander (Kleisli f) = Kleisli (Prelude.traverse f)

instance Prelude.Applicative f => Monoidal (,) () (Kleisli f) where
  Kleisli f *** Kleisli g = Kleisli (\(x,y) -> (,) Prelude.<$> f x Prelude.<*> g y)
  unit = Kleisli Prelude.pure

instance Prelude.Applicative f => Traversing (Kleisli f) where

data Market a b s t = Market (b ->. t) (s ->. Either t a)
runMarket :: Market a b s t ->. (b ->. t, s ->. Either t a)
runMarket (Market f g) = (f, g)

instance Profunctor (Market a b) where
  dimap f g (Market h k) = Market (g . h) (either (Left . g) Right . k . f)

instance Strong Either Void (Market a b) where
  first (Market f g) = Market (Left . f) (either (either (Left . Left) Right . g) (Left . Right))

instance Control.Functor (Const (Top, a)) where
  fmap f (Const (t, x)) = Const (throw f <> t, x)
instance Monoid a => Control.Applicative (Const (Top, a)) where
  pure x = Const (throw x, mempty)
  Const x <*> Const y = Const (x <> y)

-- TODO: pick a more sensible name for this
newtype MyFunctor a b t = MyFunctor (b ->. (a, t))
runMyFunctor :: MyFunctor a b t ->. b ->. (a, t)
runMyFunctor (MyFunctor f) = f

instance Data.Functor (MyFunctor a b) where
  fmap f (MyFunctor g) = MyFunctor (getLA (second (LA f)) . g)
instance Control.Functor (MyFunctor a b) where
  fmap f (MyFunctor g) = MyFunctor (Control.fmap f . g)

newtype OtherFunctor a b t = OtherFunctor (a, b ->. t)
runOtherFunctor :: OtherFunctor a b t ->. (a, b ->. t)
runOtherFunctor (OtherFunctor f) = f
instance Data.Functor (OtherFunctor a b) where
  fmap f (OtherFunctor (a,g)) = OtherFunctor (a,f . g)
instance Control.Functor (OtherFunctor a b) where
  fmap f (OtherFunctor (a,g)) = OtherFunctor (a,f . g)
