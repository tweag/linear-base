{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Data.Profunctor.Linear
  ( Profunctor(..)
  , Monoidal(..)
  , Strong(..)
  , Wandering(..)
  , LinearArrow(..), getLA
  , Kleisli(..)
  , LKleisli(..)
  , CoLKleisli(..)
  , Exchange(..)
  ) where

import qualified Control.Monad.Linear as Control
import qualified Data.Functor.Linear as Data
import Data.Bifunctor.Linear hiding (first, second)
import Data.Functor.Const
import Prelude.Linear
import Data.Void
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

class (Strong (,) () arr, Strong Either Void arr) => Wandering arr where
  wander :: Data.Traversable f => a `arr` b -> f a `arr` f b

---------------
-- Instances --
---------------

newtype LinearArrow a b = LA (a ->. b)
-- | Temporary deconstructor since inference doesn't get it right
-- TODO: maybe use TH to automatically write things like this?
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

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Prelude.Functor f => Profunctor (Kleisli f) where
  dimap f g (Kleisli h) = Kleisli (\x -> forget g Prelude.<$> h (f x))

instance Prelude.Functor f => Strong (,) () (Kleisli f) where
  first  (Kleisli f) = Kleisli (\(a,b) -> (,b) Prelude.<$> f a)
  second (Kleisli g) = Kleisli (\(a,b) -> (a,) Prelude.<$> g b)

instance Prelude.Applicative f => Strong Either Void (Kleisli f) where
  first  (Kleisli f) = Kleisli $ \case
                                   Left  x -> Prelude.fmap Left (f x)
                                   Right y -> Prelude.pure (Right y)


newtype LKleisli m a b = LKleisli { runLKleisli :: a ->. m b }

instance Data.Functor f => Profunctor (LKleisli f) where
  dimap f g (LKleisli h) = LKleisli (Data.fmap g . h . f)

instance Control.Functor f => Strong (,) () (LKleisli f) where
  first  (LKleisli f) = LKleisli (\(a,b) -> (,b) Control.<$> f a)
  second (LKleisli g) = LKleisli (\(a,b) -> (a,) Control.<$> g b)

instance Control.Applicative f => Strong Either Void (LKleisli f) where
  first  (LKleisli f) = LKleisli (either (Data.fmap Left . f) (Control.pure . Right))
  second (LKleisli g) = LKleisli (either (Control.pure . Left) (Data.fmap Right . g))

instance Control.Applicative f => Wandering (LKleisli f) where
  wander (LKleisli f) = LKleisli (Data.traverse f)

instance Profunctor (->) where
  dimap f g h x = g (h (f x))
instance Strong (,) () (->) where
  first f (x, y) = (f x, y)
instance Strong Either Void (->) where
  first f (Left x) = Left (f x)
  first _ (Right y) = Right y

-- XXX: Since CoLKleisli has uses, it might be better to replace all this
-- with a Bif-like structure...
--
newtype CoLKleisli w a b = CoLKleisli { runCoLKleisli :: w a ->. b }

instance Data.Functor f => Profunctor (CoLKleisli f) where
  dimap f g (CoLKleisli h) = CoLKleisli (g . h . Data.fmap f)

-- instance of a more general idea, but this will do for now
instance Strong Either Void (CoLKleisli (Const x)) where
  first (CoLKleisli f) = CoLKleisli (\(Const x) -> Left (f (Const x)))

data Exchange a b s t = Exchange (s ->. a) (b ->. t)
instance Profunctor (Exchange a b) where
  dimap f g (Exchange p q) = Exchange (p . f) (g . q)

