{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Data.Profunctor.Linear
  ( Profunctor(..)
  , Monoidal(..)
  , Strong(..)
  , Wandering(..)
  , Unrestricting(..)
  , LinearArrow(..), getLA
  , Exchange(..)
  , Market(..), runMarket
  ) where

import qualified Control.Monad.Linear as Control
import Data.Bifunctor.Linear hiding (first, second)
import qualified Data.Bifunctor as Prelude
import Prelude.Linear
import Data.Kind (Type)
import Data.Void
import qualified Prelude
import Control.Arrow (Kleisli(..))

-- TODO: write laws

class Profunctor (arr :: Type -> Type -> Type) where
  {-# MINIMAL dimap | lmap, rmap #-}

  dimap :: (s #-> a) -> (b #-> t) -> a `arr` b -> s `arr` t
  dimap f g x = lmap f (rmap g x)
  {-# INLINE dimap #-}

  lmap :: (s #-> a) -> a `arr` t -> s `arr` t
  lmap f = dimap f id
  {-# INLINE lmap #-}

  rmap :: (b #-> t) -> s `arr` b -> s `arr` t
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
  -- | Equivalently but less efficient in general:
  --
  -- > wander :: Data.Traversable f => a `arr` b -> f a `arr` f b
  wander :: forall s t a b. (forall f. Control.Applicative f => (a #-> f b) -> s #-> f t) -> a `arr` b -> s `arr` t

class Unrestricting arr where
  unrestrict :: a `arr` b -> Ur a `arr` Ur b

---------------
-- Instances --
---------------

newtype LinearArrow a b = LA (a #-> b)
-- | Temporary deconstructor since inference doesn't get it right
getLA :: LinearArrow a b #-> a #-> b
getLA (LA f) = f

instance Profunctor LinearArrow where
  dimap f g (LA h) = LA $ g . h . f

instance Strong (,) () LinearArrow where
  first  (LA f) = LA $ \(a,b) -> (f a, b)
  second (LA g) = LA $ \(a,b) -> (a, g b)

instance Strong Either Void LinearArrow where
  first  (LA f) = LA $ either (Left . f) Right
  second (LA g) = LA $ either Left (Right . g)

instance Monoidal (,) () LinearArrow where
  LA f *** LA g = LA $ \(a,x) -> (f a, g x)
  unit = LA id

instance Monoidal Either Void LinearArrow where
  LA f *** LA g = LA $ bimap f g
  unit = LA $ \case {}

instance Profunctor (->) where
  dimap f g h x = g (h (f x))
instance Strong (,) () (->) where
  first f (x, y) = (f x, y)
instance Strong Either Void (->) where
  first f (Left x) = Left (f x)
  first _ (Right y) = Right y
instance Monoidal (,) () (->) where
  (f *** g) (a,x) = (f a, g x)
  unit () = ()
instance Monoidal Either Void (->) where
  f *** g = Prelude.bimap f g
  unit = \case {}
instance Unrestricting (->) where
  unrestrict f (Ur a) = Ur (f a)

data Exchange a b s t = Exchange (s #-> a) (b #-> t)
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

instance Prelude.Applicative f => Monoidal (,) () (Kleisli f) where
  Kleisli f *** Kleisli g = Kleisli (\(x,y) -> (,) Prelude.<$> f x Prelude.<*> g y)
  unit = Kleisli Prelude.pure

instance Prelude.Functor f => Monoidal Either Void (Kleisli f) where
  Kleisli f *** Kleisli g = Kleisli $ \case
    Left a -> Left Prelude.<$> f a
    Right b -> Right Prelude.<$> g b
  unit = Kleisli $ \case {}

instance Prelude.Functor f => Unrestricting (Kleisli f) where
  unrestrict (Kleisli f) = Kleisli $ \(Ur a) -> Ur Prelude.<$> f a

data Market a b s t = Market (b #-> t) (s #-> Either t a)
runMarket :: Market a b s t #-> (b #-> t, s #-> Either t a)
runMarket (Market f g) = (f, g)

instance Profunctor (Market a b) where
  dimap f g (Market h k) = Market (g . h) (either (Left . g) Right . k . f)

instance Strong Either Void (Market a b) where
  first (Market f g) = Market (Left . f) (either (either (Left . Left) Right . g) (Left . Right))
