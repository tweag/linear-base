{-# LANGUAGE EmptyCase #-}
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

-- | This module provides profunctor classes and instances.
--
-- Please import this module qualified.
--
-- Some of the definitions in this module are heavily connected to and
-- motivated by linear optics. Please see @Control.Optics.Linear@ and other
-- optics modules for motivations for the definitions provided here.
--
-- == Connections to Linear Optics
--
-- * @Strong@ and @Wandering@ are classes drawn from
-- [this paper](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/proyo.pdf)
-- * 'Exchange' and 'Market' are ways of encoding isomorphisms and prisms
--
module Data.Profunctor.Linear
  ( Profunctor(..)
  , Monoidal(..)
  , Strong(..)
  , Wandering(..)
  , Exchange(..)
  , Market(..), runMarket
  ) where

import qualified Control.Functor.Linear as Control
import Data.Bifunctor.Linear hiding (first, second)
import qualified Data.Bifunctor as Prelude
import Data.Functor.Identity
import Prelude.Linear
import Prelude.Linear.Internal (runIdentity')
import Data.Kind (FUN, Type)
import Data.Void
import qualified Prelude
import Control.Arrow (Kleisli(..))
import GHC.Types (Multiplicity(One))


-- | A Profunctor can be thought of as a computation that involves taking
-- @a@(s) as input and returning @b@(s). These computations compose with
-- (linear) functions. Profunctors generalize the function arrow @->@.
--
-- Hence, think of a value of type @x `arr` y@ for profunctor @arr@ to be
-- something like a function from @x@ to @y@.
--
-- Laws:
--
-- > lmap id = id
-- > lmap (f . g) = lmap f . lmap g
-- > rmap id = id
-- > rmap (f . g) = rmap f . rmap g
--
class Profunctor (arr :: Type -> Type -> Type) where
  {-# MINIMAL dimap | lmap, rmap #-}

  dimap :: (s %1-> a) -> (b %1-> t) -> a `arr` b -> s `arr` t
  dimap f g x = lmap f (rmap g x)
  {-# INLINE dimap #-}

  lmap :: (s %1-> a) -> a `arr` t -> s `arr` t
  lmap f = dimap f id
  {-# INLINE lmap #-}

  rmap :: (b %1-> t) -> s `arr` b -> s `arr` t
  rmap = dimap id
  {-# INLINE rmap #-}

-- | A @(Monoidal m u arr)@ is a profunctor @arr@ that can be sequenced
-- with the bifunctor @m@. In rough terms, you can combine two function-like
-- things to one function-like thing that holds both input and output types
-- with the bifunctor @m@.
class (SymmetricMonoidal m u, Profunctor arr) => Monoidal m u arr where
  (***) :: a `arr` b -> x `arr` y -> (a `m` x) `arr` (b `m` y)
  unit :: u `arr` u

-- | A @(Strong m u arr)@ instance means that the function-like thing
-- of type @a `arr` b@ can be extended to pass along a value of type @c@
-- as a constant via the bifunctor of type @m@.
--
-- This typeclass is used primarily to generalize common patterns
-- and instances that are defined when defining optics. The two uses
-- below are used in defining lenses and prisms respectively in
-- "Control.Optics.Linear.Internal":
--
-- If @m@ is the tuple
-- type constructor @(,)@ then we can create a function-like thing
-- of type @(a,c) `arr` (b,c)@ passing along @c@ as a constant.
--
-- If @m@ is @Either@ then we can create a function-like thing of type
-- @Either a c `arr` Either b c@ that either does the original function
-- or behaves like the constant function.
class (SymmetricMonoidal m u, Profunctor arr) => Strong m u arr where
  {-# MINIMAL first | second #-}

  first :: a `arr` b -> (a `m` c) `arr` (b `m` c)
  first arr = dimap swap swap (second arr)
  {-# INLINE first #-}

  second :: b `arr` c -> (a `m` b) `arr` (a `m` c)
  second arr = dimap swap swap (first arr)
  {-# INLINE second #-}

-- | A @Wandering arr@ instance means that there is a @wander@ function
-- which is the traversable generalization of the classic lens function:
--
-- > forall f. Functor f => (a -> f b) -> (s -> f t)
--
-- in our notation:
--
-- > forall arr. (HasKleisliFunctor arr) => (a `arr` b) -> (s `arr` t)
--
-- @wander@ specializes the @Functor@ constraint to a control applicative:
--
-- > forall f. Applicative f => (a -> f b) -> (s -> f t)
-- > forall arr. (HasKleisliApplicative arr) => (a `arr` b) -> (s `arr` t)
--
-- where @HasKleisliFunctor@ or @HasKleisliApplicative@ are some constraints
-- which allow for the @arr@ to be @Kleisli f@ for control functors
-- or applicatives @f@.
--
class (Strong (,) () arr, Strong Either Void arr) => Wandering arr where
  -- | Equivalently but less efficient in general:
  --
  -- > wander :: Data.Traversable f => a `arr` b -> f a `arr` f b
  wander :: forall s t a b. (forall f. Control.Applicative f => (a %1-> f b) -> s %1-> f t) -> a `arr` b -> s `arr` t

---------------
-- Instances --
---------------



instance Profunctor (FUN 'One) where
  dimap f g h = g . h . f

instance Strong (,) () (FUN 'One) where
  first  f (a, b) = (f a, b)
  second g (a, b) = (a, g b)

instance Strong Either Void (FUN 'One) where
  first  f = either (Left . f) Right
  second g = either Left (Right . g)

instance Wandering (FUN 'One) where
  wander f a_to_b s = runIdentity' $ f (Identity . a_to_b) s

instance Monoidal (,) () (FUN 'One) where
  (f *** g) (a,x) = (f a, g x)
  unit = id

instance Monoidal Either Void (FUN 'One) where
  f *** g = bimap f g
  unit = \case {}


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

-- | An exchange is a pair of translation functions that encode an
-- isomorphism; an @Exchange a b s t@ is equivalent to a @Iso a b s t@.
data Exchange a b s t = Exchange (s %1-> a) (b %1-> t)
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

-- | A market is a pair of constructor and deconstructor functions that encode
-- a prism; a @Market a b s t@ is equivalent to a @Prism a b s t@.
data Market a b s t = Market (b %1-> t) (s %1-> Either t a)
runMarket :: Market a b s t %1-> (b %1-> t, s %1-> Either t a)
runMarket (Market f g) = (f, g)

instance Profunctor (Market a b) where
  dimap f g (Market h k) = Market (g . h) (either (Left . g) Right . k . f)

instance Strong Either Void (Market a b) where
  first (Market f g) = Market (Left . f) (either (either (Left . Left) Right . g) (Left . Right))
