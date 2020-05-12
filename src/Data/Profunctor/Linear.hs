{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides profunctor classes and instances
--
-- Please import this module qualified.
--
-- == Conventions from "Control.Optics.Linear.Internal"
--
-- Many Profunctors are heavily used throughout optics.
--
-- * @TypeName a b s t@ means we have an optic of some @s@ with sub-structures
-- of type @a@ that if replaced by @b@s yield a @t@.
-- * @Strong@ and @Wandering@ are classes
--
module Data.Profunctor.Linear
  ( Profunctor(..)
  , Monoidal(..)
  , Strong(..)
  , Wandering(..)
  , LinearArrow(..), getLA
  , Exchange(..)
  , Market(..), runMarket
  ) where

import qualified Data.Functor.Linear as Data
import Data.Bifunctor.Linear hiding (first, second)
import Prelude.Linear
import Data.Void
import qualified Prelude
import Control.Arrow (Kleisli(..))


-- | A Profunctor is essentially a computation that involves taking @a@(s)
-- as input and returning @b@(s). These computations compose with
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
class Profunctor (arr :: * -> * -> *) where
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

-- | This typeclass asserts the constraint on the profunctor @arr@ that
-- function-like things of type @a `arr` b@ with certain properties
-- can be extended to work on traversible structures. These are the properties
-- these function-like things need: they need @Strong (,) ()@ and
-- @Strong Either Void@ instances. In other words, they need to be able to form
-- sum and product function-like things with @id@:
--
--  * A sum-like function-thing with @id@ is of type
-- > idOrFn :: (Either a c) `arr` (Either b c)
--  * A product-like function-thing with @id@ is of type
-- > idAndFn :: (a, c) `arr` (b, c)
--
-- This typeclass is used primarily to define traversals in
-- "Control.Optics.Linear.Internal".
class (Strong (,) () arr, Strong Either Void arr) => Wandering arr where
  wander :: Data.Traversable f => a `arr` b -> f a `arr` f b

---------------
-- Instances --
---------------

-- | This newtype is needed to implement 'Profunctor' instances of @#->@.
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

instance Profunctor (->) where
  dimap f g h x = g (h (f x))
instance Strong (,) () (->) where
  first f (x, y) = (f x, y)
instance Strong Either Void (->) where
  first f (Left x) = Left (f x)
  first _ (Right y) = Right y

-- | An exchange is a pair of translation functions that encode an
-- isomorphism; an @Exchange a b s t@ is equivalent to a @Iso a b s t@.
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

-- | A market is a pair of constructor and deconstructor functions that encode
-- a prism; a @Market a b s t@ is equivalent to a @Prism a b s t@.
data Market a b s t = Market (b #-> t) (s #-> Either t a)
runMarket :: Market a b s t #-> (b #-> t, s #-> Either t a)
runMarket (Market f g) = (f, g)

instance Profunctor (Market a b) where
  dimap f g (Market h k) = Market (g . h) (either (Left . g) Right . k . f)

instance Strong Either Void (Market a b) where
  first (Market f g) = Market (Left . f) (either (either (Left . Left) Right . g) (Left . Right))
