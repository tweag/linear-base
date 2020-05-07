{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides linear optics.
--
--
--
-- TODO: most of this stuff hasn't yet been translated to work for 'Strong'
-- and 'Wandering' constraints. I'll document that first, and then document this.
--
--
--
--
--
-- The documentation below provides an overview of the optics provided in this
-- module and examples of how to use them.
--
-- **Please access this background material if you are unfamiliar with lens and
-- optics.**
--
-- == Helpful Background Material
--
--  * [A great intro to lens talk by Simon Peyton Jones](https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation)
--  * [A nice introductory blog post](https://tech.fpcomplete.com/haskell/tutorial/lens)
--  * [The wiki of the @lens@ package](https://github.com/ekmett/lens/wiki)
--  that contains some nice examples
--
-- == What are the optics in this file?
--
-- === Overview
--
-- An optic is basically a getter and a mapper into some type tradionally
-- called @s@. Getters and mappers are basically functions with these types:
--
-- > getter :: s -> a
-- > mapper :: (a -> a) -> s -> s
--
-- If we want to be able to modify some of the @a@s by applying a function of type
-- @a -> b@ inside some @s@ to make a @t@, we want these types:
--
-- > getter :: t -> b
-- > mapper :: (a -> b) -> s -> t
--
--
-- == What optics are in this module?
--
-- There are four optics provided in this module.
--
-- Here's a basic diagram
--
--        Traversal
--          ^    ^
--         /      \
--        /        \
--     Lens       Prism
--       ^         ^
--        \       /
--         \     /
--           Iso
--
-- The upward arrow means "is a specialization of" or "is a strict subset of".
-- So, an @Iso@ is a speciailization of a @Prism@, and any @Iso@ is a @Prism@.
-- On the other hand, there are some @Prism@s that are not @Iso@s.
--
-- === At a high level, what is each optic useful for?
--
--
-- === How do the types work out?
--
-- All optics are functions, called __optic functions__ of the type
--
-- > forall arr. (a `arr` b) -> (s `arr` t)
--
-- where @arr@ is some specific kind of arrow. Note that is is a rank-2 type
-- (which just means that there is a @forall@ nested inside a @forall@).
--
--
-- * 'Traversal's are optics where @a `arr` b@ is @a -> f b@ for any
--   @Applicative f@
-- * 'Lens'es are optics where @a `arr` b@ is @a -> f b@ for any @Functor f@
-- * 'Iso's are optics where @a `arr` b@ is @a `p` f b@ for any
--    @Profunctor p@ and @Functor f@
-- * 'Prism's are optics where @a `arr` b@ is @a `c` f b@ for any
--   @Choice c@ and @Applicative f@
--
-- ==== How do these specialize?
--
-- Any @Lens@ is a @Traversal@. A @Lens@ is a function that can be applied
-- for /any/ functor. A @Traversal@ is a function that can be applied for /any/
-- applicative functor. (Recall that these are rank-2 types.) An applicative
-- functor is a specific kind of functor. Hence, if I have a function that can
-- be applied for any functor, and I provide arguments that feature an
-- applicative functor, that works. Put another way, a @Lens@ is a more generic
-- function than a @Traversal@ since a @Traversal@ has a stricter constraint,
-- i.e., @Applicative@ is stricter (and includes) @Functor@.  
-- * Any @Prism@ is a @Traversal@. A @Prism@ has a stricter condition: the
-- optic function must work for /any/ @Choice c@. A traversal's optic function
-- works for the function arrow @->@, which is an instance of @Choice@.
--
--
-- Please take care to notice and understand the principle at play:
--
-- __If @D@ is a stricter constraint than @C@ (@C => D@), then the rank-2 type
-- @forall x. C x => f@ is a special case of @forall x. D x => f@__
--
-- Hence, we create specific kinds of Traversals by relaxing the constraints of
-- Traversals.
--
-- In summary:
--
-- * Lenses are Traversals because Traversal's have a stricter constraint:
-- @Applicative f@ is stricter than @Functor f@
-- * Prisms are Traversals because Traversals have a stricter constraint:
-- The function arrow @a -> f b@ is stricter than using 
-- any @Choice c@ in @a `c` b@.
-- * Isos are Lenses because Isos have a stricter constraint:
-- A @a -> f b@ is stricter than using any @Profunctor p@ in @a `p` f b@.
-- * Isos are Prisms because Isos have a stricter constraint:
-- A @Choice c => Profunctor c@ is a stricter constraint on the @arr@.
--
--
-- == Examples of 'Lens'-es
--
-- == Examples of 'Traversal's
--
-- == Examples of 'Iso'morphisms
--
-- == Examples of 'Prism's
--
--
--
module Control.Optics.Linear.Internal
  ( -- * Types
    Optic_(..)
  , Optic
  , Iso, Iso'
  , Lens, Lens'
  , Prism, Prism'
  , Traversal, Traversal'
    -- * Composing optics
  , (.>)
    -- * Common optics
  , swap, assoc
  , _1, _2
  , _Left, _Right
  , _Just, _Nothing
  , traversed
    -- * Using optics
  , get, set, gets
  , match, build
  , over, over'
  , traverseOf, traverseOf'
  , lengthOf
  , withIso, withPrism
    -- * Constructing optics
  , iso, prism
  )
  where

import qualified Control.Arrow as NonLinear
import qualified Data.Bifunctor.Linear as Bifunctor
import Data.Bifunctor.Linear (SymmetricMonoidal)
import Data.Profunctor.Linear
import Data.Functor.Linear
import qualified Data.Profunctor.Kleisli.Linear as Linear
import Data.Void
import Prelude.Linear
import qualified Prelude as P


-- | A wrapper for an Optic that uses a "Data.Profunctor.Linear" @arr@
newtype Optic_ arr a b s t = Optical (a `arr` b -> s `arr` t)

-- | @Optic c a b s t@ is essentially
-- > forall arr. c arr => (a `arr` b ) -> (s `arr` t)
-- for some @arr@ that is an instance of "Data.Profunctor.Linear".
--
-- An optic is a generic way of lifting a computation on values of type @a@ to
-- values of type @s@. You have some value of type @s@, from which you could
-- access value(s) of type @a@. After performing computations of type
-- @a `arr` b@, you change that value of type @s@ into a value of type @t@.
type Optic c a b s t =
  forall arr. c arr => Optic_ arr a b s t

type Iso a b s t = Optic Profunctor a b s t
type Iso' a s = Iso a a s s
type Lens a b s t = Optic (Strong (,) ()) a b s t
type Lens' a s = Lens a a s s
type Prism a b s t = Optic (Strong Either Void) a b s t
type Prism' a s = Prism a a s s
type Traversal a b s t = Optic Wandering a b s t
type Traversal' a s = Traversal a a s s

swap :: SymmetricMonoidal m u => Iso (a `m` b) (c `m` d) (b `m` a) (d `m` c)
swap = iso Bifunctor.swap Bifunctor.swap

assoc :: SymmetricMonoidal m u => Iso ((a `m` b) `m` c) ((d `m` e) `m` f) (a `m` (b `m` c)) (d `m` (e `m` f))
assoc = iso Bifunctor.lassoc Bifunctor.rassoc

(.>) :: Optic_ arr a b s t -> Optic_ arr x y a b -> Optic_ arr x y s t
Optical f .> Optical g = Optical (f P.. g)

prism :: (b #-> t) -> (s #-> Either t a) -> Prism a b s t
prism b s = Optical $ \f -> dimap s (either id id) (second (rmap b f))

_1 :: Lens a b (a,c) (b,c)
_1 = Optical first

_2 :: Lens a b (c,a) (c,b)
_2 = Optical second

_Left :: Prism a b (Either a c) (Either b c)
_Left = Optical first

_Right :: Prism a b (Either c a) (Either c b)
_Right = Optical second

_Just :: Prism a b (Maybe a) (Maybe b)
_Just = prism Just (maybe (Left Nothing) Right)

_Nothing :: Prism' () (Maybe a)
_Nothing = prism (\() -> Nothing) Left

traversed :: Traversable t => Traversal a b (t a) (t b)
traversed = Optical wander

over :: Optic_ LinearArrow a b s t -> (a #-> b) -> s #-> t
over (Optical l) f = getLA (l (LA f))

traverseOf :: Optic_ (Linear.Kleisli f) a b s t -> (a #-> f b) -> s #-> f t
traverseOf (Optical l) f = Linear.runKleisli (l (Linear.Kleisli f))

get :: Optic_ (NonLinear.Kleisli (Const a)) a b s t -> s -> a
get l = gets l P.id

gets :: Optic_ (NonLinear.Kleisli (Const r)) a b s t -> (a -> r) -> s -> r
gets (Optical l) f s = getConst' (NonLinear.runKleisli (l (NonLinear.Kleisli (Const P.. f))) s)

set :: Optic_ (->) a b s t -> b -> s -> t
set (Optical l) x = l (const x)

match :: Optic_ (Market a b) a b s t -> s #-> Either t a
match (Optical l) = snd (runMarket (l (Market id Right)))

build :: Optic_ (Linear.CoKleisli (Const b)) a b s t -> b #-> t
build (Optical l) x = Linear.runCoKleisli (l (Linear.CoKleisli getConst')) (Const x)

-- XXX: move this to Prelude
-- | Linearly typed patch for the newtype deconstructor. (Temporary until
-- inference will get this from the newtype declaration.)
getConst' :: Const a b #-> a
getConst' (Const x) = x

lengthOf :: MultIdentity r => Optic_ (NonLinear.Kleisli (Const (Adding r))) a b s t -> s -> r
lengthOf l s = getAdded (gets l (const (Adding one)) s)

-- XXX: the below two functions will be made redundant with multiplicity
-- polymorphism on over and traverseOf'
over' :: Optic_ (->) a b s t -> (a -> b) -> s -> t
over' (Optical l) f = l f

traverseOf' :: Optic_ (NonLinear.Kleisli f) a b s t -> (a -> f b) -> s -> f t
traverseOf' (Optical l) f = NonLinear.runKleisli (l (NonLinear.Kleisli f))

iso :: (s #-> a) -> (b #-> t) -> Iso a b s t
iso f g = Optical (dimap f g)

withIso :: Optic_ (Exchange a b) a b s t -> ((s #-> a) -> (b #-> t) -> r) -> r
withIso (Optical l) f = f fro to
  where Exchange fro to = l (Exchange id id)

withPrism :: Optic_ (Market a b) a b s t -> ((b #-> t) -> (s #-> Either t a) -> r) -> r
withPrism (Optical l) f = f b m
  where Market b m = l (Market id Right)
