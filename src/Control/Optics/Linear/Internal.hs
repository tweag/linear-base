{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides linear optics.
--
-- == Overview
--
-- /Some familiarity with optics is needed to understand linear optics.
-- Please go through the (hopefully friendly!) background material section
-- if you are unfamiliar with lenses, prisms, traversals or isomorphisms./
--
-- This documentation provides
--
--  * a high-level idea of
--     * what linear optics are
--     * how to think about linear optics
--     * how these relate to the standard optic definitions in @lens@
--  * examples of using these optics
--
-- == Conceptualizing and using optics
--
-- === What are /linear/ optics?
--
-- **Optics can be conceptualized as a first class object with which you can
-- view and map functions over sub-structure(s) within a larger structure.**
--
-- __/Linear/ optics are optics where the \"viewing\" and \"mapping\" are done
-- with linear functions (and any corresponding structures hold values
-- linearly, i.e., with constructors that use linear arrows).__
--
-- In types: a (linear) optic of type @Optic a b s t@ is a way of viewing the
-- sub-structure(s) of type @a@ in the structure of type @s@ and mapping a
-- function from an @a@ to a @b@ on those sub-structures in @s@ which change an
-- @s@ to a @t@. The non-polymorphic version of the optic is specialized
-- to the types @Optic a a s s@ and is usually defined with a tick mark,
-- e.g., the non-polymorphic @Lens@ is @Lens'@.
--
-- There are four basic optics: traversals, lenses, prisms and isomorphisms:
--
-- ==== Sub-typing diagram of optics
--
-- @
-- {-
--        Traversal
--          ^    ^
--         /      \
--        /        \
--     Lens       Prism
--       ^         ^
--        \       /
--         \     /
--           Iso
-- -}
-- @
--
-- In the diagram above, the arrows @X --> Y@ mean any of the following
-- equivalent things:
--
--  * X is a specialization of Y
--  * X is a strict subset of Y
--  * You can (basically) implement @f :: X -> Y@ with @f = id@
--  but you can't implement @f :: Y -> X@.
--
-- === How can each optic be used? How do I think about them?
--
-- * A @Traversal a b s t@ is roughly equivalent to a
--  @(s #-> (Tree a,x), (Tree b,x) #-> t)@ for some type @x@.
--  It provides a means of accessing several @a@s organized in
--  some structural way in an @s@,
--  and a means of changing them to @b@s to create a @t@.
--  In very ordinary language, it's like walking or traversing the
--  data structure, going across cases and inside definitions.
--
-- * A @Lens a b s t@ is roughly equivalent to a
--  @(s #-> (a,x), (b,x) #-> t)@ for some type @x@. It's a traversal
--  on exactly one @a@ in a @s@.
--
-- * A @Prism a b s t@ is roughly equivalent to a
--  @(s #-> Either a x, Either b x #-> t)@ for some sum type @x@.
--  It's focusing on one branch or case that a sum type could be.
--
-- * An @Iso a b s t@ is equivalent to a @(s #-> a, b #-> t)@.  In the simple
--  case of an @Iso' a s@, this is equivalent to inverse functions
--  @(s #-> a, a #-> s)@.  In the general case an @Iso a b s t@ means if you
--  have the isomorphisms @(a #-> b, b #-> a)@ and @(s #-> t, t #-> a)@, then
--  you can use @(s #-> a, b #-> t)@ to form isomorphisms between @s@, @t@,
--  @a@ and @b@.
--
--
-- == Examples
--
-- === 'Lens' Examples
-- TODO
--
-- === 'Traversal' Examples
-- TODO
--
-- === 'Prism' Examples
-- TODO
--
-- === 'Iso' Examples
-- TODO
--
-- == Background Material
--
--  * [A great intro-to-lenses talk by Simon Peyton Jones](https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation)
--  * [A nice introductory blog post](https://tech.fpcomplete.com/haskell/tutorial/lens)
--  * [A friendly introduction to prisms and isos](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
--  * [The wiki of the @lens@ package](https://github.com/ekmett/lens/wiki)
--  that contains some nice examples
--
-- === Background on implementation
--
-- * Many ideas are drawn from
-- [this paper](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/proyo.pdf)
--
--
-- == Appendix: How do the types make sense?
--
-- === What are the optic types and why?
--
-- All optics are functions, called __optic functions__ of the form
--
-- > forall arr. (a `arr` b) -> (s `arr` t)
--
-- where @arr@ is some specific kind of arrow. Note that is is a rank-2 type
-- (which just means that there is a @forall@ nested inside a @forall@).
--
-- The type of each optic suffices to write functions that go from the optics
-- in this module to the standard optic types in the
-- [lens package](https://hackage.haskell.org/package/lens).
--
-- === How do these types imply the sub-typing diagram above?
--
-- Let's look at a simple example and then describe the core principle
-- that explains the sub-typing diagram above.
--
-- The example: any @Iso a b s t@ is a @Lens a b s t@.
-- The types are essentially these:
--
-- > type Iso a b s t  = forall p. Profunctor p      => a `p` b -> s `p` t
-- > type Lens a b s t = forall p. (Strong (,) () p) => a `p` b -> s `p` t
--
-- By the definition of @Strong@ in "Data.Profunctor.Linear", 
-- any @p@ with a @Strong m u p@ instance also has a @Profunctor p@ instance.
--
-- Hence, we can write
--
-- > convert :: Iso a b s t -> Lens a b s t
-- > convert iso a_p_b = iso a_p_b
--
-- This works because these are rank-2 types. The @iso@ is an optic function
-- that transforms any profunctor computation of type 
-- @Profunctor p => a `p` b@.  The second argument to @convert@ is @a_p_b@
-- which is a computation of type @Strong (,) () p => a `p` b@. However, since
-- the @Strong@ constraint implies @Profunctor@ constraint, we can give @a_p_b@
-- to @iso@. We could not write @convert@ with the types going the other way.
--
-- More generally, since isomorphisms are functions that work on all
-- profunctors, they are also functions that work on all profunctors
-- with additional constraints. Moreover, there are more optic functions that
-- work on all profunctors with that @Strong@ constraint since the assumption
-- of further constraints allows other optic functions to be written. This is
-- analogus to how in abstract algebra, a theorem on groups is still a theorem
-- about abelian groups and in fact, the theorems that hold on abelian groups
-- are a superset of the theorems that hold on just groups.
--
-- __Please take care to understand the principle at play:__
--
-- /If @D@ is a stricter constraint than @C@ (i.e., any @D@ instance has a @C@
-- instance), then the rank-2 type @forall x. C x => f@ is a sub-type of
-- @forall x. D x => f@/
--
-- === What do these optic types /mean/?
--
-- This is quite complicated. With simple types like a @Maybe a@ or
-- @Monad m => a -> m b@, we can develop a sense of what the type means
-- abstractly. This is much harder with optics because of all the type classes
-- and the rank-2 types.
--
-- Even the classic lens type, for instance, is not easily grasped abtractly:
--
-- > type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)
--
-- So, suffice it to say that the types of linear optics are simply generic
-- enough to making the interface we want, i.e., providing a first class object
-- with which we can map over sub-structure(s) within a structure and view
-- sub-structure(s) within a structure.
--
-- However, due to the fact that they are so general, they have several other
-- unforseen use cases.
module Control.Optics.Linear.Internal
  ( -- * Definitions of Optics
    Optic_(..)
  , Optic
  , Iso, Iso'
  , Lens, Lens'
  , Prism, Prism'
  , Traversal, Traversal'
    -- * Constructing optics
  , iso, prism
    -- * Composing optics
  , (.>)
    -- * Using standard optics
  , swap, assoc
  , _1, _2
  , _Left, _Right
  , _Just, _Nothing
  , traversed
    -- * Consuming optics
  , get, set, gets
  , match, build
  , over, over'
  , traverseOf, traverseOf'
  , lengthOf
  , withIso, withPrism
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


-- # Data Type Definitions
-------------------------------------------------------------------------------

-- | A wrapper for an Optic that uses a "Data.Profunctor.Linear" @arr@
newtype Optic_ arr a b s t = Optical (a `arr` b -> s `arr` t)

-- | @Optic c a b s t@ is a first class object that allows
-- viewing and mapping over sub-structures of type @a@ in a type @s@
-- such that replacing all @a@s with @b@s yields a new structure
-- of type @t@. The `arr` is something that behaves like a function
-- (specificially, it is a 'Profunctor') and the types is basically:
--
-- > Optic c a b s t = forall arr. c arr => a `arr` b -> s `arr` t
--
-- Note that it is a rank-2 type.
type Optic c a b s t =
  forall arr. c arr => Optic_ arr a b s t

-- | An isomorphism between @s@ and @t@ represents the fact that
-- except for changing each sub-structure of type @a@ into type @b@,
-- these structures are algebraically the same.
--
-- For instance, @Either () a@ and @Maybe a@ are isomorphic.
--
-- See [the wiki](https://en.wikipedia.org/wiki/Isomorphism).
type Iso a b s t = Optic Profunctor a b s t
type Iso' a s = Iso a a s s

-- | A lens is something that focuses on a single sub-structure
-- of type @a@ in the structure of product type @s@.
--
-- Essentially, there exists some @x@ such that
-- @s@ is isomorphic to @(a,x)@. A lens provides
-- a way to get the @a@ and map over it to make a
-- @(b,x)@ which is isomorphic to @t@
type Lens a b s t = Optic (Strong (,) ()) a b s t
type Lens' a s = Lens a a s s

-- | A prism is something that focuses on a branch of a sum type @s@.
--
-- Essentially, there exists some x such that @s@ is isomorphic to
-- @Either a x@. A prism provides a way to access the @a@ (if it exists)
-- and mapping to create a @Either b x@ which is isomorphic to a @t@.
type Prism a b s t = Optic (Strong Either Void) a b s t
type Prism' a s = Prism a a s s

-- | A traversal is something that focuses on several sub-structures of
-- a product type.
--
-- Essentially, for some structure @s@ there is some type @x@ such that
-- @s@ is isomorphic to @([a], x)@. A traversal provides a way to get out
-- the list of type @[a]@ and to map over @s@ to create a structure of type
-- @t@ which is isomorphic to @([b], x)@.
type Traversal a b s t = Optic Wandering a b s t
type Traversal' a s = Traversal a a s s

-- # Composing Optics
-------------------------------------------------------------------------------

(.>) :: Optic_ arr a b s t -> Optic_ arr x y a b -> Optic_ arr x y s t
Optical f .> Optical g = Optical (f P.. g)

-- # Common Optics
-------------------------------------------------------------------------------

-- | An isomorphism of the symmetry symmetric monoidal bifunctors.
-- A simple example: for the symmetric monoidal of a tuple @(,)@,
-- with a simple @Iso'@, this is of type
--
-- > swap :: Iso' (a,b) (b,a)
--
-- which is clearly the case. These two types are isomorphic.
swap :: SymmetricMonoidal m u => Iso (a `m` b) (c `m` d) (b `m` a) (d `m` c)
swap = iso Bifunctor.swap Bifunctor.swap

-- | An isomorphism of the associativity of a symmetric monoidal bifunctors.
-- A simple example: for the symmetric monoidal of a tuple @(,)@,
-- with a simple @Iso'@, this is of type
--
-- > assoc :: Iso' ((a,b),c)) (a,(b,c))
--
-- and these two types are surely isomorphic.
assoc :: SymmetricMonoidal m u => Iso ((a `m` b) `m` c) ((d `m` e) `m` f) (a `m` (b `m` c)) (d `m` (e `m` f))
assoc = iso Bifunctor.lassoc Bifunctor.rassoc

-- | Lens for the first element of a tuple
_1 :: Lens a b (a,c) (b,c)
_1 = Optical first

-- | Lens for the second element of a tuple
_2 :: Lens a b (c,a) (c,b)
_2 = Optical second

-- | Prism for viewing and mapping over the
-- left case of an either
_Left :: Prism a b (Either a c) (Either b c)
_Left = Optical first

-- | Prism for viewing and mapping over the
-- right case of an either
_Right :: Prism a b (Either c a) (Either c b)
_Right = Optical second

-- | Prism for viewing and mapping over the
-- just case of a maybe
_Just :: Prism a b (Maybe a) (Maybe b)
_Just = prism Just (maybe (Left Nothing) Right)

-- | Prism for the viewing and mapping over
-- the nothing case of a maybe
_Nothing :: Prism' () (Maybe a)
_Nothing = prism (\() -> Nothing) Left

-- | The default traversal of a traversible of type @t@.
-- Mapping over and getting all @a@s is done via
-- the @Traversable@ instance.
traversed :: Traversable t => Traversal a b (t a) (t b)
traversed = Optical wander

-- #  Using Optics
-------------------------------------------------------------------------------

-- | Use a lens or prism to map a (linear) function over a structure
over :: Optic_ LinearArrow a b s t -> (a #-> b) -> s #-> t
over (Optical l) f = getLA (l (LA f))

-- | Using a traversal, map actions over each sub-structure of type @a@ in a
-- @s@ and create an action that produces a @t@
traverseOf :: Optic_ (Linear.Kleisli f) a b s t -> (a #-> f b) -> s #-> f t
traverseOf (Optical l) f = Linear.runKleisli (l (Linear.Kleisli f))

-- | Using a lens, get or view a sub-structure of type @a@ in a structure
-- of type @s@ with a lens of type @Lens s a@.
get :: Optic_ (NonLinear.Kleisli (Const a)) a b s t -> s -> a
get l = gets l P.id

-- | Like 'get' but applies a function of type @a -> r@ before returning
gets :: Optic_ (NonLinear.Kleisli (Const r)) a b s t -> (a -> r) -> s -> r
gets (Optical l) f s = getConst' (NonLinear.runKleisli (l (NonLinear.Kleisli (Const P.. f))) s)

-- | Using a lens, set the sub-structure of type @a@ in an @s@ to something of
-- type @b@ that transforms the structure into a @t@.
set :: Optic_ (->) a b s t -> b -> s -> t
set (Optical l) x = l (const x)

-- | Using a prism, break down a sum-type @s@ into cases
match :: Optic_ (Market a b) a b s t -> s #-> Either t a
match (Optical l) = snd (runMarket (l (Market id Right)))

-- | Using a prism, build a value of a sum-type @t@ with one branch of type @b@
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

-- | The same as 'over' but for normal non-linear arrows @(->)@
over' :: Optic_ (->) a b s t -> (a -> b) -> s -> t
over' (Optical l) f = l f

-- | The same as 'traverseOf' but for non-linear Kleisi arrows
traverseOf' :: Optic_ (NonLinear.Kleisli f) a b s t -> (a -> f b) -> s -> f t
traverseOf' (Optical l) f = NonLinear.runKleisli (l (NonLinear.Kleisli f))

withIso :: Optic_ (Exchange a b) a b s t -> ((s #-> a) -> (b #-> t) -> r) -> r
withIso (Optical l) f = f fro to
  where Exchange fro to = l (Exchange id id)

withPrism :: Optic_ (Market a b) a b s t -> ((b #-> t) -> (s #-> Either t a) -> r) -> r
withPrism (Optical l) f = f b m
  where Market b m = l (Market id Right)

-- # Constructing Optics
-------------------------------------------------------------------------------

-- | @iso convertForth convertBack@ constructs an isomorphism
-- given functions to convert back and forth between @s@ and @t@.
iso :: (s #-> a) -> (b #-> t) -> Iso a b s t
iso f g = Optical (dimap f g)

-- | @prism builder takeCase@ constructs a prism given a @builder@
-- which goes from a @b@ (produced by applying a function of type
-- @a -> b@ to the @a@ case of the sum type @s@) to a @t@ and a
-- @takeCase@ which breaks @s@ down into either the @a@ case or the
-- transformed structure of type @t@.
prism :: (b #-> t) -> (s #-> Either t a) -> Prism a b s t
prism b s = Optical $ \f -> dimap s (either id id) (second (rmap b f))

