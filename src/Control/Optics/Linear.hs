-- | This module provides linear optics.
--
-- Documentation for specific optics (lenses, prisms, traversals and
-- isomorphisms) are provided in their respective modules.
--
-- Here we just provide an overview.
--
-- Some familiarity with optics is needed to understand linear optics.
-- Please go through the (hopefully friendly!) background material section
-- if you are unfamiliar with lenses, prisms, traversals or isomorphisms.
--
-- == Background Material
--
-- If you don't know anything about optics, we suggest looking at the
-- resources below and playing with the @lens@ package.
--
--  * [A great intro-to-lenses talk by Simon Peyton Jones](https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation)
--  * [A nice introductory blog post](https://tech.fpcomplete.com/haskell/tutorial/lens)
--  * [A friendly introduction to prisms and isos](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
--  * [The wiki of the @lens@ package](https://github.com/ekmett/lens/wiki)
--  that contains some nice examples
--
-- == Conceptualizing and using optics
--
-- === What are /linear/ optics?
--
-- Optics can be conceptualized as a first class object with which you can
-- view and map functions over sub-structure(s) within a larger structure.
--
-- __ /Linear/ optics are optics where the \"viewing\" and \"mapping\" are__
-- __done with linear functions (and any corresponding structures hold values__
-- __linearly, i.e., with constructors that use linear arrows).__
--
-- In types: a (linear) optic of type @Optic s t a b@ is a way of viewing the
-- sub-structure(s) of type @a@ in the structure of type @s@ and mapping a
-- function from an @a@ to a @b@ on those sub-structures in @s@ which change an
-- @s@ to a @t@. The non-polymorphic version of the optic is specialized
-- to the types @Optic s s a a@ and is usually defined with a tick mark,
-- e.g., the non-polymorphic @Lens@ is @Lens'@.
--
-- There are four basic optics: traversals, lenses, prisms and isomorphisms.
--
-- === Sub-typing diagram of optics
--
-- \[ \texttt{Traversal} \]
-- \[ \Huge \nearrow ~~~~~ \nwarrow \]
-- \[ \texttt{Lens}\hspace{6em}\texttt{Prism} \]
-- \[ \Huge \nwarrow ~~~~~ \nearrow \]
-- \[ \texttt{Iso} \]
--
-- In the diagram above, the arrows @X --> Y@ mean any of the following
-- equivalent things:
--
--  * X is a specialization of Y
--  * X is a strict subset of Y
--  * You can (basically) implement @f :: X -> Y@ with @f = id@
--  but you can't implement @f :: Y -> X@.
--
-- === A bird's eye view of the types
--
-- The types of linear optics are generalizations of the standard optic
-- types from the @lens@ package.
--
-- These are the standard optic types:
--
-- > type Traversal s t a b =
-- >   forall f. Applicative f => (a -> f b) -> s -> f t
-- > type Lens s t a b =
-- >   forall f. Functor f => (a -> f b) -> (s -> f t)
-- > type Prism s t a b =
-- >   forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
-- > type Iso s t a b =
-- >   forall p f. (Profunctor p, Functor f) => a `p` (f b) -> s `p` (f t)
--
-- These are (basically) the linear optic types:
--
-- > type Traversal a b s t =
-- >   forall arr.  Wandering arr => (a `arr` b) -> (s `arr` t)
-- > type Lens a b s t =
-- >   forall arr. Strong (,) () arr => (a `arr` b) -> (s `arr` t)
-- > type Prism a b s t =
-- >   forall arr. Strong Either Void arr => (a `arr` b) -> (s `arr` t)
-- > type Iso a b s t =
-- >   forall arr. Profunctor arr => (a `arr` b) -> (s `arr` t)
--
-- Below is a table that lists the instances of the typeclasses which
-- generalize the standard optics.
--
-- Note that Kleisli arrows basically defined like so:
--
-- > type Kleisli f a b = a #-> f b
--
-- /Note: We abbreviate Control for Control.Functor.Linear./
--
-- +-----------------+------------+---------------+--------------------+-----------+
-- |                 | Profunctor | Strong (,) () | Strong Either Void | Wandering |
-- +=================+============+===============+====================+===========+
-- |     @(->)@      |     X      |       X       |         X          |           |
-- +-----------------+------------+---------------+--------------------+-----------+
-- |    @(\#->)@     |     X      |       X       |         X          |           |
-- +-----------------+------------+---------------+--------------------+-----------+
-- |    (Prelude)    |            |               |                    |           |
-- |  @Functor f@    |            |               |                    |           |
-- | @=> Kleisli f@  |     X (4)  |       X       |                    |           |
-- +-----------------+------------+---------------+--------------------+-----------+
-- | (Data.Functor)  |            |               |                    |           |
-- |  @Functor f@    |            |               |                    |           |
-- | @=> Kleisli f@  |     X      |               |                    |           |
-- +-----------------+------------+---------------+--------------------+-----------+
-- |    (Prelude)    |            |               |                    |           |
-- | @Applicative f@ |            |               |                    |           |
-- | @=> Kleisli f@  |     X      |       X       |         X   (3)    |           |
-- +-----------------+------------+---------------+--------------------+-----------+
-- |    (Control)    |            |               |                    |           |
-- |   @Functor f@   |            |               |                    |           |
-- | @=> Kleisli f@  |     X      |       X (2)   |                    |           |
-- +-----------------+------------+---------------+--------------------+-----------+
-- |    (Control)    |            |               |                    |           |
-- | @Applicative f@ |            |               |                    |           |
-- | @=> Kleisli f@  |     X      |       X       |         X          |     X (1) |
-- +-----------------+------------+---------------+--------------------+-----------+
--
-- Essentially:
--
--  * The instance marked (1) implies that the linear traversal definition
--    includes the standard one
--  * The instance marked by (2) implies that the linear lens definition
--    includes the standard one
--  * The instance marked by (3) implies that the linear prism definition
--    includes the standard one
--  * The instance marked by (4) implies that the linear iso definition
--    includes the standard one
--
module Control.Optics.Linear
  ( Optic_(..)
  , Optic
  , module Control.Optics.Linear.Iso
  , module Control.Optics.Linear.Lens
  , module Control.Optics.Linear.Prism
  , module Control.Optics.Linear.Traversal
  )
where

import Control.Optics.Linear.Internal (Optic_(..), Optic)
import Control.Optics.Linear.Iso
import Control.Optics.Linear.Lens
import Control.Optics.Linear.Prism
import Control.Optics.Linear.Traversal
