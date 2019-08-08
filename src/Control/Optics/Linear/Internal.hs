{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

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
  , match, match', build
  , over, over'
  , traverseOf, traverseOf'
  , lengthOf
  , withIso
    -- * Constructing optics
  , iso, prism
  )
  where

import qualified Control.Arrow as NonLinear
import qualified Data.Bifunctor.Linear as Bifunctor
import Data.Bifunctor.Linear (SymmetricMonoidal)
import Data.Functor.Const
import Data.Functor.Linear
import Data.Monoid
import Data.Profunctor.Linear
import qualified Data.Profunctor.Kleisli.Linear as Linear
import Data.Void
import Prelude.Linear
import qualified Prelude as P

newtype Optic_ arr a b s t = Optical (a `arr` b -> s `arr` t)

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

prism :: (b ->. t) -> (s ->. Either t a) -> Prism a b s t
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

over :: Optic_ LinearArrow a b s t -> (a ->. b) -> s ->. t
over (Optical l) f = getLA (l (LA f))

traverseOf :: Optic_ (Linear.Kleisli f) a b s t -> (a ->. f b) -> s ->. f t
traverseOf (Optical l) f = Linear.runKleisli (l (Linear.Kleisli f))

get :: Optic_ (NonLinear.Kleisli (Const a)) a b s t -> s -> a
get l = gets l P.id

gets :: Optic_ (NonLinear.Kleisli (Const r)) a b s t -> (a -> r) -> s -> r
gets (Optical l) f s = getConst' (NonLinear.runKleisli (l (NonLinear.Kleisli (Const P.. f))) s)

set :: Optic_ (->) a b s t -> b -> s -> t
set (Optical l) x = l (const x)

match :: Optic_ (Linear.Kleisli (Either a)) a b s t -> s ->. Either t a
match (Optical l) = withIso swap (\x _ -> x) . Linear.runKleisli (l (Linear.Kleisli Left))

-- will be redundant with multiplicity polymorphism
match' :: Optic_ (NonLinear.Kleisli (Either a)) a b s t -> s -> Either t a
match' (Optical l) = withIso swap (\x _ -> forget x) P.. NonLinear.runKleisli (l (NonLinear.Kleisli Left))

build :: Optic_ (Linear.CoKleisli (Const b)) a b s t -> b ->. t
build (Optical l) x = Linear.runCoKleisli (l (Linear.CoKleisli getConst')) (Const x)

-- XXX: move this to Prelude
-- | Linearly typed patch for the newtype deconstructor. (Temporary until
-- inference will get this from the newtype declaration.)
getConst' :: Const a b ->. a
getConst' (Const x) = x

lengthOf :: Num r => Optic_ (NonLinear.Kleisli (Const (Sum r))) a b s t -> s -> r
lengthOf l s = getSum (gets l (const (Sum 1)) s)

-- XXX: the below two functions will be made redundant with multiplicity
-- polymorphism on over and traverseOf'
over' :: Optic_ (->) a b s t -> (a -> b) -> s -> t
over' (Optical l) f = l f

traverseOf' :: Optic_ (NonLinear.Kleisli f) a b s t -> (a -> f b) -> s -> f t
traverseOf' (Optical l) f = NonLinear.runKleisli (l (NonLinear.Kleisli f))

iso :: (s ->. a) -> (b ->. t) -> Iso a b s t
iso f g = Optical (dimap f g)

withIso :: Optic_ (Exchange a b) a b s t -> ((s ->. a) -> (b ->. t) -> r) -> r
withIso (Optical l) f = f fro to
  where Exchange fro to = l (Exchange id id)
