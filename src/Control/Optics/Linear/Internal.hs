{-# OPTIONS_HADDOCK hide #-}
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
  , get, set, gets, setSwap
  , match, build
  , over, overU
  , traverseOf, traverseOfU
  , toListOf, lengthOf
  , reifyLens
  , withIso, withLens, withPrism
    -- * Constructing optics
  , iso, lens, prism, traversal
  )
  where

import qualified Control.Arrow as NonLinear
import qualified Control.Monad.Linear as Control
import qualified Data.Bifunctor.Linear as Bifunctor
import Data.Bifunctor.Linear (SymmetricMonoidal)
import Data.Profunctor.Linear
import Data.Functor.Compose hiding (getCompose)
import Data.Functor.Linear
import qualified Data.Profunctor.Kleisli.Linear as Linear
import Data.Void
import GHC.Exts (FUN)
import GHC.Types
import Prelude.Linear
import qualified Prelude as P

newtype Optic_ arr s t a b = Optical (a `arr` b -> s `arr` t)

type Optic c s t a b =
  forall arr. c arr => Optic_ arr s t a b

type Iso s t a b = Optic Profunctor s t a b
type Iso' s a = Iso s s a a
type Lens s t a b = Optic (Strong (,) ()) s t a b
type Lens' s a = Lens s s a a
type Prism s t a b = Optic (Strong Either Void) s t a b
type Prism' s a = Prism s s a a
type Traversal s t a b = Optic Wandering s t a b
type Traversal' s a = Traversal s s a a

swap :: SymmetricMonoidal m u => Iso (a `m` b) (c `m` d) (b `m` a) (d `m` c)
swap = iso Bifunctor.swap Bifunctor.swap

assoc :: SymmetricMonoidal m u => Iso (a `m` (b `m` c)) (d `m` (e `m` f)) ((a `m` b) `m` c) ((d `m` e) `m` f)
assoc = iso Bifunctor.lassoc Bifunctor.rassoc

(.>) :: Optic_ arr s t a b -> Optic_ arr a b x y -> Optic_ arr s t x y
Optical f .> Optical g = Optical (f P.. g)


lens :: (s %1-> (a, b %1-> t)) -> Lens s t a b
lens k = Optical $ \f -> dimap k (\(x,g) -> g $ x) (first f)

prism :: (b %1-> t) -> (s %1-> Either t a) -> Prism s t a b
prism b s = Optical $ \f -> dimap s (either id id) (second (rmap b f))

traversal :: (forall f. Control.Applicative f => (a %1-> f b) -> s %1-> f t) -> Traversal s t a b
traversal trav = Optical $ wander trav

_1 :: Lens (a,c) (b,c) a b
_1 = Optical first

_2 :: Lens (c,a) (c,b) a b
_2 = Optical second

_Left :: Prism (Either a c) (Either b c) a b
_Left = Optical first

_Right :: Prism (Either c a) (Either c b) a b
_Right = Optical second

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism (\() -> Nothing) Left

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = Optical $ wander traverse

over :: Optic_ LinearArrow s t a b -> (a %1-> b) -> s %1-> t
over (Optical l) f = getLA (l (LA f))

traverseOf :: Optic_ (Linear.Kleisli f) s t a b -> (a %1-> f b) -> s %1-> f t
traverseOf (Optical l) f = Linear.runKleisli (l (Linear.Kleisli f))

toListOf :: Optic_ (NonLinear.Kleisli (Const [a])) s t a b -> s -> [a]
toListOf l = gets l (\a -> [a])

get :: Optic_ (NonLinear.Kleisli (Const a)) s t a b -> s -> a
get l = gets l P.id

gets :: Optic_ (NonLinear.Kleisli (Const r)) s t a b -> (a -> r) -> s -> r
gets (Optical l) f s = getConst' (NonLinear.runKleisli (l (NonLinear.Kleisli (Const P.. f))) s)

set :: Optic_ (->) s t a b -> b -> s -> t
set (Optical l) x = l (const x)

setSwap :: Optic_ (Linear.Kleisli (Compose (LinearArrow b) ((,) a))) s t a b -> s %1-> b %1-> (a, t)
setSwap (Optical l) s = getLA (getCompose (Linear.runKleisli (l (Linear.Kleisli (\a -> Compose (LA (\b -> (a,b)))))) s))

match :: Optic_ (Market a b) s t a b -> s %1-> Either t a
match (Optical l) = P.snd (runMarket (l (Market id Right)))

build :: Optic_ (Linear.CoKleisli (Const b)) s t a b -> b %1-> t
build (Optical l) x = Linear.runCoKleisli (l (Linear.CoKleisli getConst')) (Const x)

-- XXX: move this to Prelude
-- | Linearly typed patch for the newtype deconstructor. (Temporary until
-- inference will get this from the newtype declaration.)
getConst' :: Const a b %1-> a
getConst' (Const x) = x

lengthOf :: MultIdentity r => Optic_ (NonLinear.Kleisli (Const (Adding r))) s t a b -> s -> r
lengthOf l s = getAdded (gets l (const (Adding one)) s)

-- XXX: the below two functions will be made redundant with multiplicity
-- polymorphism on over and traverseOfU
overU :: Optic_ (->) s t a b -> (a -> b) -> s -> t
overU (Optical l) f = l f

traverseOfU :: Optic_ (NonLinear.Kleisli f) s t a b -> (a -> f b) -> s -> f t
traverseOfU (Optical l) f = NonLinear.runKleisli (l (NonLinear.Kleisli f))

iso :: (s %1-> a) -> (b %1-> t) -> Iso s t a b
iso f g = Optical (dimap f g)

withIso :: Optic_ (Exchange a b) s t a b -> ((s %1-> a) -> (b %1-> t) -> r) -> r
withIso (Optical l) f = f fro to
  where Exchange fro to = l (Exchange id id)

withPrism :: Optic_ (Market a b) s t a b -> ((b %1-> t) -> (s %1-> Either t a) -> r) -> r
withPrism (Optical l) f = f b m
  where Market b m = l (Market id Right)

-- XXX: probably a direct implementation would be better
withLens
  :: Optic_ (Linear.Kleisli (Compose ((,) a) (FUN 'One b))) s t a b
  -> (forall c. (s %1-> (c, a)) -> ((c, b) %1-> t) -> r)
  -> r
withLens l k = k (Bifunctor.swap . (reifyLens l)) (uncurry ($))

reifyLens :: Optic_ (Linear.Kleisli (Compose ((,) a) (FUN 'One b))) s t a b -> s %1-> (a, b %1-> t)
reifyLens (Optical l) s = getCompose (Linear.runKleisli (l (Linear.Kleisli (\a -> Compose (a, id)))) s)

-- linear variant of getCompose
getCompose :: Compose f g a %1-> f (g a)
getCompose (Compose x) = x
