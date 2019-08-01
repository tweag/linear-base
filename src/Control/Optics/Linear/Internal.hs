{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Control.Optics.Linear.Internal where

import qualified Data.Bifunctor.Linear as Bifunctor
import Data.Bifunctor.Linear (SymmetricMonoidal)
import Data.Functor.Const
import Data.Functor.Linear
import Data.Monoid
import Data.Profunctor.Linear
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
swap = Optical (dimap Bifunctor.swap Bifunctor.swap)

assoc :: SymmetricMonoidal m u => Iso ((a `m` b) `m` c) ((d `m` e) `m` f) (a `m` (b `m` c)) (d `m` (e `m` f))
assoc = Optical (dimap Bifunctor.lassoc Bifunctor.rassoc)

_1 :: Lens a b (a,c) (b,c)
_1 = Optical first

_2 :: Lens a b (c,a) (c,b)
_2 = Optical second

_Left :: Prism a b (Either a c) (Either b c)
_Left = Optical first

_Right :: Prism a b (Either c a) (Either c b)
_Right = Optical second

traversed :: Traversable t => Traversal a b (t a) (t b)
traversed = Optical wander

over :: Optic_ LinearArrow a b s t -> (a ->. b) -> s ->. t
over (Optical l) f = getLA (l (LA f))

traverseOf :: Optic_ (LKleisli f) a b s t -> (a ->. f b) -> s ->. f t
traverseOf (Optical l) f = runLKleisli (l (LKleisli f))

get :: Optic_ (Kleisli (Const a)) a b s t -> s -> a
get l = gets l P.id

gets :: Optic_ (Kleisli (Const r)) a b s t -> (a -> r) -> s -> r
gets (Optical l) f s = getConst (runKleisli (l (Kleisli (Const P.. f))) s)

set :: Optic_ (->) a b s t -> b -> s -> t
set (Optical l) x = l (const x)

lengthOf :: Num r => Optic_ (Kleisli (Const (Sum r))) a b s t -> s -> r
lengthOf l s = getSum (gets l (const (Sum 1)) s)

traverseOf' :: Optic_ (Kleisli f) a b s t -> (a -> f b) -> s -> f t
traverseOf' (Optical l) f = runKleisli (l (Kleisli f))
