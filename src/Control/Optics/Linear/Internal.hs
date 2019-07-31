{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Control.Optics.Linear.Internal where

import qualified Data.Bifunctor.Linear as Bifunctor
import Data.Bifunctor.Linear (SymmetricMonoidal)
import Data.Profunctor.Linear
import Data.Void
import Prelude.Linear

newtype Optic_ arr a b s t = Optical (a `arr` b -> s `arr` t)

type Optic c a b s t =
  forall arr. c arr => Optic_ arr a b s t

type Iso a b s t = Optic Profunctor a b s t
type Iso' a s = Iso a a s s
type Lens a b s t = Optic (Strong (,) ()) a b s t
type Lens' a s = Lens a a s s
type Prism a b s t = Optic (Strong Either Void) a b s t
type Prism' a s = Prism a a s s

-- | Because it is inconvenient to pair up parametric constraints, 'Traversal'
-- is defined in terms of 'Optic_' rather than 'Optic'.
-- type Traversal a b s t =
--   forall arr.
--   ( Strong (,) () arr , Strong Either Void arr , Monoidal (,) () arr)
--   => Optic_ arr a b s t
-- type Traversal' a s = Traversal a a s s

-- TODO: I'm sure most of these type can be monomorphic optics into polymorphic
-- ones.
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

-- traversed :: Traversable t => Traversal a b (t a) (t b)
-- traversed p = _
