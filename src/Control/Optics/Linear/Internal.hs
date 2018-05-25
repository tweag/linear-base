{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Control.Optics.Linear.Internal where

import qualified Data.Profunctor.Linear as Monoidal
import Data.Profunctor.Linear hiding (swap)
import Data.Void
import Prelude.Linear

type Optic_ arr a b s t = a `arr` b -> s `arr` t

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
type Traversal a b s t =
  forall arr.
  ( Strong (,) () arr , Strong Either Void arr , Monoidal (,) () arr)
  => Optic_ arr a b s t
type Traversal' a s = Traversal a a s s

-- TODO: I'm sure most of these type can be monomorphic optics into polymorphic
-- ones.
swap :: SymmetricMonoidal m u => Iso' (a `m` b) (b `m` a)
swap = dimap Monoidal.swap Monoidal.swap

_1 :: Lens' a (a, b)
_1 = first

_2 :: Lens' b (a, b)
_2 = second

_Left :: Prism' a (Either a b)
_Left = first

_Right :: Prism' b (Either a b)
_Right = second

-- traversed :: Traversable t => Traversal a b (t a) (t b)
-- traversed p = _
