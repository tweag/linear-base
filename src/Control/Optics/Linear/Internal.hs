{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Control.Optics.Linear.Internal where

import Prelude.Linear
import Data.Profunctor.Linear
import Data.Void

type Optic_ arr a b s t = a `arr` b -> s `arr` t

type Optic c a b s t =
  forall arr. c arr => Optic_ arr a b s t

type Iso a b s t = Optic Profunctor a b s t
type Lens a b s t = Optic (StrongProfunctor (,) ()) a b s t
type Prism a b s t = Optic (StrongProfunctor Either Void) a b s t

-- | Because it is inconvenient to pair up parametric constraints, 'Traversal'
-- is defined in terms of 'Optic_' rather than 'Optic'.
type Traversal a b s t =
  forall arr.
  ( StrongProfunctor (,) () arr
  , StrongProfunctor Either Void arr
  , MonoidalProfunctor (,) () arr)
  => Optic_ arr a b s t
