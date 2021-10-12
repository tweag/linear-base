{-# language EmptyCase #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LinearTypes #-}
{-# language NoImplicitPrelude #-}
{-# language TypeOperators #-}

module Data.Functor.Linear.Internal.Traversable.Generic where

import Generics.Linear
import Control.Functor.Linear
import Data.Functor.Linear.Internal.Traversable
import Control.Functor.Linear.Internal.Kan
import GHC.Types (Multiplicity (..))
import Prelude.Linear

class GTraversable t where
  gtraverse :: Applicative f => (a %1-> f b) -> t a %1-> Curried (Yoneda f) (Yoneda f) (t b)

instance GTraversable t => GTraversable (M1 i c t) where
  gtraverse f (M1 x) = M1 <$> gtraverse f x
  {-# INLINE gtraverse #-}

-- Can m be polymorphic? I'm not optimistic.
instance (m ~ 'One, GTraversable t) => GTraversable (MP1 m t) where
  gtraverse f (MP1 x) = fmap MP1 (gtraverse f x)
  {-# INLINE gtraverse #-}

instance GTraversable Par1 where
  gtraverse f (Par1 x) = Par1 <$> liftCurriedYonedaC (f x)
  {-# INLINE gtraverse #-}

instance (GTraversable f, Traversable g) => GTraversable (f :.: g) where
  gtraverse f (Comp1 x) = Comp1 <$> gtraverse (traverse f) x
  {-# INLINE gtraverse #-}

instance (GTraversable f, GTraversable g) => GTraversable (f :+: g) where
  gtraverse f (L1 x) = L1 <$> gtraverse f x
  gtraverse f (R1 x) = R1 <$> gtraverse f x
  {-# INLINE gtraverse #-}

instance (GTraversable f, GTraversable g) => GTraversable (f :*: g) where
  gtraverse f (x :*: y) = liftA2 (:*:) (gtraverse f x) (gtraverse f y)
  {-# INLINE gtraverse #-}

instance GTraversable (K1 i c) where
  gtraverse _ (K1 c) = pure (K1 c)
  {-# INLINE gtraverse #-}

instance GTraversable U1 where
  gtraverse _ U1 = pure U1
  {-# INLINE gtraverse #-}

instance GTraversable V1 where
  gtraverse _ v = pure ((\case) v)

instance GTraversable UAddr where
  gtraverse _ (UAddr x) = pure (UAddr x)
  {-# INLINE gtraverse #-}

instance GTraversable UChar where
  gtraverse _ (UChar x) = pure (UChar x)
  {-# INLINE gtraverse #-}

instance GTraversable UDouble where
  gtraverse _ (UDouble x) = pure (UDouble x)
  {-# INLINE gtraverse #-}

instance GTraversable UFloat where
  gtraverse _ (UFloat x) = pure (UFloat x)
  {-# INLINE gtraverse #-}

instance GTraversable UInt where
  gtraverse _ (UInt x) = pure (UInt x)
  {-# INLINE gtraverse #-}

instance GTraversable UWord where
  gtraverse _ (UWord x) = pure (UWord x)
  {-# INLINE gtraverse #-}

genericTraverse
  :: (Generic1 t, GTraversable (Rep1 t), Applicative f)
  => (a %1-> f b) -> t a %1-> f (t b)
genericTraverse f = lowerYoneda . lowerCurriedC . fmap to1 . gtraverse f . from1
{-# INLINE genericTraverse #-}
