{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Functor.Linear.Internal.Traversable.Generic where

import Control.Functor.Linear
import Control.Functor.Linear.Internal.Kan
import Data.Functor.Linear.Internal.Traversable
import GHC.Types (Multiplicity (..))
import Generics.Linear
import Prelude.Linear.Internal

-- | This type class derives the definition of 'genericTraverse' by induction on
-- the generic representation of a type.
class GTraversable t where
  -- gtraverse :: Applicative f => (a %1 -> f b) -> t a %1 -> forall r. (forall k. ((a %1 -> r) %1 -> k) %1 -> f k) %1 -> forall k. (t b %1 -> k) %1 -> f k
  --
  -- TODO: developer documentation on why we use this type rather than the more
  -- straightforward type of `traverse`. Used, for instance, in the
  -- generic-deriving package.
  gtraverse :: Applicative f => (a %1 -> f b) -> t a %1 -> Curried (Yoneda f) (Yoneda f) (t b)

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
  gtraverse _ v = pure ((\case {}) v)

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

-- | Implementation of 'Data.Functor.Linear.traverse' for types which derive
-- (linear) 'Generics.Linear.Generic1'.
--
-- ### Example
--
-- > data T
-- > $(deriveGeneric1 ''T)
-- >
-- > instance Traversable T where
-- >   traverse = genericTraverse
--
-- Note that, contrary to many other classes in linear-base, we can't define
-- `Traversable T` using deriving via, because the
-- [role](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html)
-- of `t`, in the type of 'Data.Functor.Linear.traverse', is nominal.
genericTraverse ::
  (Generic1 t, GTraversable (Rep1 t), Applicative f) =>
  (a %1 -> f b) ->
  t a %1 ->
  f (t b)
genericTraverse f = lowerYoneda . lowerCurriedC . fmap to1 . gtraverse f . from1
{-# INLINE genericTraverse #-}
