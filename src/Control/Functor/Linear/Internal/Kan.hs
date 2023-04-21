{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | A few things lifted from kan-extensions and lens for generic deriving of
-- 'Data.Functor.Linear.Traversable' instances (see
-- "Data.Functor.Linear.Internal.Traversable").
module Control.Functor.Linear.Internal.Kan where

import Control.Functor.Linear
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import Prelude.Linear.Internal

-- | A linear version of @Data.Functor.Day.Curried.Curried@ in the
-- @kan-extensions@ package. We use this for generic traversals. How
-- does it help? Consider a type like
--
-- @data Foo a = Foo a a a a@
--
-- The generic representation may look roughly like
--
-- @D1 _ (C1 _ ((S1 _ Rec1 :*: S1 _ Rec1) :*: (S1 _ Rec1 :*: S1 _ Rec1)))@
--
-- Traversing this naively requires a bunch of @fmap@ applications.
-- Most of them could be removed using 'Yoneda', but one aspect
-- can't be. Let's simplify down to the hard bit:
--
-- @m :*: (n :*: o)@
--
-- Traversing this looks like
--
-- @((:*:) <$> m) <*> ((:*:) <$> n <*> o)@
--
-- We want to reassociate the applications so the whole reconstruction
-- of the generic representation happens in one place, allowing inlining
-- to (hopefully) erase them altogether. It will end up looking roughly like
--
-- @(\x y z -> x :*: (y :*: z)) <$> m <*> n <*> o@
--
-- In our context, we always have the two functor
-- arguments the same, so something like @Curried f f@.
-- @Curried f f a@ is a lot like @f a@, as demonstrated directly by
-- 'lowerCurriedC' and, in @kan-extensions@, @liftCurried@.
-- It's a sort of "continuation passing style" version. If we have
-- something like
--
-- @
-- Con <$> m <*> n <*> o
--
-- -- parenthesized
--
-- ((Con <$> m) <*> n) <*> o
-- @
--
-- we can look at what happens next to each field. So the next thing
-- after performing @m@ is to map @Con@ over it. The next thing after
-- performing @n@ is to apply @Con <$> m@ to it within the functor.
newtype Curried g h a = Curried
  {runCurried :: forall r. g (a %1 -> r) %1 -> h r}

instance (Data.Functor g) => Data.Functor (Curried g h) where
  fmap f (Curried g) = Curried (g . Data.fmap (. f))
  {-# INLINE fmap #-}

instance (Functor g) => Functor (Curried g h) where
  fmap f (Curried g) = Curried (\x -> g (fmap (\y -> y . f) x))
  {-# INLINE fmap #-}

instance (Data.Functor g, g ~ h) => Data.Applicative (Curried g h) where
  pure a = Curried (Data.fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . Data.fmap (.))
  {-# INLINE (<*>) #-}

instance (Functor g, g ~ h) => Applicative (Curried g h) where
  pure a = Curried (fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}

lowerCurriedC :: (Applicative f) => Curried f g a %1 -> g a
lowerCurriedC (Curried f) = f (pure id)
{-# INLINE lowerCurriedC #-}

newtype Yoneda f a = Yoneda {runYoneda :: forall b. (a %1 -> b) %1 -> f b}

instance Data.Functor (Yoneda f) where
  fmap f (Yoneda m) = Yoneda (\k -> m (k . f))
  {-# INLINE fmap #-}

instance Functor (Yoneda f) where
  fmap f (Yoneda m) = Yoneda (\k -> m (k . f))
  {-# INLINE fmap #-}

instance (Applicative f) => Data.Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  {-# INLINE pure #-}
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (\g -> f . g) <*> n id)
  {-# INLINE (<*>) #-}

instance (Applicative f) => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  {-# INLINE pure #-}
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (\g -> f . g) <*> n id)
  {-# INLINE (<*>) #-}

lowerYoneda :: Yoneda f a %1 -> f a
lowerYoneda (Yoneda m) = m id
{-# INLINE lowerYoneda #-}

-- This bit comes from lens.
liftCurriedYonedaC :: (Applicative f) => f a %1 -> Curried (Yoneda f) (Yoneda f) a
liftCurriedYonedaC fa = Curried (`yap` fa)
{-# INLINE liftCurriedYonedaC #-}

yap :: (Applicative f) => Yoneda f (a %1 -> b) %1 -> f a %1 -> Yoneda f b
yap (Yoneda k) fa = Yoneda (\ab_r -> k (\g -> ab_r . g) <*> fa)
{-# INLINE yap #-}
