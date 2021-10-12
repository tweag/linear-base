{-# language GADTs #-}
{-# language LinearTypes #-}
{-# language RankNTypes #-}
{-# language NoImplicitPrelude #-}

-- | A few things lifted from kan-extensions and lens for generic deriving of
-- Traversable instances.
module Control.Functor.Linear.Internal.Kan where
import Control.Functor.Linear
import qualified Data.Functor.Linear as D
import Prelude.Linear

newtype Curried g h a = Curried
  { runCurried :: forall r. g (a %1-> r) %1-> h r }

instance D.Functor g => D.Functor (Curried g h) where
  fmap f (Curried g) = Curried (g . D.fmap (.f))
  {-# INLINE fmap #-}

instance Functor g => Functor (Curried g h) where
  fmap f (Curried g) = Curried (\x -> g (fmap (\y -> y . f) x))
  {-# INLINE fmap #-}

instance (D.Functor g, g ~ h) => D.Applicative (Curried g h) where
  pure a = Curried (D.fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . D.fmap (.))
  {-# INLINE (<*>) #-}

instance (Functor g, g ~ h) => Applicative (Curried g h) where
  pure a = Curried (fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}

lowerCurriedC :: Applicative f => Curried f g a %1-> g a
lowerCurriedC (Curried f) = f (pure id)
{-# INLINE lowerCurriedC #-}

newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a %1-> b) %1-> f b }

instance D.Functor (Yoneda f) where
  fmap f (Yoneda m) = Yoneda (\k -> m (k . f))
  {-# INLINE fmap #-}

instance Functor (Yoneda f) where
  fmap f (Yoneda m) = Yoneda (\k -> m (k . f))
  {-# INLINE fmap #-}

instance Applicative f => D.Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  {-# INLINE pure #-}
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)
  {-# INLINE (<*>) #-}

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  {-# INLINE pure #-}
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)
  {-# INLINE (<*>) #-}

lowerYoneda :: Yoneda f a %1-> f a
lowerYoneda (Yoneda m) = m id
{-# INLINE lowerYoneda #-}

-- This bit comes from lens.
liftCurriedYonedaC :: Applicative f => f a %1-> Curried (Yoneda f) (Yoneda f) a
liftCurriedYonedaC fa = Curried (`yap` fa)
{-# INLINE liftCurriedYonedaC #-}

yap :: Applicative f => Yoneda f (a %1-> b) %1-> f a %1-> Yoneda f b
yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa)
{-# INLINE yap #-}
