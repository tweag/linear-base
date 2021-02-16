{-# LANGUAGE LinearTypes #-}

module Data.Unrestricted.Internal.UrT
  (
    UrT(..)
  , runUrT
  , liftUrT
  , evalUrT
  ) where

import qualified Control.Functor.Linear as Linear
import Data.Unrestricted.Internal.Ur
import Data.Unrestricted.Internal.Movable

-- | @UrT@ transforms linear control monads to non-linear monads.
--
-- * @UrT `Linear.State` a@ is a non-linear monad with linear state.
newtype UrT m a = UrT (m (Ur a))

-- | Linearly unwrap the @UrT@ newtype wrapper.
runUrT :: UrT m a %1 -> m (Ur a)
runUrT (UrT ma) = ma

instance Linear.Functor m => Functor (UrT m) where
  fmap f (UrT ma) = UrT (Linear.fmap (\(Ur a) -> Ur (f a)) ma)

instance Linear.Applicative m => Applicative (UrT m) where
  pure a = UrT (Linear.pure (Ur a))
  UrT mf <*> UrT ma = UrT (Linear.liftA2 (\(Ur f) (Ur a) -> Ur (f a)) mf ma)

instance Linear.Monad m => Monad (UrT m) where
  UrT ma >>= f = UrT (ma Linear.>>= (\(Ur a) -> case f a of (UrT mb) -> mb))

-- | Lift a computation to the @UrT@ monad, provided that the type @a@ can be used unrestriced.
liftUrT :: (Movable a, Linear.Functor m) => m a %1 -> UrT m a
liftUrT ma = UrT (Linear.fmap move ma)

-- | Extract the inner computation linearly, the inverse of `liftUrT`.
--
-- > evalUrT (liftUrT m) = m
evalUrT :: Linear.Functor m => UrT m a %1 -> m a
evalUrT u = Linear.fmap unur (runUrT u)

