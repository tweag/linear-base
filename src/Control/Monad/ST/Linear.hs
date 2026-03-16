{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module redefines 'ST' with linear types.
module Control.Monad.ST.Linear
  ( ST (..),
    runST,

    -- * Interfacing with "Control.Monad.ST"
    fromUrST,
    fromUrSTU,
    toUrST,

    -- * Using Mutable References
    -- $stref
    newSTRef,
    readSTRef,
    writeSTRef,
  )
where

import qualified Control.Functor.Linear as Control
import qualified Data.Functor.Linear as Data
import Data.STRef (STRef)
import qualified Data.STRef as UrST
import GHC.Exts (State#)
import qualified GHC.ST as UrST (ST (..))
import Prelude.Linear
import qualified Control.Monad.ST as UrST
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

-- | This is the linear ST s monad.
-- It is a newtype around a function that transitions from one
-- @State# s@ to another, producing a value of type @a@ along with it.
-- The @State# s@ is the state of the overall stateful computation.
--
-- Note that this is the same definition as the standard ST monad, but with a
-- linear arrow enforcing the implicit invariant that ST actions linearly
-- thread the state. Hence, we can safely release the
-- constructor to this newtype.
newtype ST s a = ST (State# s %1 -> (# State# s, a #))
  deriving (Data.Functor, Data.Applicative) via (Control.Data (ST s))

type role ST nominal representational

-- Defined separately because projections from newtypes are considered like
-- general projections of data types, which take an unrestricted argument.
unST :: ST s a %1 -> State# s %1 -> (# State# s, a #)
unST (ST action) = action

-- | Coerces a standard ST action into a linear ST action.
-- Note that the value @a@ must be used linearly in the linear ST monad.
fromUrST :: UrST.ST s a %1 -> ST s a
-- The implementation relies on the fact that the monad abstraction for ST
-- actually enforces linear use of the @s@ token.
--
-- There are potential difficulties coming from the fact that usage differs:
-- returned value in 'Control.Monad.ST' can be used unrestrictedly, which is not
-- typically possible of linear 'ST'. This means that 'Control.Monad.ST' action are
-- not actually mere translations of linear 'ST' action. Still I [aspiwack]
-- think that it is safe, hence no "unsafe" in the name.
fromUrST = Unsafe.coerce

-- | Coerces a standard ST action to a linear ST action, allowing you to use
-- the result of type @a@ in a non-linear manner by wrapping it inside
-- 'Ur'.
fromUrSTU :: UrST.ST s a -> ST s (Ur a)
fromUrSTU action =
  fromUrST (Ur Prelude.<$> action)

-- | Convert a linear ST action to a "Control.Monad.ST" action.
toUrST :: ST s a %1 -> UrST.ST s a
toUrST (ST f) = UrST.ST (\s -> f s)

runST :: (forall s. ST s (Ur a)) -> a
runST action = UrST.runST ((\x -> unur x) Prelude.<$> toUrST action)

-- * Monadic interface

instance Control.Functor (ST s) where
  fmap :: forall a b. (a %1 -> b) %1 -> ST s a %1 -> ST s b
  fmap f x = ST $ \s ->
    cont (unST x s) f
    where
      -- XXX: long line
      cont :: (# State# s, a #) %1 -> (a %1 -> b) %1 -> (# State# s, b #)
      cont (# s', a #) f' = (# s', f' a #)

instance Control.Applicative (ST s) where
  pure :: forall a. a %1 -> ST s a
  pure a = ST $ \s -> (# s, a #)

  (<*>) :: forall a b. ST s (a %1 -> b) %1 -> ST s a %1 -> ST s b
  (<*>) = Control.ap

instance Control.Monad (ST s) where
  (>>=) :: forall a b. ST s a %1 -> (a %1 -> ST s b) %1 -> ST s b
  x >>= f = ST $ \s ->
    cont (unST x s) f
    where
      -- XXX: long line
      cont :: (# State# s, a #) %1 -> (a %1 -> ST s b) %1 -> (# State# s, b #)
      cont (# s', a #) f' = unST (f' a) s'

  (>>) :: forall b. ST s () %1 -> ST s b %1 -> ST s b
  x >> y = ST $ \s ->
    cont (unST x s) y
    where
      cont :: (# State# s, () #) %1 -> ST s b %1 -> (# State# s, b #)
      cont (# s', () #) y' = unST y' s'

instance (Semigroup a) => Semigroup (ST s a) where
  (<>) = Control.liftA2 (<>)

instance (Monoid a) => Monoid (ST s a) where
  mempty = Control.pure mempty

-- $stref
-- @STRef@s are mutable references to values, or pointers to values.
-- You can create, mutate and read them from running ST actions.
--
-- Note that all arrows are unrestricted.  This is because STRefs containing
-- linear values can make linear values escape their scope and be used
-- non-linearly.

newSTRef :: a -> ST s (Ur (STRef s a))
newSTRef a = fromUrSTU (UrST.newSTRef a)

readSTRef :: STRef s a -> ST s (Ur a)
readSTRef r = fromUrSTU (UrST.readSTRef r)

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef r a = fromUrST $ UrST.writeSTRef r a
