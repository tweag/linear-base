{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}

module Data.Unrestricted.Internal.Ur
  (
    Ur(..)
  , unur
  , lift
  , lift2
  ) where

-- | @Ur a@ represents unrestricted values of type @a@ in a linear
-- context. The key idea is that because the contructor holds @a@ with a
-- regular arrow, a function that uses @Ur a@ linearly can use @a@
-- however it likes.
-- > someLinear :: Ur a %1-> (a,a)
-- > someLinear (Ur a) = (a,a)
data Ur a where
  Ur :: a -> Ur a

-- | Get an @a@ out of an @Ur a@. If you call this function on a
-- linearly bound @Ur a@, then the @a@ you get out has to be used
-- linearly, for example:
--
-- > restricted :: Ur a %1-> b
-- > restricted x = f (unur x)
-- >   where
-- >     -- f __must__ be linear
-- >     f :: a %1-> b
-- >     f x = ...
unur :: Ur a %1-> a
unur (Ur a) = a

-- | Lifts a function on a linear @Ur a@.
lift :: (a -> b) -> Ur a %1-> Ur b
lift f (Ur a) = Ur (f a)

-- | Lifts a function to work on two linear @Ur a@.
lift2 :: (a -> b -> c) -> Ur a %1-> Ur b %1-> Ur c
lift2 f (Ur a) (Ur b) = Ur (f a b)

