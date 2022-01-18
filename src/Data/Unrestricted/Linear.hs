-- | This module provides essential tools for doing non-linear things
-- in linear code.
--
-- = /Critical/ Definition: Restricted
--
-- In a linear function @f :: a %1-> b@, the argument @a@ must
-- be used in a linear way. Its use is __restricted__ while
-- an argument in a non-linear function is __unrestricted__.
--
-- Hence, a linear function with an argument of @Ur a@ (@Ur@ is short for
-- /unrestricted/) can use the @a@ in an unrestricted way. That is, we have
-- the following equivalence:
--
-- @
-- (Ur a %1-> b) ≌ (a -> b)
-- @
--
-- = Consumable, Dupable, Moveable classes
--
-- Use these classes to perform some non-linear action on linearly bound values.
--
-- If a type is 'Consumable', you can __consume__ it in a linear function that
-- doesn't need that value to produce it's result:
--
-- > first :: Consumable b => (a,b) %1-> a
-- > first (a,b) = withConsume (consume b) a
-- >   where
-- >     withConsume :: () %1-> a %1-> a
-- >     withConsume () x = x
--
-- If a type is 'Dupable', you can __duplicate__ it as much as you like.
--
-- > -- checkIndex ix size_of_array
-- > checkIndex :: Int %1-> Int %1-> Bool
-- > checkIndex ix size = withDuplicate (dup2 ix) size
-- >   where
-- >     withDuplicate :: (Int, Int) %1-> Int %1-> Bool
-- >     withDuplicate (ix,ix') size = (0 <= ix) && (ix < size)
-- >     (<) :: Int %1-> Int %1-> Bool
-- >     (<) = ...
-- >
-- >     (<=) :: Int %1-> Int %1-> Bool
-- >     (<=) = ...
-- >
-- >     (&&) :: Bool %1-> Bool %1-> Bool
-- >     (&&) = ...
--
-- If a type is 'Moveable', you can __move__ it inside 'Ur'
-- and use it in any non-linear way you would like.
--
-- > diverge :: Int %1-> Bool
-- > diverge ix = fromMove (move ix)
-- >   where
-- >     fromMove :: Ur Int %1-> Bool
-- >     fromMove (Ur 0) = True
-- >     fromMove (Ur 1) = True
-- >     fromMove (Ur x) = False
module Data.Unrestricted.Linear
  ( -- * Unrestricted
    Ur (..),
    unur,
    lift,
    lift2,
    UrT (..),
    runUrT,
    liftUrT,
    evalUrT,

    -- * Performing non-linear actions on linearly bound values
    Consumable (..),
    Dupable (..),
    Movable (..),
    RepStream (..),
    Replicator (..),
    lseq,
    dup,
    dup3,
    module Data.Unrestricted.Internal.Instances,
  )
where

import Data.Unrestricted.Internal.Consumable
import Data.Unrestricted.Internal.Dupable
import Data.Unrestricted.Internal.Instances
import Data.Unrestricted.Internal.Movable
import Data.Unrestricted.Internal.Ur
import Data.Unrestricted.Internal.UrT
import Data.Unrestricted.Internal.Replicator
