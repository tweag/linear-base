{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides Bifunctor and related classes.
--
-- == 'Bifunctor'
--
-- Use a bifunctor instance to map functions over data structures
-- that have two type paramaters @a@ and @b@ and could be have a
-- functor instance for either the @a@s or @b@s.
-- For instance, you might want to map a function on either the left
-- or right element of a @(Int, Bool)@:
--
-- > import Prelude.Linear
-- > import Data.Bifunctor.Linear
-- >
-- > -- Map over the second element
-- > negateRight :: (Int, Bool) %1-> (Int, Bool)
-- > negateRight x = second not x
module Data.Bifunctor.Linear
  ( Bifunctor (..),
    SymmetricMonoidal (..),
  )
where

import Data.Bifunctor.Linear.Internal.Bifunctor
import Data.Bifunctor.Linear.Internal.SymmetricMonoidal
