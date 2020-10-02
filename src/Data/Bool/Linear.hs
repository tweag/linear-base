{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides linear functions on the standard 'Bool' type.
module Data.Bool.Linear
  ( -- * The Boolean type
    Bool(..)
    -- * Operators
  , (&&)
  , (||)
  , not
  , otherwise
  )
  where

import Prelude (Bool(..), otherwise)

-- | @True@ iff both are @True@.
-- __NOTE:__ this is strict and not lazy!
(&&) :: Bool %1-> Bool %1-> Bool
False && False = False
False && True = False
True && x = x

infixr 3 &&

-- | @True@ iff either is @True@
-- __NOTE:__ this is strict and not lazy!
(||) :: Bool %1-> Bool %1-> Bool
True || False = True
True || True = True
False || x = x

infixr 2 ||

-- | @not b@ is @True@ iff b is @False@
-- __NOTE:__ this is strict and not lazy!
not :: Bool %1-> Bool
not False = True
not True = False
