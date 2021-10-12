{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | An ergonomic class for unsatisfiable constraints. This is based on
-- the @trivial-constraint@ package and the
-- <https://github.com/adamgundry/ghc-proposals/blob/unsatisfiable/proposals/0000-unsatisfiable.rst Unsatisfiable proposal>
-- Once that proposal is implemented, we can use it.

module Prelude.Linear.Unsatisfiable
  ( Unsatisfiable
  , unsatisfiable
  , Bottom
  ) where
import GHC.Exts (Any, TYPE)
import GHC.TypeLits (TypeError, ErrorMessage)

-- The 'Any' constraint prevents anyone from instantiating 'Bottom' with
-- unsatisfiable' = undefined if they don't understand what it's for.

-- | A constraint that cannot be satisfied. Users should normally use
-- 'Unsatisfiable' instead of using this class directly.
class Any => Bottom where
  unsatisfiable' :: a

-- | An unsatisfiable constraint with a user-provided error message.  Under an
-- @Unsatisfiable@ constraint, users can use 'unsatisfiable' to get a value of
-- any type (and runtime representation) they desire. For example,
--
-- @
-- instance Unsatisfiable
--   (\'Text \"V1 cannot have an Applicative instance because it cannot implement pure\")
--     => Applicative V1 where
--   pure = unsatisfiable
--   (<*>) = unsatisfiable
-- @
class (Bottom, TypeError e) => Unsatisfiable (e :: ErrorMessage)

-- | Produce a value of any type (and runtime representation) under
-- an 'Unsatisfiable' or 'Bottom' constraint.
unsatisfiable :: forall {rep} (a :: TYPE rep). Bottom => a
unsatisfiable = unsatisfiable' (##)
