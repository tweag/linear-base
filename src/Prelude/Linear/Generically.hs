{-# LANGUAGE LinearTypes #-}

module Prelude.Linear.Generically
  (
    unGenerically,
    unGenerically1,
    module Prelude.Linear.Generically.Types
  ) where

import Prelude.Linear.Generically.Types

unGenerically :: Generically a %1 -> a
unGenerically (Generically a) = a

unGenerically1 :: Generically1 f a %1 -> f a
unGenerically1 (Generically1 fa) = fa
