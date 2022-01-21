{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Arity.Linear.Internal.Arity (Arity) where

import GHC.TypeLits

type family Arity b f where
  Arity b b = 0
  Arity b (a %1 -> f) = Arity b f + 1
  Arity _ _ = TypeError ('Text "The given object of type f is not a well-formed function")
