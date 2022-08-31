{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Linear.Generically
  ( unGenerically,
    unGenerically1,
    module Prelude.Linear.Internal.Generically,
  )
where

import Prelude.Linear.Internal.Generically

unGenerically :: Generically a %1 -> a
unGenerically (Generically a) = a

unGenerically1 :: Generically1 f a %1 -> f a
unGenerically1 (Generically1 fa) = fa
