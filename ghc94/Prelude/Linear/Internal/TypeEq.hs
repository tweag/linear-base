{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | As of GHC 9.4, @~@ is a type operator exported from `Data.Type.Equality`
-- rather than a language construct. As a temporary workaround to enable
-- compilation on both GHC 9.4 and 9.2, this module re-exports the new type
-- operator, while the 9.2 version is empty. As a result, files which depend
-- on this module will likely have -Wno-unused-imports enabled (and potentially
-- also -Wno-dodgy exports if they re-export it). These should be removed once
-- support for GHC 9.2 is dropped.
module Prelude.Linear.Internal.TypeEq
  ( type (~),
  )
where

import Data.Type.Equality (type (~))
