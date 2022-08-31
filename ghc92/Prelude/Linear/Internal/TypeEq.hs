-- | As of GHC 9.4, @~@ is a type operator exported from `Data.Type.Equality`
-- rather than a language construct. As a temporary workaround to enable
-- compilation on both GHC 9.4 and 9.2, this module is empty, while the GHC
-- 9.4 version re-exports the new type  operator. As a result, files which
-- depend on this module will likely have -Wno-unused-imports enabled (and
-- potentially also -Wno-dodgy exports if they re-export it). These should be
-- removed once support for GHC 9.2 is dropped.
module Prelude.Linear.Internal.TypeEq where
