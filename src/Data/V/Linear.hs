{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

-- | This module defines vectors of known length which can hold linear values.
--
-- Having a known length matters with linear types, because many common vector
-- operations (like zip) are not total with linear types.
--
-- Make these vectors by giving any finite number of arguments to 'make'
-- and use them with 'elim':
--
-- >>> :set -XLinearTypes
-- >>> :set -XTypeApplications
-- >>> :set -XTypeInType
-- >>> :set -XTypeFamilies
-- >>> import Prelude.Linear
-- >>> import qualified Data.V.Linear as V
-- >>> :{
--  doSomething :: Int %1-> Int %1-> Bool
--  doSomething x y = x + y > 0
-- :}
--
-- >>> :{
--  isTrue :: Bool
--  isTrue = V.elim doSomething (build 4 9)
--    where
--      -- GHC can't figure out this type equality, so this is needed.
--      build :: Int %1-> Int %1-> V.V 2 Int
--      build = V.make @2 @Int
-- :}
--
-- A much more expensive library of vectors of known size (including matrices
-- and tensors of all dimensions) is the [@linear@ library on
-- Hackage](https://hackage.haskell.org/package/linear) (that's /linear/ in the
-- sense of [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra),
-- rather than linear types).
module Data.V.Linear
  ( V,
    uncons,
    uncons#,
    elim,
    cons,
    fromReplicator,
    dupV,
    FunN,
    make,
    theLength,
    module Data.V.Linear.Internal.Instances,
  )
where

import Data.V.Linear.Internal
import Data.V.Linear.Internal.Instances ()

{- Developers Note

To avoid a common circular dependence, we moved the data type to
Data.V.Internal.Linear.V and moved the instances here. The common import issue
is as follows. Dupable depends on @V@ yet the instances of @V@ depend on
a variety of things (data functors, control functors, traversable) which
often end up depending on dupable. By moving the instances here, we
can make sure that Data.Unrestricted.Internal.Dupable only depends on the data
type defintion in Data.V.Linear.V and does not require any of the dependencies
of the instances.

Remark: ideally the instances below would be in an internal `Instances`
module. But we haven't got around to it yet.
-}
