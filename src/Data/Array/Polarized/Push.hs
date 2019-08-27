{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

-- | This module is intended to be imported qualified, e.g.
-- > import qualified Data.Array.Polarized.Push as Push
module Data.Array.Polarized.Push where

-- XXX: there is lots of scope for testing array functions

import Data.Array.Destination (DArray)
import qualified Data.Array.Destination as DArray
import qualified Data.Functor.Linear as Data
import Data.Vector (Vector)
import Prelude.Linear
import qualified Prelude
import Data.Monoid.Linear

data Array a where
  Array :: (forall b. (a ->. b) -> DArray b ->. ()) ->. Int -> Array a
  deriving Prelude.Semigroup via NonLinear (Array a)
  -- A note on implementation: `exists b. ((a -> b), DArray b)` adjoins freely
  -- the structure of contravariant functor to `DArray`. Because it appears to
  -- the left of an arrow, we can curry the existential quantification (and,
  -- less crucially, the pair) so that we can spare an extra type definition.
  --
  -- An invariant is kept that, really, the first parameter is only passed
  -- arrays of size @n@ (the second parameter, which is the length of the
  -- array).
  --
  -- TODO: Consider changing DArray b ->. () to something more general

instance Data.Functor Array where
  fmap f (Array k n) = Array (\g dest -> k (g . f) dest) n

instance Semigroup (Array a) where
  (<>) = append

-- XXX: the use of Vector in the type of alloc is temporary (see also "Data.Array.Destination")
alloc :: Array a ->. Vector a
alloc (Array k n) = DArray.alloc n (k id)

make :: a -> Int -> Array a
make x n = Array (\k -> DArray.replicate (k x)) n

append :: Array a ->. Array a ->. Array a
append (Array kl nl) (Array kr nr) =
    Array
      (\f dest -> parallelApply f kl kr (DArray.split nl dest))
      (nl+nr)
  where
    parallelApply :: (a ->. b) -> ((a ->. b) -> DArray b ->. ()) ->. ((a ->. b) -> DArray b ->. ()) ->. (DArray b, DArray b) ->. ()
    parallelApply f' kl' kr' (dl, dr) = kl' f' dl <> kr' f' dr
