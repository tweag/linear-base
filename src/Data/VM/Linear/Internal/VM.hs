{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.VM.Linear.Internal.VM where

import Data.Kind (Type)
import Data.Type.Equality
import GHC.TypeLits
import Prelude.Linear.Internal
import Prelude (Either(..) , error)
import Data.LArray.Mutable.Unlifted.Linear (LArray#)
import qualified Data.LArray.Mutable.Unlifted.Linear as LArray
import Data.Unrestricted.Internal.Ur
import Data.V.Linear.Internal.V (theLength, caseNat, predNat, FunN, expandFunN,
                                 contractFunN, continue, Dict (..))

{- Developers Note

See the "Developers Note" in Data.V.Linear for an explanation of this module
structure.
-}

-- # Type Definitions
-------------------------------------------------------------------------------

data VM (n :: Nat) (a :: Type) = VM (LArray# a)

-- # API
-------------------------------------------------------------------------------

consumeV :: VM 0 a %1-> b %1-> b
consumeV (VM arr) b =
  LArray.map (error "impossible: arr should be empty" :: a %1-> ()) arr
    `LArray.lseq` b

{-
split :: 1 <= n => V n a %1-> (# a, V (n-1) a #)
split = Unsafe.toLinear split'
  where
    split' :: 1 <= n => V n a -> (# a, V (n-1) a #)
    split' (V xs) = (# Vector.head xs, V (Vector.tail xs) #)
-}

make :: forall n a b. KnownNat n => FunN n a ((VM n a %1-> Ur b) %1-> Ur b)
make =
  continue
    @n @a @[a] @((VM n a %1-> Ur b) %1-> Ur b)
    (\xs f -> LArray.fromList xs (\arr -> f (VM arr)))
    (go @n @a)
 where
  go :: forall n' a'. KnownNat n' => FunN n' a' [a']
  go =
    case caseNat @n' of
      Left Refl -> []
      Right Refl ->
        contractFunN @n' @a' @[a'] (\a ->
          case predNat @n' of
            Dict -> continue @(n'-1) @a' @([a']) (a:) (go @(n'-1) @a'))

update :: forall ix n a b. (KnownNat ix, ix <= n - 1) => (a %1-> (b, a)) %1-> VM n a %1-> (b, VM n a)
update f (VM arr) =
  (\(# b, arr' #) -> (b, VM arr'))
    (LArray.update (theLength @ix) f arr)

elim :: forall n a b. KnownNat n => VM n a %1-> FunN n a b %1-> b
elim (VM arr) f = go @n @a @b (LArray.toList arr) f
 where
  -- invariant: length xs == n
  go :: forall n' a' b'. KnownNat n' => [a'] %1-> FunN n' a' b' %1-> b'
  go xs' g =
    (caseNat @n', xs') & \case
      (Left Refl, []) -> g
      (Right Refl, x:xs) ->
        predNat @n' & \Dict ->
          go @(n'-1) xs (expandFunN @n' @a' @b' g x)
      other -> error "invariant violation" other g

toList :: VM n a %1-> [a]
toList (VM arr) = LArray.toList arr
