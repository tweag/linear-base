{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
{-# OPTIONS_HADDOCK hide #-}

module Compact.Destination.Fill where

import Compact.Destination.Internal
import Compact.Destination.GFill
import Data.Kind (Type)
import GHC.Compact (Compact (..))
import GHC.MVar (MVar (..))
import GHC.Exts
import Unsafe.Linear (toLinear)

class Fill lCtor (a :: Type) where
  _fill :: forall (r :: Type). (Region r) => Dest r a -> DestsOf lCtor r a

instance (specCtor ~ LiftedCtorToSpecCtor lCtor a, GFill# lCtor specCtor a) => Fill lCtor a where
  _fill :: forall (r :: Type). (Region r) => Dest r a -> DestsOf lCtor r a
  _fill (Dest d#) = case getRegionInfo @r of
    (RegionInfo (Compact c# _ (MVar m#))) -> case runRW# (gFill# @lCtor @specCtor @a @r c# m# d#) of (# _, res #) -> res
  {-# INLINE _fill #-}

fill :: forall lCtor (r :: Type) (a :: Type). (Fill lCtor a, Region r) => Dest r a %1 -> DestsOf lCtor r a
fill = toLinear (_fill @lCtor @a @r)
{-# INLINE fill #-}