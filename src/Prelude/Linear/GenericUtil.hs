{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @'FixupMetaData' a g@ copies the metadata from the
-- @"GHC.Generics".'GHC.Generics.Generic'@ representation of @a@ to the
-- representation @g@.
--
-- @'FixupMetaData1' f g@ does something similar when @f 'Any'@ is an instance
-- of @Generic@ and @g@ is a @Rep1@. See the individual type documentation
-- for details.
--
-- This is intended to help users instantiate 'Rep' and 'Rep1' for types with
-- nonlinear or multiplicity-polymorphic fields.
--
-- == Suggested use
--
-- You will need to derive a @"GHC.Generics.Generic"@ instance for the
-- type. This is used to obtain the correct metadata.
--
-- Next you need to construct a @Rep@ and/or @Rep1@ for your type, ignoring the
-- metadata.
--
-- Constructing the actual representations can be a bit annoying, but GHC can
-- help.
--
-- === For 'Rep'
--
-- Once you have derived  @"GHC.Generics".'GHC.Generics.Generic'@ for your
-- type, define a value like
--
-- @
-- test :: Rep T a
-- test = _
-- @
--
-- Then compile. The stripped representation you need will be in the error
-- message.
--
-- === For 'Rep1'
--
-- Construct a type with the same shape as the one you wish to
-- instantiate, but with only linear fields. Strictness annotations
-- and @UNPACK@ pragmas are irrelevant here.
--
-- Instantiate @"Generics.Linear".'Generic1'@ for the lookalike using
-- 'Generics.Linear.TH.deriveGeneric1' and follow the same procedure
-- as above (but with @'Rep1' T@, of course) to get a metadata-stripped
-- representation.
--
-- === For either
--
-- To avoid confusion, replace at least the package and module names in the
-- representation with 'Any'. Wrap @MP1@ around any nonlinear/representation
-- polymorphic fields, just under the @S1@ type constructor. The first type
-- argument of @MP1@ will indicate the multiplicity.
module Prelude.Linear.GenericUtil
  ( FixupMetaData,
    FixupMetaData1,
    RemoveMetaData,
  )
where

import Data.Kind (Type)
import qualified GHC.Generics
import GHC.TypeLits
import Generics.Linear

-- | @FixupMetaData a g@ copies the metadata from the
-- @"GHC.Generics".'GHC.Generics.Generic'@ representation of @a@ to the
-- representation @g@. It also checks that the structure of @Rep a@ is the
-- same as @g@, except that @g@ may have @MP1@ applications under some @S1@
-- constructors.
--
-- === Example
--
-- @
-- instance 'Generic' ('Prelude.Linear.Ur' a) where
--   type Rep (Ur a) = FixupMetaData (Ur a)
--         (D1 Any
--           (C1 Any
--             (S1 Any
--               (MP1 \'Many (Rec0 a)))))
-- @
type FixupMetaData (a :: Type) (g :: Type -> Type) =
  Fixup (GHC.Generics.Rep a) g

-- | @FixupMetaData1 f g@ copies the metadata from the
-- @"GHC.Generics".'GHC.Generics.Generic'@ representation of @f 'Any'@
-- to the representation @g@. It also checks that the overall structure of
-- @Rep (f 'Any')@ is the same as @g@, but does not check that their fields
-- match.
--
-- === Example
--
-- @
-- instance 'Generic1' 'Prelude.Linear.Ur' where
--   type Rep1 Ur = FixupMetaData1 Ur
--          (D1 Any
--             (C1 Any
--                (S1 Any
--                   (MP1 \'Many Par1))))
-- @
type FixupMetaData1 (f :: k -> Type) (g :: k -> Type) =
  Fixup1 (GHC.Generics.Rep (f Any)) g

type family Fixup (f :: Type -> Type) (g :: Type -> Type) :: Type -> Type where
  Fixup (D1 c f) (D1 _c g) = D1 c (Fixup f g)
  Fixup (C1 c f) (C1 _c g) = C1 c (Fixup f g)
  Fixup (S1 c f) (S1 _c (MP1 m f)) = S1 c (MP1 m f)
  Fixup (S1 c f) (S1 _c f) = S1 c f
  Fixup (f :*: g) (f' :*: g') = Fixup f f' :*: Fixup g g'
  Fixup (f :+: g) (f' :+: g') = Fixup f f' :+: Fixup g g'
  Fixup V1 V1 = V1
  Fixup _ _ = TypeError ('Text "FixupMetaData: representations do not match.")

type family Fixup1 (f :: Type -> Type) (g :: k -> Type) :: k -> Type where
  Fixup1 (D1 c f) (D1 _c g) = D1 c (Fixup1 f g)
  Fixup1 (C1 c f) (C1 _c g) = C1 c (Fixup1 f g)
  Fixup1 (f :*: g) (f' :*: g') = Fixup1 f f' :*: Fixup1 g g'
  Fixup1 (f :+: g) (f' :+: g') = Fixup1 f f' :+: Fixup1 g g'
  Fixup1 (S1 c _f) (S1 _c g) = S1 c g
  Fixup1 V1 V1 = V1
  Fixup1 _ _ = TypeError ('Text "Fixup1MetaData1: representations do not match.")

type family RemoveMetaData (f :: k -> Type) :: k -> Type where
  RemoveMetaData (D1 _c f) = D1 Any (RemoveMetaData f)
  RemoveMetaData (C1 _c f) = C1 Any (RemoveMetaData f)
  RemoveMetaData (S1 _c f) = S1 Any f
  RemoveMetaData (f :*: g) = RemoveMetaData f :*: RemoveMetaData g
  RemoveMetaData (f :+: g) = RemoveMetaData f :+: RemoveMetaData g
  RemoveMetaData x = x

type family Any :: k
