{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Trustworthy #-} -- for GHC.Types
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Unrestricted.Linear.Internal.Ur
  ( Ur (..),
    unur,
    lift,
    lift2,
  )
where

import Generics.Linear
import qualified GHC.Generics as GHCGen
import Prelude.Linear.GenericUtil
import GHC.Types (Multiplicity (..))
import qualified Prelude

-- | @Ur a@ represents unrestricted values of type @a@ in a linear
-- context. The key idea is that because the contructor holds @a@ with a
-- regular arrow, a function that uses @Ur a@ linearly can use @a@
-- however it likes.
--
-- > someLinear :: Ur a %1-> (a,a)
-- > someLinear (Ur a) = (a,a)
data Ur a where
  Ur :: a -> Ur a

deriving instance GHCGen.Generic (Ur a)
deriving instance GHCGen.Generic1 Ur

-- | Get an @a@ out of an @Ur a@. If you call this function on a
-- linearly bound @Ur a@, then the @a@ you get out has to be used
-- linearly, for example:
--
-- > restricted :: Ur a %1-> b
-- > restricted x = f (unur x)
-- >   where
-- >     -- f __must__ be linear
-- >     f :: a %1-> b
-- >     f x = ...
unur :: Ur a %1 -> a
unur (Ur a) = a

-- | Lifts a function on a linear @Ur a@.
lift :: (a -> b) -> Ur a %1 -> Ur b
lift f (Ur a) = Ur (f a)

-- | Lifts a function to work on two linear @Ur a@.
lift2 :: (a -> b -> c) -> Ur a %1 -> Ur b %1 -> Ur c
lift2 f (Ur a) (Ur b) = Ur (f a b)

instance Prelude.Functor Ur where
  fmap f (Ur a) = Ur (f a)

instance Prelude.Foldable Ur where
  foldMap f (Ur x) = f x

instance Prelude.Traversable Ur where
  sequenceA (Ur x) = Prelude.fmap Ur x

instance Prelude.Applicative Ur where
  pure = Ur
  Ur f <*> Ur x = Ur (f x)

instance Prelude.Monad Ur where
  Ur a >>= f = f a

-- -------------------
-- Generic and Generic1 instances

instance Generic (Ur a) where
  type Rep (Ur a) = FixupMetaData (Ur a)
         (D1 Any
           (C1 Any
             (S1 Any
               (MP1 'Many (Rec0 a)))))
  to rur = to' rur
    where
      to' :: Rep (Ur a) p %1-> Ur a
      to' (M1 (M1 (M1 (MP1 (K1 a))))) = Ur a

  from ur = from' ur
    where
      from' :: Ur a %1-> Rep (Ur a) p
      from' (Ur a) = M1 (M1 (M1 (MP1 (K1 a))))

instance Generic1 Ur where
  type Rep1 Ur = FixupMetaData1 Ur
         (D1 Any
            (C1 Any
               (S1 Any
                  (MP1 'Many Par1))))

  to1 rur = to1' rur
    where
      to1' :: Rep1 Ur a %1-> Ur a
      to1' (M1 (M1 (M1 (MP1 (Par1 a))))) = Ur a

  from1 ur = from1' ur
    where
      from1' :: Ur a %1-> Rep1 Ur a
      from1' (Ur a) = M1 (M1 (M1 (MP1 (Par1 a))))

type family Any :: Meta
