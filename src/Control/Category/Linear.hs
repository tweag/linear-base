{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Category.Linear where

import qualified Prelude.Linear
import GHC.Exts(FUN)
import GHC.Types (Multiplicity(One))
import Control.Functor.Linear as Control
import Data.Profunctor.Kleisli.Linear (Kleisli(..))

class Category cat where
  id :: cat a a
  (.) :: cat b c %1 -> cat a b %1 -> cat a c
  infixr 9 .

-- | Left-to-right composition
(>>>) :: Category cat => cat a b %1 -> cat b c %1 -> cat a c
f >>> g = (Control.Category.Linear..) g f
infixr 1 >>>
{-# INLINE (>>>) #-}

-- | Right-to-left composition
(<<<) :: Category cat => cat b c %1 -> cat a b %1 -> cat a c
(<<<) = (Control.Category.Linear..)
infixr 1 <<<


instance Category (FUN 'One) where
  id = Prelude.Linear.id
  (.) = (Prelude.Linear..)


instance Monad m => Category (Kleisli m) where
    id = Kleisli return
    (Kleisli f) . (Kleisli g) = Kleisli (\b -> g b >>= f)
