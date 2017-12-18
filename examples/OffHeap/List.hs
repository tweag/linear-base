{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OffHeap.List where

import qualified Data.OffHeap as Manual
import Data.OffHeap (Pool, Box)
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple
import Prelude.Linear

data List a
  = Nil
  | Cons a (Box (List a))

-- TODO: generating appropriate instances using the Generic framework
instance Storable a => Storable (List a) where
  sizeOf _ = sizeOf (undefined :: (Word8, a, Box (List a)))
  alignment _ = alignment (undefined :: (Word8, a, Box (List a)))

  peek ptr = do
    (tag, a, l) <- peek (castPtr ptr :: Ptr (Word8, a, Box (List a)))
    case tag of
      0 -> return Nil
      1 -> return $ Cons a l
      _ -> error "Storable (List a): peek: unknown tag"

  poke ptr Nil = poke (castPtr ptr :: Ptr Word8) 0
  poke ptr (Cons a l) = poke (castPtr ptr :: Ptr (Word8, a, Box (List a))) (1, a, l)
