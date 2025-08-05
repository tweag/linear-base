{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Linear versions of 'Data.Complex' functions.
--
-- Consult the original "Data.Complex" module for more detailed information.
module Data.Complex.Linear
    ( realPart
    , imagPart
    , conjugate
    , NonLinear.mkPolar
    , NonLinear.cis
    , NonLinear.polar
    , NonLinear.magnitude
    , NonLinear.phase
    ) where

import Data.Complex (Complex ((:+)))
import qualified Data.Complex as NonLinear
import Data.Num.Linear
import Data.Unrestricted.Linear

realPart :: Consumable a => Complex a %1 -> a
realPart (x :+ y) = lseq y x

imagPart :: Consumable a => Complex a %1 -> a
imagPart (x :+ y) = lseq x y

conjugate :: AdditiveGroup a => Complex a %1 -> Complex a
conjugate (x :+ y) =  x :+ (negate y)
