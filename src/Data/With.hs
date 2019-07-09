{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Data.With where

import Prelude.Linear
import qualified Data.Functor.Linear as Data

-- | The @(,)@ constructor represents multiplicative conjunction, typically
-- represented in linear logic using ⊗. Given a value of type @(a, b)@, to
-- consume it we must consume both consituent parts. This means, for instance,
-- that
-- @fst :: (a, b) ->. a@
-- is not well typed. Similarly to output a value of type @(a, b)@, we must
-- produce both values simultaneously.
--
-- The operator & of linear logic instead represents additive conjunction,
-- meaning an alternative occurence. Consuming a value of type @a & b@
-- means consuming /either/ a value of type @a@, or a value of type @b@, at
-- the consumer's choice. (This is in contrast to @Either a b@, in which it
-- is at the producer's choice).
--
-- For instance, @fst :: a & b ->. a@ can be made well-typed (and the linear
-- implication @a & b -o a@ holds), while @(a ->. b ->. c) -> (a & b ->. c)@
-- cannot be, since to produce a value of type @c@ we must use both an @a@ and
-- a @b@.
--
-- See 'A taste of linear logic' by Phil Wadler
-- (https://homepages.inf.ed.ac.uk/wadler/papers/lineartaste/lineartaste-revised.pdf)
-- start of section 4.1 for a real-life example.
--
--
-- I don't yet have a satisfactory implementation of this type, but a rough
-- potential API is given here, with possible implementations below.

data a & b
type With a b = a & b

-- These three are essentially the defining property of (&). (It is the
-- product in the category of linear types.)
-- In particular, (,) would not work in place of any of these.

fst :: a & b ->. a
fst = undefined
snd :: a & b ->. b
snd = undefined
with :: (x ->. a) -> (x ->. b) -> x ->. a & b
with = undefined

-- The consumer of @a & b@ gets to choose which component they wish to consume.
consume :: Either (a ->. c) (b ->. c) ->. a & b ->. c
consume = undefined

-- Helper functions. Note `pairWith` does not require a Dupable instance for
-- `a`. Also note `pairWith` and `both` suffice to define `with` (in fact,
-- `sym`, `pairWith` and a Functor instance for @((&) a)@ suffice).
-- Conversely, `with` suffices to define all three functions here.
sym :: a & b ->. b & a
sym = undefined
pairWith :: a ->. a & a
pairWith = undefined
both :: (a ->. b) -> (c ->. d) -> a & c ->. b & d
both = undefined

-- These witness the isomorphism !(A & B) == (!A ⊗ !B).
toPair :: Unrestricted (a & b) -> (Unrestricted a, Unrestricted b)
toPair = undefined
fromPair :: (Unrestricted a, Unrestricted b) -> Unrestricted (a & b)
fromPair = undefined


-- * Implementations

-- ** Here is one possible implementation of (&).
data With1 a b where
  With1 :: (x ->. a) -> (x ->. b) -> x ->. With1 a b

instance (Eq a, Eq b) => Eq (With1 a b) where
  With1 f g x == With1 h k y = f x == h y && g x == k y

with1 :: (x ->. a) -> (x ->. b) -> x ->. With1 a b
with1 = With1

fst1 :: With1 a b ->. a
fst1 (With1 f _ t) = f t

snd1 :: With1 a b ->. b
snd1 (With1 _ g t) = g t

sym1 :: With1 a b -> With1 b a
sym1 (With1 f g x) = With1 g f x

assoc1 :: With1 a (With1 b c) ->. With1 (With1 a b) c
assoc1 (With1 f g x) = with1 (with1 f (fst1 . g)) (snd1 . g) x

pairWith1 :: a ->. With1 a a
pairWith1 x = With1 id id x

both1 :: (a ->. b) -> (c ->. d) -> With1 a c ->. With1 b d
both1 f g (With1 h k t) = With1 (f . h) (g . k) t

toPair1 :: Unrestricted (With1 a b) -> (Unrestricted a, Unrestricted b)
toPair1 (Unrestricted (With1 f g x)) = (Unrestricted (f x), Unrestricted (g x))

fromPair1 :: (Unrestricted a, Unrestricted b) -> Unrestricted (With1 a b)
fromPair1 (Unrestricted x, Unrestricted y) = Unrestricted (With1 (\() -> x) (\() -> y) ())

fromTensor1 :: (a, b) -> With1 a b
fromTensor1 (a, b) = With1 (\() -> a) (\() -> b) ()

instance Data.Functor (With1 a) where
  fmap h (With1 f g x) = with1 f (h . g) x
