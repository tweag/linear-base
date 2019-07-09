{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Data.With where

import Prelude.Linear hiding (fst, snd)
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

data a & b where
  With :: (x ->. a) -> (x ->. b) -> x ->. With a b
-- Somewhat surprisingly, the constructor With can be safely exported.

type With a b = a & b

-- These three are essentially the defining property of (&). (It is the
-- product in the category of linear types.)
-- In particular, (,) would not work in place of any of these.

fst :: a & b ->. a
fst (With f _ t) = f t

snd :: a & b ->. b
snd (With _ g t) = g t

with :: (x ->. a) -> (x ->. b) -> x ->. a & b
with = With

-- The consumer of @a & b@ gets to choose which component they wish to consume.
consume :: Either (a ->. c) (b ->. c) ->. a & b ->. c
consume (Left f)  (With h _ t) = f (h t)
consume (Right g) (With _ k t) = g (k t)

-- * Helper functions
--
-- Note `pairWith` does not require a Dupable instance for `a`. Also
-- note `pairWith` and `both` suffice to define `with` (in fact, `sym`,
-- `pairWith` and a Functor instance for @((&) a)@ suffice).  Conversely,
-- `with` suffices to define `sym`, `pairWith` and `both`.
sym :: a & b ->. b & a
sym (With f g x) = With g f x

pairWith :: a ->. a & a
pairWith x = With id id x

both :: (a ->. b) -> (c ->. d) -> a & c ->. b & d
both f g (With h k t) = With (f . h) (g . k) t

assoc :: a & (b & c) ->. (a & b) & c
assoc (With f g x) = with (with f (fst . g)) (snd . g) x

-- * Converting from normal pairs.

-- One of `a` and `b` is not consumed, so this is non-linear.
-- In particular, in the presence of weakening, tensor is
-- stronger than &.
-- (See 1.2.1.Weakening Linear logic: syntax and semantics (Girard))
fromTensor :: (a, b) -> a & b
fromTensor (a, b) = With (\() -> a) (\() -> b) ()

-- @x@ is consumed twice, so this is non-linear. In particular, if we're
-- allowed to consume @a & b@ non-linearly, it's the same as a normal
-- pair @(a, b)@.
-- In particular, in the presence of contraction, & is
-- stronger than tensor.
-- (See 1.2.1.Contraction Linear logic: syntax and semantics (Girard))
toTensor :: a & b -> (a, b)
toTensor (With f g x) = (f x, g x)

-- Recall that !(!A ⊗ !B) == !(A ⊗ B).

-- These witness the isomorphism !(A & B) == (!A ⊗ !B).
-- (BM: `fromPair . toPair` isn't strictly `id`, but I'm pretty sure they don't
-- differ in any observable way...)
toPair :: Unrestricted (a & b) -> (Unrestricted a, Unrestricted b)
toPair (Unrestricted (With f g x)) = (Unrestricted (f x), Unrestricted (g x))

fromPair :: (Unrestricted a, Unrestricted b) -> Unrestricted (a & b)
fromPair (Unrestricted x, Unrestricted y) =
  Unrestricted (With (\() -> x) (\() -> y) ())

-- This is just as the Eq (a, b) instance would be: the consumption is non-linear.
instance (Eq a, Eq b) => Eq (a & b) where
  With f g x == With h k y = f x == h y && g x == k y

-- There is no Control.Functor instance, nor a Data.Applicative one.
-- There would be a Data.Bifunctor though, as witnessed by `both`.
instance Data.Functor ((&) a) where
  fmap h (With f g x) = with f (h . g) x

-- Miscellaneous comments:
--
-- It's tempting to normalise With in such a way that fromPair . toPair always
-- holds, i.e. insist that the stored value is just () in every case. But
-- this may not be possible if the `With` was constructed using a non-Dupable
-- `x`.
--
--
-- It's nice to have this type, and know that it's possible with linear
-- Haskell, but I'm struggling to think of a usecase.
-- In particular, if one would like to export a function of type a ->. b & c,
-- you might as well export both a ->. b and a ->. c. Similarly, if you'd
-- like to export a & b ->. c, your function can only use one of `a` or `b`,
-- so you might as well export either a ->. c or b ->. c.
--
-- Perhaps one would like to have IO (a & b) to represent access to one
-- of the resources a, b which cannot occur simultaneously.

uselessnessIso :: (a ->. With b c) -> (a ->. b, a ->. c)
uselessnessIso f = (fst . f, snd . f)
-- inverse of `with`
