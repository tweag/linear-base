# `Prelude` classes comparison between `base` and `linear-base`

<table>
<thead>
<tr>
<td>

**Class in `base` (non-linear)**

</td>
<td>

**Equivalent class(es) in `linear-base`**

</td>
</tr>
</thead>
<tbody>
<tr>
<td>

`Data.Eq (Eq)`, `Prelude (Eq)`

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

</td>
<td>

`Data.Eq.Linear (Eq)`, `Prelude.Linear (Eq)`

```haskell
class Eq a where
  (==) :: a %1 -> a %1 -> Bool
  (/=) :: a %1 -> a %1 -> Bool
```

</td>
</tr>
<tr>
<td>

`Data.Ord (Ord)`, `Prelude (Ord)`

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
```

</td>
<td>

`Data.Linear.Ord (Ord)`, `Prelude.Linear (Ord)`

```haskell
class Eq a => Ord a where
  compare :: a %1 -> a %1 -> Ordering
  (<=) :: a %1 -> a %1 -> Bool
  (<) :: a %1 -> a %1 -> Bool
  (>) :: a %1 -> a %1 -> Bool
  (>=) :: a %1 -> a %1 -> Bool
```

N.B.: `min` and `max` are available as functions (not methods) in the same module.

</td>
</tr>
<tr>
<td>

`GHC.Enum (Enum)`, `Prelude (Enum)`

```haskell
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (Enum)`

</td>
</tr>
<tr>
<td>

`GHC.Bounded (Enum)`, `Prelude (Bounded)`

```haskell
class Bounded a where
  minBound :: a
  maxBound :: a
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (Bounded)`

</td>
</tr>
<tr>
<td>

`GHC.Num (Num)`, `Prelude (Num)`

```haskell
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

</td>
<td>

`Data.Num.Linear (Num)`, `Prelude.Linear (Num)`

```haskell
class Additive a where
  (+) :: a %1 -> a %1 -> a
class Additive a => AddIdentity a where
  zero :: a
class AddIdentity a => AdditiveGroup a where
  negate :: a %1 -> a
  (-) :: a %1 -> a %1 -> a
class Multiplicative a where
  (*) :: a %1 -> a %1 -> a
class Multiplicative a => MultIdentity a where
  one :: a
class (AddIdentity a, MultIdentity a) => Semiring a
class (AdditiveGroup a, Semiring a) => Ring a
class FromInteger a where
  fromInteger :: Prelude.Integer %1 -> a

class (Ring a, FromInteger a) => Num a where
  abs :: a %1 -> a
  signum :: a %1 -> a
```

</td>
</tr>
<tr>
<td>

`GHC.Real (Real)`, `Prelude (Real)`

```haskell
class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (Real)`

N.B.: `Prelude.Linear (Real)` uses non-linear constraints `Num` and `Ord`.

</td>
</tr>
<tr>
<td>

`GHC.Real (Integral)`, `Prelude (Integral)`

```haskell
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (Integral)`

</td>
</tr>
<tr>
<td>

`GHC.Real (Fractional)`, `Prelude (Fractional)`

```haskell
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (Fractional)`

</td>
</tr>
<tr>
<td>

`GHC.Float (Floating)`, `Prelude (Floating)`

```haskell
class Fractional a => Floating a where
  pi :: a
  exp :: a -> a
  log :: a -> a
  sqrt :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a
  log1p :: a -> a
  expm1 :: a -> a
  log1pexp :: a -> a
  log1mexp :: a -> a
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (Floating)`

</td>
</tr>
<tr>
<td>

`GHC.Real (RealFrac)`, `Prelude (RealFrac)`

```haskell
class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (RealFrac)`

</td>
</tr>
<tr>
<td>

`GHC.Float (RealFloat)`, `Prelude (Float)`

```haskell
class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (RealFloat)`

</td>
</tr>
<tr>
<td>

`Data.Semigroup (Semigroup)`, `Prelude (Semigroup)`

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes :: Integral b => b -> a -> a
```

</td>
<td>

`Data.Monoid.Linear (Semigroup)`, `Prelude.Linear (Semigroup)`

```haskell
class Semigroup a where
  (<>) :: a %1 -> a %1 -> a
```

N.B.: Linear versions of `sconcat` and `stimes` are not available.

</td>
</tr>
<tr>
<td>

`Data.Moinoid (Monoid)`, `Prelude (Monoid)`

```haskell
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```

</td>
<td>

`Data.Monoid.Linear (Monoid)`, `Prelude.Linear (Monoid)`

```haskell
class Semigroup a => Monoid a where
  mempty :: a
```

N.B.: `mconcat` and `mappend` are available as functions (not methods) in the same module.

</td>
</tr>
<tr>
<td>

`Data.Functor (Functor)`, `Prelude (Functor)`

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
```

</td>
<td>

[**Data Functor**](https://www.tweag.io/blog/2020-01-16-data-vs-control#data-functors)

`Data.Functor.Linear (Functor)`

```haskell
class Data.Functor f where
  fmap :: (a %1 -> b) -> f a %1 -> f b
```

N.B.: `(<$)` is available as a function (not method) in the same module.

[**Control Functor**](https://www.tweag.io/blog/2020-01-16-data-vs-control#control-functors)

`Control.Functor.Linear (Functor)`

```haskell
class Data.Functor f => Control.Functor f where
  fmap :: (a %1 -> b) %1 -> f a %1 -> f b
```

N.B.: `(<$)` is available as a function (not method) in the same module.

</td>
</tr>
<tr>
<td>

`Control.Applicative (Applicative)`, `Prelude (Applicative)`

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
```

</td>
<td>

[**Data Applicative**](https://www.tweag.io/blog/2020-01-16-data-vs-control#data-functors)

`Data.Functor.Linear (Applicative)`

```haskell
class Data.Functor f => Data.Applicative f where
  pure :: a -> f a
  (<*>) :: f (a %1 -> b) %1 -> f a %1 -> f b
  liftA2 :: (a %1 -> b %1 -> c) -> f a %1 -> f b %1 -> f c
```

N.B.: a linear Data variant of `(<*)` is available as a function (not method) in `Prelude.Linear` with the following signature:

```haskell
(<*) :: (Data.Applicative f, Consumable b) => f a %1 -> f b %1 -> f a
```

`(*>)` is not available though.

[**Control Applicative**](https://www.tweag.io/blog/2020-01-16-data-vs-control#control-functors)

`Control.Functor.Linear (Applicative)`

```haskell
class (Data.Applicative f, Control.Functor f) => Control.Applicative f where
  pure :: a %1 -> f a
  (<*>) :: f (a %1 -> b) %1 -> f a %1 -> f b
  liftA2 :: (a %1 -> b %1 -> c) %1 -> f a %1 -> f b %1 -> f c
```

N.B.: linear Control variants of `(<*)` and `(*>)` are not available.

</td>
</tr>
<tr>
<td>

`Control.Monad (Monad)`, `Prelude (Monad)`

```haskell
class Applicative m => Monad m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
  (>>) :: forall a b. m a -> m b -> m b
  return :: a -> m a
```

</td>
<td>

`Control.Functor.Linear (Monad)`

```haskell
class Control.Applicative m => Monad m where
  (>>=) :: m a %1 -> (a %1 -> m b) %1 -> m b
  (>>) :: m () %1 -> m a %1 -> m a
```

N.B.: `return` is available as a function (not method) in the same module.

</td>
</tr>
<tr>
<td>

`Control.Monad.Fail (MonadFail)`, `Prelude (MonadFail)`

```haskell
class Monad m => MonadFail m where
  fail :: String -> m a
```

</td>
<td>

`Control.Functor.Linear (MonadFail)`

```haskell
class Monad m => MonadFail m where
  fail :: String -> m a
```

</td>
</tr>
<tr>
<td>

`Data.Foldable (Foldable)`, `Prelude (Foldable)`

```haskell
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: forall a. Ord a => t a -> a
  minimum :: forall a. Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
```

</td>
<td>

No linear counterpart available at the moment, but individual fold functions can be found in `Data.List.Linear`

</td>
</tr>
<tr>
<td>

`Data.Traversable (Traversable)`, `Prelude (Traversable)`

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
```

</td>
<td>

`Data.Functor.Linear (Traversable)`

```haskell
class Data.Functor t => Traversable t where
  traverse :: Control.Applicative f => (a %1 -> f b) -> t a %1 -> f (t b)
  sequence :: Control.Applicative f => t (f a) %1 -> f (t a)
```

N.B.: `sequenceA` and `mapM` are available as functions (not methods) in the same module.

</td>
</tr>
<tr>
<td>

`GHC.Show (Show)`, `Prelude (Show)`

```haskell
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (Show)`

</td>
</tr>
<tr>
<td>

`GHC.Read (Read)`, `Prelude (Read)`

```haskell
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
```

</td>
<td>

`Prelude` (non-linear) version reexported as `Prelude.Linear (Read)`

</td>
</tr>
<tr>
<td>

No non-linear counterpart available.

</td>
<td>

`Data.Unrestricted.Linear (Consumable)`, `Prelude.Linear (Consumable)`

```haskell
class Consumable a where
  consume :: a %1 -> ()
```

</td>
</tr>
<tr>
<td>

No non-linear counterpart available.

</td>
<td>

`Data.Unrestricted.Linear (Dupable)`, `Prelude.Linear (Dupable)`

```haskell
class Consumable a => Dupable a where
  dupR :: a %1 -> Replicator a
  dup2 :: a %1 -> (a, a)
```

</td>
</tr>
<tr>
<td>

No non-linear counterpart available.

</td>
<td>

`Data.Unrestricted.Linear (Movable)`, `Prelude.Linear (Movable)`

```haskell
class Dupable a => Movable a where
  move :: a %1 -> Ur a
```

</td>
</tr>
</tbody>
</table>
