# User Guide

This guide provides a minimal list of things you need to know to write
practical linear code with `linear-base`.

## Naming conventions & layout

Typically, variants of common haskell tools and facilities
share the same name with a `Linear` postfix. For instance,
`Data.Bool.Linear` provides the linear versions of `not`
and `&&`.

The module names follow the typical hierarchical module
naming scheme with top-level names like `Control`, `Data`, `System`
and so on.

## Temporary limitations

### Case statements are not linear

The following definition will **fail** to type check:

```haskell
maybeFlip :: Int #-> Int #-> (a,a) -> a
maybeFlip i j (x,y) = case i < j of
  True -> x
  False -> y
```

The scrutinee on (i.e., `x` in `case x of ...`) is considered to be
consumed many times. It's a limitation of the current implementation
of the type checker.

For now, we can mimic a linear case statement using the
`-XLambdaCase` language extension and the `(&)` from `Prelude.Linear`:

```haskell
{-# LANGUAGE LambdaCase #-}
import Prelude.Linear ((&))

maybeFlip :: Int #-> Int #-> (a,a) -> a
maybeFlip i j (x,y) =  i < j & \case
  True -> x
  False -> y
```

The `(&)` operator is like `($)` with the argument order flipped.

### `let` and `where` bindings are not linear

The following will **fail** to type check:

```haskell
idBad1 :: a #-> a
idBad1 x = y
  where
    y = x

idBad2 :: a #-> a
idBad2 x = let y = x in y
```

This is because GHC assumes that anything used in a `where`-binding or
`let`-binding is consumed with multiplicity `Many`. Workaround: inline
these bindings or use sub-functions.

```haskell
inlined1 :: a #-> a
inlined1 x = x

useSubfunction :: Array a #-> Array a
useSubfunction arr = fromRead (read arr 0)
  where
    fromRead :: (Array a, Ur a) #-> Array a
    fromRead = undefined
```

## Non-linear and linear code interactions

Throughout linear code, you will need to interface between non-linear
and linear code. All the tools you need to do this are in
[`Data.Unrestricted`] and are typically re-exported by
[`Prelude.Linear`].

This is basically done through type classes. Types that can be used in
a non-linear way even when they are bound in a linear function have
instances of one or more of these things: `Consumable, Dupable,
Moveable`.

## Design patterns

### `f :: X -> (SomeType #-> Ur b) #-> Ur b` functions

This function limits the **scope** of using `SomeType` by taking
a scope function of type `(SomeType #-> Ur b)`
as its second argument and using it with a value of type `SomeType` to
produce an `Ur b`.

The `SomeType` cannot escape by having it be inside the type `b` 
in some way. This is because the `SomeType` is bound linearly in the scope
function and `Ur` holds its value with a non-linear constructor
arrow.

Now, if `f` is the only function that can make a `SomeType`,
then we have an API that completely controls the creation-to-deletion
lifetime (i.e, the scope) of `SomeType` and ensures it is used linearly.

## New linear abstractions

Here's a list of new abstractions made possible by linear types:

1. Mutable arrays, hashmaps, vectors, sets with a pure API. See
   `Data.Array.Mutable.Linear`.
2. Push and Pull arrays: a way to control when arrays are allocated
   and force array fusion. See `Data.Array.Polarized`.
3. A linear API for system heap (not GC) allocation of values. See
   `Foreign.Marshall.Pure`.

[`Data.Unrestricted`]: ../src/Data/Unrestricted/Linear.hs
[`Prelude.Linear`]: ../src/Prelude/Linear.hs
