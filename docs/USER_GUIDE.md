# User Guide

This short guide assumes
familiarity with linear types (see the [`README`] for resources about linear types
if you are unfamiliar).

#### Table of contents

  1. [How to navigate the library](#navigating-the-library)
  2. [Core concepts you need to know](#core-concepts)
  3. [Current limitations](#temporary-limitations)

## Navigating the library

 * The [`Prelude.Linear`] module is a good place to start. It is a prelude for
 Haskell programs that use `-XLinearTypes` and is meant to replace the original
 prelude from `base`.
 * Mutable data with a pure API.
   Consider looking at `Data.{Array, Hashmap, Vector, Set}.Mutable.Linear`.
 * A linear `IO` monad is in `System.IO.Linear`.
   * A variant of linear `IO` which lets you enforce resource safety
     can be found in `System.IO.Resource`.
 * Streams in the style of the [`streaming`
   library](https://hackage.haskell.org/package/streaming) is in
   `Streaming.Linear` and `Streaming.Prelude.Linear`.

There are many other modules of course but a lot of the ones not already listed
are still experimental, such as system-heap memory management in `Foreign.Marshall.Pure`.

### Naming conventions & layout

Typically, variants of common Haskell tools and facilities
share the same name with a `Linear` postfix. For instance,
`Data.Bool.Linear` provides the linear versions of `not`
and `&&`.

The module names follow the typical hierarchical module
naming scheme with top-level names like `Control`, `Data`, `System`
and so on.


## Core concepts

### Using values multiple times

Frequently enough, you will want to consume a linear value, or maybe
use it multiple time. The basic tools you need to do this are in
[`Data.Unrestricted`] and are typically re-exported by
[`Prelude.Linear`].

Interfacing linear code with regular Haskell is done, for instance, through the type `Ur`.
The data type `Ur`, short for _unrestricted_ lets you store an
unrestricted value inside a linear value.

### Import Conventions

We've designed `linear-base` to work nicely with the following import conventions:

- `import qualified Data.Functor.Linear as Data`
- `import qualified Control.Functor.Linear as Control`

### Importing linear and non-linear code

Most modules with `{-# LANGUAGE LinearHaskell #-}` will want to have a mix of
linear and non-linear code and, for example, import linear modules like
`Data.Functor.Linear` and unrestricted modules from `base` like `Data.List`.
The pattern we've followed internally is to import the non-linear module
qualified. For instance:

```haskell
import Prelude.Linear
import Data.Functor.Linear
import qualified Prelude as NonLinear
import Data.List as List
```

Sometimes it's easier to use `forget :: (a %1-> b) -> (a -> b)` from
`Prelude.Linear` than to import the non-linear version of some function.
This is useful in passing linear functions to higher order functions.
For non HOF uses, we can use linear functions directly; given a linear function
`f`, we can always write `g x = f x` for `g :: A -> B`.


### `f :: X -> (SomeType %1-> Ur b) %1-> Ur b` functions

This style function is used throughout `linear-base`, particularly
with mutable data structures.

It serves to limit the **scope** of using `SomeType` by taking
a function of type `(SomeType %1-> Ur b)`
as its second argument and using it with a value of type `SomeType` to
produce an `Ur b`. We call this function of type `(SomeType %1-> Ur b)`,
a **scope function** or just **scope** for short.

The `SomeType` cannot escape the scope function by being inside the type `b`
in some way. This is because the `SomeType` is bound linearly in the scope
function and `Ur` can only contain unrestricted (in particular not linear)
values. At any nested level, the `SomeType` would have to be used in an
unrestricted way.

Now, if `f` is the only function that can make a `SomeType`,
then we have an API that completely controls the creation-to-deletion
lifetime (i.e, the scope) of `SomeType`.


## Temporary limitations

### Case statements are not linear

The following definition will **fail** to type check:

```haskell
maybeFlip :: Int %1-> Int %1-> (a,a) -> a
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

maybeFlip :: Int %1-> Int %1-> (a,a) -> a
maybeFlip i j (x,y) =  i < j & \case
  True -> x
  False -> y
```

The `(&)` operator is like `($)` with the argument order flipped.

### `let` and `where` bindings are not linear

The following will **fail** to type check:

```haskell
idBad1 :: a %1-> a
idBad1 x = y
  where
    y = x

idBad2 :: a %1-> a
idBad2 x = let y = x in y
```

This is because GHC assumes that anything used in a `where`-binding or
`let`-binding is consumed with multiplicity `Many`. Workaround: inline
these bindings or use sub-functions.

```haskell
inlined1 :: a %1-> a
inlined1 x = x

useSubfunction :: Array a %1-> Array a
useSubfunction arr = fromRead (read arr 0)
  where
    fromRead :: (Array a, Ur a) %1-> Array a
    fromRead = undefined
```

[`Data.Unrestricted`]: ../src/Data/Unrestricted/Linear.hs
[`Prelude.Linear`]: ../src/Prelude/Linear.hs
[`README`]: ../README.md
