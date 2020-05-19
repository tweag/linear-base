# Design

The purpose of this document is to describe how `linear-base` is developed.
_Please be familiar with the user guide before reading this document._

This document describes

 * the overall architecture of linear base
 * the general implementation strategy
 * the ideas behind implementing key parts of linear base
 * rules and advice on contributing

## Overall Architecture

Linear base is more than a copy of things from [`base`] with some function
arrows being replaced by linear arrows. Moreover, the goal is __not__ exact
compliance to base.

Linear base consists of the following:

 * fundamental data structures, functions and classes that arise naturally from
   wanting to do any linear development (e.g., `Unrestricted` and `Consumable`)
 * tools ported from [`base`] **and from other critical haskell libraries, like `lens`**
 * new APIs for using external resources, e.g., file IO in [`System.IO.Resource`]
 * new tools made possible with linear-types, like an API for mutable arrays
   that doesn't use complicated monads ([`Data.Array.Mutable.Linear`]).

There is a top-level `Prelude.Linear` that is meant to be imported _unqualified_.
It does not include functors, monads, applicatives and so on because there are
multiple sensible ways to give linear arrows to these things. See this [blog
post] for details. This prelude includes:

  * Linear variants of the standard `Prelude`
  * The fundamental `Data.Unrestricted` data, functions and classes
  * The `Num` and related classes and instances in the [`Num`] module


## General Implementation Strategy

This is the strategy (that we've followed so far) for developing linear base:

1. If the tool is simple enough that there's only one sensible place to replace
a function arrow by a linear arrow, do that. Example:

```haskell
foldr :: (a #-> b #-> b) -> b #-> [a] #-> b
foldr f z = \case
  [] -> z
  x:xs -> f x (foldr f z xs)
```

Otherwise, we need to implement each sensible variant of the tool and put it in a
well-named module. This is the case with `Data.Functor`s and `Control.Functor`s
(recall this [blog post]).

2. The ideas behind new tools that are just now possible with linear types vary
and each have unique concepts that are not addressed by a general strategy.
These should be documented below if one of the following is true:

  * There is an overarching concept that extends beyond a handful of modules
  * There is an explicit departure away from the direction `base` takes on some
    tool. (E.g., we decide there should be different laws for some typeclass
    from `base`.)

## How Key Pieces are Implemented

### Num and similar classes

We create a hierarchy of numeric type classes, like an additive Abelian
group, all the way up to `Num`. Then (with some tricks) we derive instances of
these classes for `Int`, `Double` and the numeric types. Link to the source:
[`Num`].

### Mutable Data Structures

Mutable data structures release constructors that take continuations. The
accessors linearly thread the data structure. The mutators linearly consume
and return the data structure. These data structures are always consumable.

```haskell
allocate :: Int -> (DataStructure a #-> Unrestricted b) #-> Unrestricted b
mutate :: Something -> DataStructure a #-> DataStructure a
access :: Something -> DataStructure a #-> (DataStructure a, AccessInfo)
instance Consumable (DataStructure a)
```

## Rules and Advice on Contributing

In addition to [our contributing guide], we have these rules:

A few simple conventions are required:

  * Qualified imports that are meaningful and not abbreviations. So, for
    instance, import `Data.Functor.Linear` as `Linear` and not as `F` for
    functor.
  * An export list for public modules.
  * Module level documentation and haddock on key data definitions and top
    level functions.

General advice:

 * Sticking with the feel of the code already in linear base; make your work blend in.
 * Organize your code into simple sections.
 * Write tests for anything using unsafe functions (and tests in general).

[functors]: https://www.tweag.io/posts/2020-01-16-data-vs-control.html
[examples/Simple/FileIO.hs]: https://github.com/tweag/linear-base/tree/master/examples/Simple/FileIO.hs
[`Data.Unrestricted.Linear`]: https://github.com/tweag/linear-base/tree/master/src/Data/Unrestricted/Linear.hs
[`Num`]: https://github.com/tweag/linear-base/tree/master/src/Data/Num/Linear.hs
[`base`]: https://hackage.haskell.org/package/base
[`Data.Array.Mutable.Linear`]: https://github.com/tweag/linear-base/blob/master/src/Data/Array/Mutable/Linear.hs
[blog post]: https://www.tweag.io/posts/2020-01-16-data-vs-control.html
[our contributing guide]: https://github.com/tweag/.github/blob/master/CONTRIBUTING.md
[`System.IO.Resource`]: https://github.com/tweag/linear-base/blob/master/src/System/IO/Resource.hs
