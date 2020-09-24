# Design

## Overall architecture

Linear base is more than a copy of things from [`base`] with some function
arrows being replaced by linear arrows. Moreover, the goal is __not__ exact
compliance with `base`.

Linear base consists of the following:

* fundamental data structures, functions and classes that arise
  naturally from wanting to do any linear development (e.g.,
  `Ur` and `Consumable`),
* tools ported from [`base`] and from other critical haskell
  libraries, like `lens`,
* new APIs for using system resources, e.g., file I/O in
  [`System.IO.Resource`],
* new abstractions made possible by linear types, like monad-free
  mutable arrays in ([`Data.Array.Mutable.Linear`]).

There is a top-level `Prelude.Linear` that is meant to be imported _unqualified_.
It does not include functors, monads, applicatives and so on because there are
multiple sensible ways to give linear arrows to these things. See this [blog
post] for details. This prelude includes:

* linear variants of definitions in `Prelude`,
* a few pervasive utility definitions when programming with linear
  types.

## General implementation strategy

This is the strategy that we've followed so far for developing
`linear-base`:

1. If the definition is simple enough that there's only one sensible
   place to replace a function arrow by a linear arrow, do that.
   Example:

   ```haskell
   foldr :: (a #-> b #-> b) -> b #-> [a] #-> b
   foldr f z = \case
     [] -> z
     x:xs -> f x (foldr f z xs)
   ```

	Otherwise, implement each sensible variant of the definition in
    dedicated modules. For instance, this is the case with
    `Data.Functor`s and `Control.Functor`s (see this [blog post]).

2. The ideas behind new definitions that are just now possible with
   linear types vary and each have unique concepts that are not
   addressed by a general strategy. These should be documented below
   if one of the following is true:

   * there is an overarching concept that extends beyond a handful of
     modules. Or,
   * There is an explicit departure away from the direction of `base`.
     (E.g., we decide there should be different laws for some type
     class already in `base`.)

## Conventions

We have established the following conventions in this project:

* use full words for Qualified imports, not abbreviations. For
  instance, import `Data.Functor.Linear` as `Linear` and not as `F`
  for functor.
* All public modules have an export list.

[functors]: https://www.tweag.io/posts/2020-01-16-data-vs-control.html
[examples/Simple/FileIO.hs]: https://github.com/tweag/linear-base/tree/master/examples/Simple/FileIO.hs
[`Data.Unrestricted.Linear`]: https://github.com/tweag/linear-base/tree/master/src/Data/Unrestricted/Linear.hs
[`Num`]: https://github.com/tweag/linear-base/tree/master/src/Data/Num/Linear.hs
[`base`]: https://hackage.haskell.org/package/base
[`Data.Array.Mutable.Linear`]: https://github.com/tweag/linear-base/blob/master/src/Data/Array/Mutable/Linear.hs
[blog post]: https://www.tweag.io/posts/2020-01-16-data-vs-control.html
[contributor's guide]: ../CONTRIBUTING.md
[`System.IO.Resource`]: https://github.com/tweag/linear-base/blob/master/src/System/IO/Resource.hs
