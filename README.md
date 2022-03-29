# Linear base

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/tweag/linear-base/blob/master/LICENSE)
[![Build status](https://badge.buildkite.com/5b60ab93dadba234a95e04e6568985918552dcc9e7685ede0d.svg?branch=master)](https://buildkite.com/tweag-1/linear-base)
[![Hackage](https://img.shields.io/hackage/v/linear-base.svg?style=flat&color=brightgreen)][hackage-pkg]
[![Stackage](https://stackage.org/package/linear-base/badge/nightly)][stackage-pkg]

Linear base is a standard library for developing applications with linear
types. It is named `linear-base` to be an analog to the original [`base`]
package that ships with GHC.

The purpose of `linear-base` is to provide the minimal facilities you need to
write _practical_ Linear Haskell code, i.e., Haskell code that uses the
`-XLinearTypes` language extension.

## Motivation

_Why do you need `linear-base` to write linear projects?_

1. Data types, functions and classes in `base` are not linear types
  aware. For instance, if `n` is a linearly-bound `Int`, the RHS of
  a definition cannot write `n + 1` — this will not type check. We
  need linear variants of `Num`, `Functor`s, `Monad`s, `($)`, etc.

2. This library exports new abstractions that leverage linear types
  for resource safety or performance. For example, there are new APIs
  for file and socket I/O as well as for safe in-place mutation of
  arrays.

## Getting started

`-XLinearTypes` is released with GHC 9, and `linear-base` is released
on [Hackage][hackage-pkg] and [Stackage][stackage-pkg].

All source files with linear types need a language extension pragma at
the top:

```
{-# LANGUAGE LinearTypes #-}
```

## User Guide

If you already know what `-XLinearTypes` does and what the linear
arrow `a %1-> b` means, then read the [User Guide] and explore the
[`examples/`](https://github.com/tweag/linear-base/blob/master/examples) folder to know how to use `linear-base`.

You can also find a table comparing `base` and `linear-base` typeclasses
[here](https://github.com/tweag/linear-base/blob/master/docs/CLASS_TABLE.md).

## Learning about `-XLinearTypes`

If you're a Haskeller who hasn't written any Linear Haskell code, don't fear!
There are plenty of excellent resources and examples to help you.

### Tutorials and examples

 * See the [`examples/`](https://github.com/tweag/linear-base/blob/master/examples) folder.
 * [Linear examples on watertight 3D models](https://github.com/gelisam/linear-examples)

### Reading material

  * There is a [wiki page](https://gitlab.haskell.org/ghc/ghc/-/wikis/linear-types).
  * Key Blog posts
    * [Predictable performance](https://www.tweag.io/posts/2017-03-13-linear-types.html) (the first blog post from Tweag on this)
    * [IO State Transitions](https://www.tweag.io/posts/2017-08-03-linear-typestates.html)
    * [Streaming](https://www.tweag.io/posts/2018-06-21-linear-streams.html)
    * See [here](https://www.tweag.io/blog/tags/linear-types/) for all of Tweag's blog posts on linear types.
  * [Here is the paper](https://arxiv.org/pdf/1710.09756.pdf) behind `-XLinearTypes`.

### Talks
–
 * [Distributed Programming with Linear Types – Haskell Exchange 2017](https://skillsmatter.com/skillscasts/10637-distributed-programming-with-linear-types)
 * [Practical Linearity in a higher-order polymorphic language – POPL 2018](https://www.youtube.com/watch?v=o0z-qlb5xbI)
 * [Practical Linearity in a higher-order polymorphic language – Curry on 2018](https://www.youtube.com/watch?v=t0mhvd3-60Y)
 * [Practical Linearity in a higher-order polymorphic language – Haskell Exchange 2018](https://skillsmatter.com/skillscasts/11067-keynote-linear-haskell-practical-linearity-in-a-higher-order-polymorphic-language)
 * [Implementing Linear Haskell](https://www.youtube.com/watch?v=uxv62QQajx8)
 * [In-place array update with linear types – ZuriHac 2020](https://www.youtube.com/watch?v=I7-JuVNvz78)
 * [Typecheck Your Memory Management with Linear Types – Haskell Exchange 2017](https://skillsmatter.com/skillscasts/14896-typecheck-your-memory-management-with-linear-types)

## Contributing

Linear base is maintained by [Tweag].

To contribute please see the [Design Document] for instructions and advice on
making pull requests.

## Licence

See the [Licence file](https://github.com/tweag/linear-base/blob/master/LICENSE).

Copyright © Tweag Holding and its affiliates.

[Tweag]: https://www.tweag.io/
[`base`]: https://hackage.haskell.org/package/base
[User Guide]: https://github.com/tweag/linear-base/blob/master/docs/USER_GUIDE.md
[Design Document]: https://github.com/tweag/linear-base/blob/master/docs/DESIGN.md
[hackage-pkg]: https://hackage.haskell.org/package/linear-base
[stackage-pkg]: https://www.stackage.org/nightly/package/linear-base
