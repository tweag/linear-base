# Linear base

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/tweag/linear-base/blob/master/LICENSE)
[![Build status](https://badge.buildkite.com/5b60ab93dadba234a95e04e6568985918552dcc9e7685ede0d.svg?branch=master)](https://buildkite.com/tweag-1/linear-base)


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

`-XLinearTypes` is not yet part of any GHC release. We recommend using
Stack and Nix together to pull in an experimental version of GHC into
your project. Use [this `stack.yaml`](./stack.yaml) as a starting
point for your project. To learn more about Stack+Nix integration, see
[here](https://docs.haskellstack.org/en/stable/nix_integration/).

All source files with linear types need a language extension pragma at
the top:

```
{-# LANGUAGE LinearTypes #-}
```

## Documentation and User Guide

 * If you already know what `-XLinearTypes` does and what the linear
   arrow `a #-> b` means, then read the [User Guide] and explore the
   [`examples/`](./examples) folder to know how to use `linear-base`.
 * Each public module has haddock documentation usually with examples
   of use.

## Learning about `-XLinearTypes`

If you're a Haskeller who hasn't written any Linear Haskell code, don't fear!
There are plenty of excellent resources and examples to help you.

### Tutorials and examples

 * See the [`examples/`](./examples) folder.
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

 * [Practical Linearity in a higher-order polymorphic language -- POPL 2018](https://www.youtube.com/watch?v=o0z-qlb5xbI)
 * [Practical Linearity in a higher-order polymorphic language -- Curry on 2018](https://www.youtube.com/watch?v=t0mhvd3-60Y&t=3s)
 * [Practical Linearity in a higher-order polymorphic language -- Haskell Exchange 2018](https://skillsmatter.com/skillscasts/11067-keynote-linear-haskell-practical-linearity-in-a-higher-order-polymorphic-language)
 * [Implementing Linear Haskell](https://www.youtube.com/watch?v=uxv62QQajx8)

## Contributing

Linear base is maintained by [Tweag].

To contribute please see the [Design Document] for instructions and advice on
making pull requests.

## Licence

See the [Licence file](./LICENSE).

Copyright © Tweag Holding and its affiliates.

[Tweag]: https://www.tweag.io/
[`base`]: https://hackage.haskell.org/package/base
[User Guide]: ./docs/USER_GUIDE.md
[Design Document]: ./docs/DESIGN.md
