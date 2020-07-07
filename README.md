Linear base
===========

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/tweag/linear-base/blob/master/LICENSE)
[![Build status](https://badge.buildkite.com/5b60ab93dadba234a95e04e6568985918552dcc9e7685ede0d.svg?branch=master)](https://buildkite.com/tweag-1/linear-base)


Linear base is a standard library for developing applications with linear
types. It is named `linear-base` to be an analog to the original [`base`]
package that ships with GHC.

The purpose of `linear-base` is to provide the minimal facilities you need to
write _practical_ Linear Haskell code, i.e., Haskell code that uses the
`-XLinearHaskell` language extension.

## Motivation

_Why do you need `linear-base` to write linear projects?_

1. If you are writing a linear function, you cannot use the standard
  non-linear data types, functions and classes in `base`. Even simple uses of
  `base` facilities break down. For instance, if `n` is a linearly bound `Int`,
  the RHS of an definition cannot write `n + 1` --- this will not type check. We need
  linear variants of `Num`, `Functor`s, `Monad`s, `($)`, etc.

2. There are several primitive data structures, functions and classes
  that can only be safely released with `-XLinearHaskell`.  For example, the
  API for file IO released by `linear-base` is safe unlike the current API
  which allows, say, reading from a closed file handle.  _Some of these are new
  and exciting because they are not in `base`, e.g., `linear-base` releases a
  safe API for mutable arrays allowing values to be updated in place!_

## Getting Started

Since -XLinearTypes are not part of a GHC release, we need to use nix to get a
version of GHC with support for linear types.

1. In your stack project, add our [`shell-stack.nix`] and [`nixpkgs.nix`] files.
2. Edit your `stack.yaml` to have the following fields.  _Use an appropriate
   commit in the `extra-deps`._

```yaml
resolver: lts-16.2
compiler: ghc-8.11
allow-newer: true
system-ghc: true

nix:
  enable: true
  shell-file: shell-stack.nix
  path: ["nixpkgs=./nixpkgs.nix"]

extra-deps:
  - git: https://github.com/facundominguez/quickcheck.git
    commit: a498e7b41131cf7955b9e154ab26d37d1be10304
  - git: https://github.com/facundominguez/lifted-async.git
    commit: 898eb485b21cc321058345998eedd8c409c326fd
  - git: https://github.com/tweag/linear-base.git
    commit: 705522f0bad3f1083bb3447b9f476f6f84d21410
```

4. Add `linear-base` to your cabal file's `build-depends:` for the appropriate
   library or executable.

5. You're done! You can add `{-# LANGUAGE LinearTypes #-}` to the top of your
   modules and build with `stack build` and test with `stack test` and so on.
   To learn more about stack-nix integration, see
   [here](https://docs.haskellstack.org/en/stable/nix_integration/).

## User Guide

If you already know what `-XLinearHaskell` does and what the linear arrow
`a #-> b` means, then read the [User Guide] and the [`/examples`] to know how to
use `linear-base`.

## Learning about `-XLinearHaskell`

If you're a Haskeller who hasn't written any Linear Haskell code, don't fear!
There are plenty of excellent resources and examples to help you.

### Tutorials and Examples

 * See the [`/examples`] sub-folder. There is a README organizing the examples
   and pointing out tutorials.
 * [Linear examples on watertight 3D models](https://github.com/gelisam/linear-examples)

### Reading Material

  * There is a [wiki page](https://gitlab.haskell.org/ghc/ghc/-/wikis/linear-types).
  * Key Blog posts
    * [Predictable performance](https://www.tweag.io/posts/2017-03-13-linear-types.html) (the first blog post from Tweag on this)
    * [IO State Transitions](https://www.tweag.io/posts/2017-08-03-linear-typestates.html)
    * [Streaming](https://www.tweag.io/posts/2018-06-21-linear-streams.html)
    * See [here](https://www.tweag.io/tag/linear-types.html) for all of Tweag's blog posts on linear types.
  * [Here is the paper](https://arxiv.org/pdf/1710.09756.pdf) behind `-XLinearTypes`.

### Talks

 * [Practical Linearity in a higher-order polymorphic language -- POPL 2018](https://www.youtube.com/watch?v=o0z-qlb5xbI)
 * [Practical Linearity in a higher-order polymorphic language -- Curry on 2018](https://www.youtube.com/watch?v=t0mhvd3-60Y&t=3s)
 * [Practical Linearity in a higher-order polymorphic language -- Haskell Exchange 2018](https://skillsmatter.com/skillscasts/11067-keynote-linear-haskell-practical-linearity-in-a-higher-order-polymorphic-language)
 * [Implementing Linear Haskell](https://www.youtube.com/watch?v=uxv62QQajx8)

## Contributing

Linear base is maintained by [Tweag I/O].

To contribute please see the [Design Document] for instructions and advice on
making pull requests.

## Licence

See our [Licence](https://github.com/tweag/linear-base/blob/doc-overview/LICENSE).

Copyright © Tweag I/O

[Tweag I/O]: https://www.tweag.io/
[`base`]: https://hackage.haskell.org/package/base
[`shell-stack.nix`]: https://github.com/tweag/linear-base/blob/master/shell-stack.nix
[`nixpkgs.nix`]: https://github.com/tweag/linear-base/blob/master/nixpkgs.nix
[User Guide]: https://github.com/tweag/linear-base/tree/master/docs/USER_GUIDE.md
[Design Document]: https://github.com/tweag/linear-base/tree/master/docs/DESIGN.md
[`/examples`]: https://github.com/tweag/linear-base/tree/master/examples
