# Linear base

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/tweag/linear-base/blob/master/LICENSE)
[![Build status](https://badge.buildkite.com/5b60ab93dadba234a95e04e6568985918552dcc9e7685ede0d.svg)](https://buildkite.com/tweag-1/linear-base)
Copyright © Tweag I/O

Linear base is a collection of all the standard library code one needs to
develop applications with linear types. It is named `linear-base` to be an
analog to the original [`base`] package that ships with GHC.

**The purpose of `linear-base` is to provide the minimal facilities you need to
write _practical_ Linear Haskell code, i.e., Haskell code that uses the
`-XLinearHaskell` language extension.**

## Motivation: Why do you need `linear-base` to write practical linear projects?

* First, if you are writing a linear function, you cannot use the standard
  non-linear data types, functions and classes in `base`. For instance, something
  as simple as `n + 1` for a linearly bound argument `n` will not type check.
  We need linear variants of `Functor`s, `Monad`s, `($)`, etc.

* Second, there are several primitive data structures, functions and classes
  that can only be safely released with `-XLinearHaskell`. _Some of these are
  new and exciting because they are not in `base`._ For example, the API for
  file IO released by `linear-base` is safe unlike the current API for file IO
  in `base` which allows, for instance, reading from a closed file handle.
  Consider another good example: `linear-base` releases a safe API for mutable
  arrays, that are updated in-place.

## Getting Started

Please follow these instructions carefully without skimming :)

1. In your stack project, add our [`shell-stack.nix`] and [`nixpkgs.nix`] files.
2. Edit your `stack.yaml` to have these fields:

```yaml
resolver: lts-14.6
compiler: ghc-8.9
allow-newer: true
system-ghc: true

nix:
  shell-file: shell-stack.nix
  path: ["nixpkgs=./nixpkgs.nix"]

```
3. In your project, just do `stack --nix build` to build, and `stack --nix
   test` to test and so on.
4. Add `linear-base` to your cabal file's `build-depends:` for the appropreate
   library or executable. In your `stack.yaml`, add the `linear-haskell` package as an
   `extra-dep` from GitHub. **Don't forget to use a recent commit.**

```yaml
extra-deps:
  - git: https://github.com/tweag/linear-base.git
    commit: 5dcb68d52229753f381110e8b0bb681245080235
```

5. You're done! Profit. To learn more about stack nix integration, see
[here](https://docs.haskellstack.org/en/stable/nix_integration/).

## User Guide

If you already know what `-XLinearHaskell` does and what the linear arrow `a #-> b`
means, then read the [User Guide] to know how to use `linear-base` and look at
the [`examples`] sub-folder.

## Learning about `-XLinearHaskell`

If you're a Haskeller who hasn't written any Linear Haskell code, don't fear!
There are plenty of excellent resources and examples to help you.

### Tutorials and Examples

 * See the [`/examples`] sub-folder. There is a README to guide you.
 * [Linear examples on watertight 3D models](https://github.com/gelisam/linear-examples)

### The Paper and Blog Posts

  * [Here is the paper](https://arxiv.org/pdf/1710.09756.pdf) behind `-XLinearTypes`.
  * Key Blog posts
   * [Predictable performance](https://www.tweag.io/posts/2017-03-13-linear-types.html) (the first blog post from Tweag on this)
   * [IO State Transitions](https://www.tweag.io/posts/2017-08-03-linear-typestates.html)
   * [Streaming](https://www.tweag.io/posts/2018-06-21-linear-streams.html)

### Talks

 * [Practical Linearity in a higher-order polymorphic language](https://www.youtube.com/watch?v=t0mhvd3-60Y&t=3s)
 * [Implementing Linear Haskell](https://www.youtube.com/watch?v=uxv62QQajx8)

## Contributing

Linear base is maintained by [Tweag I/O].

To contribute please see the [Design Document] for what needs work, the style guide,
advice on making pull requests and so on.

## Licence

See [Licence](https://github.com/tweag/linear-base/blob/doc-overview/LICENSE).


[Tweag I/O]: https://www.tweag.io/
[`base`]: https://hackage.haskell.org/package/base
[`shell-stack.nix`]: https://github.com/tweag/linear-base/blob/master/shell-stack.nix
[`nixpkgs.nix`]: https://github.com/tweag/linear-base/blob/master/nixpkgs.nix
[User Guide]: https://github.com/tweag/linear-base/tree/master/doc/USER_GUIDE.md
[Design Document]: https://github.com/tweag/linear-base/tree/master/doc/DESIGN.md
[`/examples`]: https://github.com/tweag/linear-base/tree/master/examples
