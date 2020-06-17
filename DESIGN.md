# Design

This document describes our _solution_ to maintaining linear stream invariants
and building out streams with different levels of linearity (or affineness).

## Goal

The goal is not to merely port the streaming library.

> The goal is to provide linear, affine, and non-affine streaming tools 
> that (1) interplay nicely (perhaps linearity-polymorphism)
> and (2) are easy to use.


While the existing streaming library is great, it has many over-complicated parts to it that
very much seem like they could be improved. Here's a basic list:

 * All the functor-general stuff should ideally either be removed, or accompanied with a
 version only for `Of`. In general, unless functor generality is well motivated,
 it's a good idea to omit it for simplicity.
 * If there's a way to simplify the `Stream` type to still do a `splitAt` or
   `copy`, do so. A `Stream f m r` that has the functor or monad be itself a
   stream are (in my view) too complicated.

## Development process

  * We will first migrate the [existing work] to be synced with linear base and the current GHC.

  * We will then create 5-10 medium sized (300+ lines) examples of using
    streams with mixed amounts of linearity using the Streaming.Prelude API.
    These should serve as a concrete goal for what we want to achieve. It seems
    like the big challenge is the interplay between streams of varying
    linearity.

  * We will design solutions to implement all the examples with linear streams
    instead of unsafe streams. The focus is interplay and simplicity.

  * We will implement anything that remains of the streaming API.  The focus is
    interplay and simplicity. The messy stuff from the original library should
    not carry over.


## Ommited right now

 * The advanced Streaming module
 * The sum and compose manipulation



[existing work]: https://github.com/m0ar/safe-streaming
