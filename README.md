# Linear Streams

A streaming library based off of [streaming] that uses linear types to enforce
invariants that were previously left for the programmer to check.

## Motivation

With the original [streaming] library, we could write the following:

```haskell
-- | Acceptable defintion
streamTwoLines :: Stream (Of String) IO ()
streamTwoLines = do
    -- open up a file handle
    handle <- lift $ openFile "temp.txt" ReadMode 
    -- Stream two lines
    str <- lift $ hGetLine handle
    S.yield str
    str' <- lift $ hGetLine handle
    S.yield str'
    -- close the file handle again
    lift $ hClose handle

-- | Minimal bad example: read more than two lines
badRead :: IO ()
badRead = do
    Right (v1, s) <- S.next streamTwoLines
    putStrLn v1
    Right (v1, s') <- S.next s
    putStrLn v1
    Right (v1, s'') <- S.next s -- non linear use of s
    putStrLn v1
```

The problem is that a stream is a list of values that you get
by repeatedly extracting a head-tail pair from some monad, *only taking out
a head-tail pair exactly ONCE* as you go. The idea is that if the stream uses
the IO monad and represents lines of a file, extracting a head-tail pair
of a line and the rest of the stream does the IO of reading a single line.
That IO operation should never be repeated. Indeed, the idea of a stream
actually includes an invariant that you bind on that monad exactly once.
We walk along streams binding the values of type `m a` for some monad `m`
exactly once. We never bind some `m a` twice.

Linear types lets us enforce this.

## Design

Effectively, all of the design boils down to changing the `m` in
`Stream f m r` to have a `Control.Monad.Linear.Monad` instance.
We do this for as many API functions as we can.

This change requires the `f` to have a `Control.Monad.Linear.Functor` instance
and a `Control.Monad` instance for any `Stream f m` with appropriate `m` and `f`.
In general, all changes are necessarily implied from just changing the `m`.

## Conventions

 * We use `Text` in place of `String`

[streaming]: https://github.com/haskell-streaming/streaming
