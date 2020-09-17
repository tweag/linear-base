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

### Replacing infinite streams like `stdinLn`

We can't have functions like `stdinLn` that produce infinite streams.
Any function that linearly consumes a `Stream (Of a) m r` where the `m`
is a control monad, would have to consume the entire stream. Hence,
for some `f :: Stream (Of a) m r #-> B`, `f stdinLn` would never terminate.
Hence, we need workarounds for the original API.

The infinite stream API in the original library consisted of
`repeats`, `repeatsM`, `stdinLn`, `readLn`, `cycle`, `enumFrom`, `enumFromThen`.

The size-delimited replacements of `repeats` and `repeatsM` are just `replicates` and
`replicatesM` from the `Streaming` module.

The rest of the API are constructors that produce an infinite streams `Of` elements
possibly from some simple arguments. From combing the examples in the Haddock and linked
to by the haddock, three common use cases emerge:

 1. Taking some finite amount:

 `stdoutLn$ doSomething $ take 3 $ stdinLn`

 2. Taking until a condition is met:

 `highLowGame n = void $ S.break (== n) (readLn :: Stream (Of Int) m ())`

 3. Zipping with a finite stream:

  `doSomething $ zip (each' ["first name: ", "last name: ", "nickname: "]) stdinLn`

 4. Some combination of the above.

We can avoid/replace these cases by having these variants of infinite
streams like `stdinLn`:

`stdinLnN :: Int -> Stream (Of Text) IO ()`
`stdinLnUntil :: (Text -> Bool) -> Stream (Of Text) IO ()`
`stdinLnUntilM :: (Text -> IO Bool) -> Stream (Of Text) IO ()`
`stdinLnZip :: (Stream (Of a) m ()) #-> Stream (Of (a, Text)) IO ()`

This approach is adopted for the rest of the API and the postfixes
`N`, `Until`, `UntilM` and `Zip` are followed. We don't need `Until*` functions
for `replicate`, `cycle`, `enumFrom` and `enumFromThen` since these streams are simple
enough that a test function doesn't make sense; we can predict each element so there's no
cause to test until a condition is met.

### Terminology

 * If the monad of the stream is a normal monad, we call the stream an
   **unrestricted stream**.
 * If the monad of the stream has a `Control.Monad` instance, then we call the
   stream a **linear stream**.

## Conventions

 * We use `Text` in place of `String`

[streaming]: https://github.com/haskell-streaming/streaming
