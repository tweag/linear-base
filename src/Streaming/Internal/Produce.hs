{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides all functions which produce a
-- 'Stream (Of a) m r' from some given non-stream inputs.
module Streaming.Internal.Produce
  ( -- * Constructing Finite 'Stream's
    yield
  , each'
  , unfoldr
  , fromHandle
  , readFile
  , replicate
  , replicateM
  , replicateZip
  , untilRight
  -- * Working with infinite 'Stream's
  , stdinLnN
  , stdinLnUntil
  , stdinLnUntilM
  , stdinLnZip
  , readLnN
  , readLnUntil
  , readLnUntilM
  , readLnZip
  , iterateN
  , iterateZip
  , iterateMN
  , iterateMZip
  , cycleN
  , cycleZip
  , enumFromN
  , enumFromZip
  , enumFromThenN
  , enumFromThenZip
  ) where

import Streaming.Internal.Type
import Streaming.Internal.Process
import Streaming.Internal.Consume (effects)
import Prelude.Linear (($), (&))
import Prelude (Either(..), Read, Bool(..), FilePath, Enum, otherwise,
               Num(..), Int, otherwise, Eq(..), Ord(..), fromEnum, toEnum)
import qualified Prelude
import qualified Control.Functor.Linear as Control
import Data.Unrestricted.Linear
import System.IO.Linear
import System.IO.Resource
import qualified System.IO as System
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack


-- # The Finite Stream Constructors
-------------------------------------------------------------------------------

{-| A singleton stream

@
\>\>\> stdoutLn $ yield "hello"
hello
@

@
\>\>\> S.sum $ do {yield 1; yield 2; yield 3}
6 :> ()
@
-}
yield :: Control.Monad m => a -> Stream (Of a) m ()
yield x = Step $ x :> Return ()
{-# INLINE yield #-}

{- | Stream the elements of a pure, foldable container.

@
\>\>\> S.print $ each' [1..3]
1
2
3
@
-}
each' :: Control.Monad m => [a] -> Stream (Of a) m ()
each' xs = Prelude.foldr (\a stream -> Step $ a :> stream) (Return ()) xs
{-# INLINABLE each' #-}

{-| Build a @Stream@ by unfolding steps starting from a seed. In particular note
    that @S.unfoldr S.next = id@.

-}
unfoldr :: Control.Monad m =>
  (s %1-> m (Either r (Ur a, s))) -> s %1-> Stream (Of a) m r
unfoldr step s = unfoldr' step s
  where
    unfoldr' :: Control.Monad m =>
      (s %1-> m (Either r (Ur a, s))) -> s %1-> Stream (Of a) m r
    unfoldr' step s = Effect $ step s Control.>>= \case
      Left r -> Control.return $ Return r
      Right (Ur a,s') ->
        Control.return $ Step $ a :> unfoldr step s'
{-# INLINABLE unfoldr #-}

-- Note: we use the RIO monad from linear base to enforce
-- the protocol of file handles and file I/O
fromHandle :: Handle %1-> Stream (Of Text) RIO ()
fromHandle h = loop h
  where
    loop :: Handle %1-> Stream (Of Text) RIO ()
    loop h = Control.do
      (Ur isEOF, h') <- Control.lift $ hIsEOF h
      case isEOF of
        True -> Control.do
          Control.lift $ hClose h'
          Control.return ()
        False -> Control.do
          (Ur text, h'') <- Control.lift $ hGetLine h'
          yield text
          fromHandle h''
{-# INLINABLE fromHandle #-}

{-| Read the lines of a file given the filename.

-}
readFile :: FilePath -> Stream (Of Text) RIO ()
readFile path = Control.do
  handle <- Control.lift $ openFile path System.ReadMode
  fromHandle handle

-- | Repeat an element several times.
replicate :: (HasCallStack, Control.Monad m) => Int -> a -> Stream (Of a) m ()
replicate n a
  | n < 0 = Prelude.error "Cannot replicate a stream of negative length"
  | otherwise = loop n a
    where
      loop :: Control.Monad m => Int -> a -> Stream (Of a) m ()
      loop n a
        | n == 0 = Return ()
        | otherwise = Effect $ Control.return $ Step $ a :> loop (n-1) a
{-# INLINABLE replicate #-}

{-| Repeat an action several times, streaming its results.

@
\>\>\> import qualified Unsafe.Linear as Unsafe
\>\>\> import qualified Data.Time as Time
\>\>\> let getCurrentTime = fromSystemIO (Unsafe.coerce Time.getCurrentTime)
\>\>\> S.print $ S.replicateM 2 getCurrentTime
2015-08-18 00:57:36.124508 UTC
2015-08-18 00:57:36.124785 UTC
@
-}
replicateM :: Control.Monad m =>
  Int -> m (Ur a) -> Stream (Of a) m ()
replicateM n ma
  | n < 0 = Prelude.error "Cannot replicate a stream of negative length"
  | otherwise = loop n ma
    where
      loop :: Control.Monad m => Int -> m (Ur a) -> Stream (Of a) m ()
      loop n ma
        | n == 0 = Return ()
        | otherwise = Effect $ Control.do
          Ur a <- ma
          Control.return $ Step $ a :> (replicateM (n-1) ma)

-- | Replicate a constant element and zip it with the finite stream which
-- is the first argument.
replicateZip :: Control.Monad m =>
  Stream (Of x) m r -> a -> Stream (Of (a,x)) m r
replicateZip stream a = map ((,) a) stream
{-# INLINABLE replicateZip #-}

untilRight :: forall m a r . Control.Monad m =>
  m (Either (Ur a) r) -> Stream (Of a) m r
untilRight mEither = Effect loop
  where
    loop :: m (Stream (Of a) m r)
    loop = Control.do
      either <- mEither
      either & \case
        Left (Ur a) ->
          Control.return $ Step $ a :> (untilRight mEither)
        Right r -> Control.return $ Return r
{-# INLINABLE untilRight #-}


-- # The \"Affine\" 'Stream'
-------------------------------------------------------------------------------

-- | An *affine stream is represented with a state of type @x@,
-- a possibly terminating step function of type @(x %1-> m (Either (f x) r))@,
-- and a stop-short function @(x %1-> m r)@.
--
-- This mirrors the unfold of a normal stream:
--
-- > data Stream f m r where
-- >   Stream :: x %1-> (x %1-> m (Either (f x) r)) -> Stream f m r
--
-- *Though referred to as an \"affine stream\" this might not be the correct
-- definition for affine streams. Sorting this out requires a bit more
-- careful thought.
data AffineStream f m r where
  AffineStream ::
    x %1->
    (x %1-> m (Either (f x) r)) ->
    (x %1-> m r) ->
    AffineStream f m r

-- | Take @n@ number of elements from the affine stream, for non-negative
-- @n@. (Negative @n@ is treated as 0.)
take :: forall f m r. (Control.Monad m, Control.Functor f) =>
  Int -> AffineStream f m r %1-> Stream f m r
take = loop where
  loop :: Int -> AffineStream f m r %1-> Stream f m r
  loop n (AffineStream s step end)
    | n <= 0 = Effect $ Control.fmap Control.return $ end s
    | otherwise = Effect $ Control.do
        next <- step s
        next & \case
          Right r -> Control.return (Return r)
          Left fx -> Control.return $ Step $
            Control.fmap (\x -> loop (n-1) (AffineStream x step end)) fx
{-# INLINABLE take #-}

-- | Run an affine stream until it ends or a monadic test succeeds.
-- Drop the element it succeeds on.
untilM :: forall a m r. Control.Monad m =>
  (a -> m Bool) -> AffineStream (Of a) m r %1-> Stream (Of a) m r
untilM = loop where
  loop :: (a -> m Bool) -> AffineStream (Of a) m r %1-> Stream (Of a) m r
  loop test (AffineStream s step end) = Effect $ Control.do
    next <- step s
    next & \case
      Right r -> Control.return (Return r)
      Left (a :> next) -> Control.do
        testResult <- test a
        testResult & \case
          False -> Control.return $
            Step $ a :> loop test (AffineStream next step end)
          True -> Control.fmap Control.return $ end next
{-# INLINABLE untilM #-}

-- | Like 'untilM' but without the monadic test.
until :: forall a m r. Control.Monad m =>
  (a -> Bool) -> AffineStream (Of a) m r %1-> Stream (Of a) m r
until = loop where
  loop :: (a -> Bool) -> AffineStream (Of a) m r %1-> Stream (Of a) m r
  loop test (AffineStream s step end) = Effect $ Control.do
    next <- step s
    next & \case
      Right r -> Control.return (Return r)
      Left (a :> next) -> case test a of
        True -> Control.fmap Control.return $ end next
        False -> Control.return $ Step $
          a :> loop test (AffineStream next step end)
{-# INLINABLE until #-}

-- | Zip a finite stream with an affine stream.
zip :: forall a x m r1 r2. Control.Monad m =>
  Stream (Of x) m r1 %1->
  AffineStream (Of a) m r2 %1->
  Stream (Of (x,a)) m (r1,r2)
zip = loop where
  loop ::
    Stream (Of x) m r1 %1->
    AffineStream (Of a) m r2 %1->
    Stream (Of (x,a)) m (r1,r2)
  loop stream (AffineStream s step end) = stream & \case
    Return r1 -> Effect $
      Control.fmap (\r2 -> Control.return $ (r1,r2)) $ end s
    Effect m -> Effect $
      Control.fmap (\str -> loop str (AffineStream s step end)) m
    Step (x :> rest) -> Effect $ Control.do
      next <- step s
      next & \case
        Right r2 -> Control.do
          r1 <- effects rest
          Control.return (Return (r1,r2))
        Left (a :> rest') -> Control.return $ Step $
          (x,a) :> loop rest (AffineStream rest' step end)
{-# INLINABLE zip #-}

-- | An affine stream of standard input lines.
stdinLn :: AffineStream (Of Text) IO ()
stdinLn = AffineStream () getALine Control.pure where
  getALine :: () %1-> IO (Either (Of Text ()) ())
  getALine () = Control.do
    Ur line <- fromSystemIOU System.getLine
    Control.return $ Left (Text.pack line :> ())

-- | An affine stream of reading lines, crashing on failed parse.
readLn :: Read a => AffineStream (Of a) IO ()
readLn = AffineStream () readALine Control.pure where
  readALine :: Read a => () %1-> IO (Either (Of a ()) ())
  readALine () = Control.do
    Ur line <- fromSystemIOU System.getLine
    Control.return $ Left (Prelude.read line :> ())

-- | An affine stream iterating an initial state forever.
iterate :: forall a m.
  Control.Monad m => a -> (a -> a) -> AffineStream (Of a) m ()
iterate a step =
  AffineStream (Ur a) stepper (\x -> Control.return $ consume x)
  where
    stepper :: Ur a %1-> m (Either (Of a (Ur a)) ())
    stepper (Ur a) = Control.return $
      Left $ a :> Ur (step a)

-- | An affine stream monadically iterating an initial state forever.
iterateM :: forall a m. Control.Monad m =>
  m (Ur a) -> (a -> m (Ur a)) -> AffineStream (Of a) m ()
iterateM ma step =
  AffineStream ma stepper (Control.fmap consume)
  where
    stepper :: m (Ur a) %1-> m (Either (Of a (m (Ur a))) ())
    stepper ma = Control.do
      Ur a <- ma
      Control.return $ Left $ a :> (step a)

-- Remark. In order to implement the affine break function, which is the third
-- argument of the constructor, we need to specify the functor as @Of@.
-- Approaches to keeping it functor general seem messy.

-- | An affine stream cycling through a given finite stream forever.
cycle :: forall a m r. (Control.Monad m, Consumable r) =>
  Stream (Of a) m r -> AffineStream (Of a) m r
cycle stream =
  -- Note. The state is (original stream, stream_in_current_cycle)
  AffineStream (Ur stream, stream) stepStream leftoverEffects
  where
    leftoverEffects ::
      (Ur (Stream (Of a) m r), Stream (Of a) m r) %1-> m r
    leftoverEffects (Ur _, str) = effects str

    stepStream :: Control.Functor f =>
      (Ur (Stream f m r), Stream f m r) %1->
      m (Either (f (Ur (Stream f m r), Stream f m r)) r)
    stepStream (Ur s, str) = str & \case
      Return r -> lseq r $ stepStream (Ur s, s)
      Effect m ->
        m Control.>>= (\stream -> stepStream (Ur s, stream))
      Step f -> Control.return $
        Left $ Control.fmap ((,) (Ur s)) f

-- | An affine stream iterating an enumerated stream forever.
enumFrom :: (Control.Monad m, Enum e) => e -> AffineStream (Of e) m ()
enumFrom e = iterate e Prelude.succ

-- | An affine stream iterating an enumerated stream forever, using the
-- first two elements to determine the gap to skip by.
-- E.g., @enumFromThen  3 5@ is like @[3,5..]@.
enumFromThen :: forall e m. (Control.Monad m, Enum e) =>
  e -> e -> AffineStream (Of e) m ()
enumFromThen e e' = iterate e enumStep where
  enumStep :: e -> e
  enumStep enum = toEnum Prelude.$
    (fromEnum enum) + ((fromEnum e') - (fromEnum e))
    -- Think:  \enum -> enum + stepSize where stepSize = (e1 - e0)


-- # Working with infinite 'Stream's
-------------------------------------------------------------------------------

-- | @stdinLnN n@ is a stream of @n@ lines from standard input
stdinLnN :: Int -> Stream (Of Text) IO ()
stdinLnN n = take n stdinLn
{-# INLINE stdinLnN #-}

-- | Provides a stream of standard input and omits the first line
-- that satisfies the predicate, possibly requiring IO
stdinLnUntilM :: (Text -> IO Bool) -> Stream (Of Text) IO ()
stdinLnUntilM test = untilM test stdinLn
{-# INLINE stdinLnUntilM #-}

-- | Provides a stream of standard input and omits the first line
-- that satisfies the predicate
stdinLnUntil :: (Text -> Bool) -> Stream (Of Text) IO ()
stdinLnUntil test = until test stdinLn
{-# INLINE stdinLnUntil #-}

-- | Given a finite stream, provide a stream of lines of standard input
-- zipped with that finite stream
stdinLnZip :: Stream (Of x) IO r %1-> Stream (Of (x, Text)) IO r
stdinLnZip stream = Control.fmap (\(r,()) -> r) $ zip stream stdinLn
{-# INLINE stdinLnZip #-}

readLnN :: Read a => Int -> Stream (Of a) IO ()
readLnN n = take n readLn
{-# INLINE readLnN #-}

readLnUntilM :: Read a => (a -> IO Bool) -> Stream (Of a) IO ()
readLnUntilM test = untilM test readLn
{-# INLINE readLnUntilM #-}

readLnUntil :: Read a => (a -> Bool) -> Stream (Of a) IO ()
readLnUntil test = until test readLn
{-# INLINE readLnUntil #-}

readLnZip :: Read a => Stream (Of x) IO r %1-> Stream (Of (x, a)) IO r
readLnZip stream = Control.fmap (\(r,()) -> r) $ zip stream readLn
{-# INLINE readLnZip #-}

-- | Iterate a pure function from a seed value,
-- streaming the results forever.
iterateN :: Control.Monad m => Int -> (a -> a) -> a -> Stream (Of a) m ()
iterateN n step a = take n $ iterate a step
{-# INLINE iterateN #-}

iterateZip :: Control.Monad m => Stream (Of x) m r ->
  (a -> a) -> a -> Stream (Of (x,a)) m r
iterateZip stream step a =
  Control.fmap (\(r,()) -> r) $ zip stream $ iterate a step
{-# INLINE iterateZip #-}

-- | Iterate a monadic function from a seed value,
-- streaming the results forever.
iterateMN :: Control.Monad m =>
  Int -> (a -> m (Ur a)) -> m (Ur a) -> Stream (Of a) m ()
iterateMN n step ma = take n $ iterateM ma step
{-# INLINE iterateMN #-}

iterateMZip :: Control.Monad m =>
  Stream (Of x) m r %1->
  (a -> m (Ur a)) -> m (Ur a) -> Stream (Of (x,a)) m r
iterateMZip stream step ma =
  Control.fmap (\(r,()) -> r) $ zip stream $ iterateM ma step
{-# INLINE iterateMZip #-}

-- | Cycle a stream a finite number of times
cycleN :: (Control.Monad m, Consumable r) =>
  Int -> Stream (Of a) m r -> Stream (Of a) m r
cycleN n stream = take n $ cycle stream
{-# INLINE cycleN #-}

-- | @cycleZip s1 s2@ will cycle @s2@ just enough to zip with the given finite
-- stream @s1@. Note that we consume all the effects of the remainder of the
-- cycled stream @s2@. That is, we consume @s2@ the smallest natural number of
-- times we need to zip.
cycleZip :: (Control.Monad m, Consumable s) =>
  Stream (Of a) m r %1-> Stream (Of b) m s -> Stream (Of (a,b)) m (r,s)
cycleZip str stream = zip str $ cycle stream
{-# INLINE cycleZip #-}

{-| An finite sequence of enumerable values at a fixed distance, determined
   by the first and second values.

@
\>\>\> S.print $ S.enumFromThenN 3 100 200
100
200
300
@
-}
enumFromThenN :: (Control.Monad m, Enum e) => Int -> e -> e -> Stream (Of e) m ()
enumFromThenN n e e' = take n $ enumFromThen e e'
{-# INLINE enumFromThenN #-}

-- | A finite sequence of enumerable values at a fixed distance determined
-- by the first and second values. The length is limited by zipping
-- with a given finite stream, i.e., the first argument.
enumFromThenZip :: (Control.Monad m, Enum e) =>
  Stream (Of a) m r %1-> e -> e -> Stream (Of (a,e)) m r
enumFromThenZip stream e e'=
  Control.fmap (\(r,()) -> r) $ zip stream $ enumFromThen e e'
{-# INLINE enumFromThenZip #-}

-- | Like 'enumFromThenN' but where the next element in the enumeration is just
-- the successor @succ n@ for a given enum @n@.
enumFromN :: (Control.Monad m, Enum e) => Int -> e -> Stream (Of e) m ()
enumFromN n e = take n $ enumFrom e
{-# INLINE enumFromN #-}

-- | Like 'enumFromThenZip' but where the next element in the enumeration is just
-- the successor @succ n@ for a given enum @n@.
enumFromZip :: (Control.Monad m, Enum e) =>
  Stream (Of a) m r %1-> e -> Stream (Of (a,e)) m r
enumFromZip str e =
  Control.fmap (\(r,()) -> r) $ zip str $ enumFrom e
{-# INLINE enumFromZip #-}

