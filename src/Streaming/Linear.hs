{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Streaming.Linear
  (
  -- $stream
   module Streaming.Internal.Type
  -- * Constructing a 'Stream' on a given functor
  , yields
  , effect
  , wrap
  , replicates
  , replicatesM
  , unfold
  , untilJust
  , streamBuild
  , delays
  -- * Transforming streams
  , maps
  , mapsPost
  , mapsM
  , mapsMPost
  , mapped
  , mappedPost
  , hoistUnexposed
  , groups
  -- * Inspecting a stream
  , inspect
  -- * Splitting and joining 'Stream's
  , splitsAt
  , chunksOf
  , concats
  , intercalates
  -- * Zipping, unzipping, separating and unseparating streams
  , unzips
  , separate
  , unseparate
  , decompose
  , expand
  , expandPost
  -- * Eliminating a 'Stream'
  , mapsM_
  , run
  , streamFold
  , iterTM
  , iterT
  , destroy
  ) where

import Streaming.Internal.Type
import Streaming.Internal.Process (destroyExposed)
import Data.Functor.Sum
import Data.Functor.Compose
import qualified Streaming.Prelude.Linear as Stream
import System.IO.Linear
import Prelude.Linear (($), (.), (&))
import Prelude (Ordering(..), Ord(..), Num(..), Int, Either(..), Double,
               Maybe(..), fromInteger)
import qualified Prelude
import qualified Control.Monad.Linear as Control
import qualified Data.Functor.Linear as Data
import Data.Unrestricted.Linear
import Control.Concurrent (threadDelay)
import GHC.Stack

{- $stream
    The 'Stream' data type is an effectful series of steps with some
    payload value at the bottom. The steps are represented with functors.
    The effects are represented with some /control/ monad. (Control monads
    must be bound to exactly once; see the documentation in
    <https://github.com/tweag/linear-base/tree/master/src/Control/Monad/Linear.hs linear-base> to learn more
    about control monads, control applicatives and control functors.)

    In words, a @Stream f m r@ is either a payload of type @r@, or
    a step of type @f (Stream f m r)@ or an effect of type @m (Stream f m r)@
    where @f@ is a @Control.Functor@ and @m@ is a @Control.Monad@.

    This module exports combinators that pertain to this general case.
    Some of these are quite abstract and pervade any use of the library,
    e.g.

>   maps    :: (forall x . f x #-> g x) -> Stream f m r #-> Stream g m r
>   mapped  :: (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
>   concats :: Stream (Stream f m) m r #-> Stream f m r

    (assuming here and thoughout that @m@ or @n@ satisfies
    a @Control.Monad@ constraint, and @f@ or @g@ a @Control.Functor@
    constraint).

    Others are surprisingly determinate in content:

>   chunksOf     :: Int -> Stream f m r #-> Stream (Stream f m) m r
>   splitsAt     :: Int -> Stream f m r #-> Stream f m (Stream f m r)
>   intercalates :: Stream f m () -> Stream (Stream f m) m r #-> Stream f m r
>   unzips       :: Stream (Compose f g) m r #->  Stream f (Stream g m) r
>   separate     :: Stream (Sum f g) m r -> Stream f (Stream g m) r  -- cp. partitionEithers
>   unseparate   :: Stream f (Stream g) m r -> Stream (Sum f g) m r
>   groups       :: Stream (Sum f g) m r #-> Stream (Sum (Stream f m) (Stream g m)) m r

    One way to see that /any/ streaming library needs some such general type is
    that it is required to represent the segmentation of a stream, and to
    express the equivalents of @Prelude/Data.List@ combinators that involve
    'lists of lists' and the like. See for example this
    <http://www.haskellforall.com/2013/09/perfect-streaming-using-pipes-bytestring.html post>
    on the correct expression of a streaming \'lines\' function.
    The module @Streaming.Prelude@ exports combinators relating to
> Stream (Of a) m r
    where @Of a r = !a :> r@ is a left-strict pair.
   This expresses the concept of a 'Producer' or 'Source' or 'Generator' and
   easily inter-operates with types with such names in e.g. 'conduit',
   'iostreams' and 'pipes'.
-}

-- # Constructing a 'Stream' on a given functor
-------------------------------------------------------------------------------

-- Remark. By default we require `Control.Monad` and `Control.Functor`
-- instances for the `m` and `f` in a `Stream f m r` since these allow the
-- stream to have a `Control.Monad` instance

{-| @yields@ is like @lift@ for items in the streamed functor.
    It makes a singleton or one-layer succession.

> lift :: (Control.Monad m, Control.Functor f)    => m r #-> Stream f m r
> yields ::  (Control.Monad m, Control.Functor f) => f r #-> Stream f m r

    Viewed in another light, it is like a functor-general version of @yield@:

> S.yield a = yields (a :> ())

-}
yields :: (Control.Monad m, Control.Functor f) => f r #-> Stream f m r
yields fr = Step $ Control.fmap Return fr
{-# INLINE yields #-}

-- Note: This must consume its input linearly since it must bind to a
-- `Control.Monad`.
{- | Wrap an effect that returns a stream

> effect = join . lift

-}
effect :: (Control.Monad m, Control.Functor f) =>
  m (Stream f m r) #-> Stream f m r
effect = Effect
{-# INLINE effect #-}

{-| Wrap a new layer of a stream. So, e.g.

> S.cons :: Control.Monad m => a -> Stream (Of a) m r #-> Stream (Of a) m r
> S.cons a str = wrap (a :> str)

   and, recursively:

> S.each' :: Control.Monad m =>  [a] -> Stream (Of a) m ()
> S.each' = foldr (\a b -> wrap (a :> b)) (return ())

   The two operations

> wrap :: (Control.Monad m, Control.Functor f) =>
>   f (Stream f m r) #-> Stream f m r
> effect :: (Control.Monad m, Control.Functor f) =>
>   m (Stream f m r) #-> Stream f m r

   are fundamental. We can define the parallel operations @yields@ and @lift@
   in terms of them

> yields :: (Control.Monad m, Control.Functor f) => f r #-> Stream f m r
> yields = wrap . Control.fmap Control.return
> lift ::  (Control.Monad m, Control.Functor f)  => m r #-> Stream f m r
> lift = effect . Control.fmap Control.return

-}
wrap :: (Control.Monad m, Control.Functor f) =>
  f (Stream f m r) #-> Stream f m r
wrap = Step
{-# INLINE wrap #-}

{- | Repeat a functorial layer, command or instruction a fixed number of times.

-}
replicates :: (HasCallStack, Control.Monad m, Control.Functor f) =>
  Int -> f () -> Stream f m ()
replicates n f = replicates' n f
  where
    replicates' :: (HasCallStack, Control.Monad m, Control.Functor f) =>
      Int -> f () -> Stream f m ()
    replicates' n f = case compare n 0 of
      LT -> Prelude.error "replicates called with negative integer"
      EQ -> Return ()
      GT -> Step $ Control.fmap (\() -> replicates (n-1) f) f
{-# INLINE replicates #-}

-- | @replicatesM n@ repeats an effect containing a functorial layer, command
-- or instruction @n@ times.
replicatesM :: forall f m . (Control.Monad m, Control.Functor f) =>
  Int -> m (f ()) -> Stream f m ()
replicatesM = loop
  where
    loop :: Int -> m (f ()) -> Stream f m ()
    loop n mfstep
      | n <= 0 = Return ()
      | Prelude.otherwise = Effect $
          Control.fmap (Step . Control.fmap (\() -> loop (n-1) mfstep)) mfstep
{-# INLINABLE replicatesM #-}

unfold :: (Control.Monad m, Control.Functor f) =>
  (s #-> m (Either r (f s))) -> s #-> Stream f m r
unfold step state = unfold' step state
  where
    unfold' :: (Control.Monad m, Control.Functor f) =>
      (s #-> m (Either r (f s))) -> s #-> Stream f m r
    unfold' step state = Effect $ Control.do
      either <- step state
      either & \case
        Left r -> Control.return $ Return r
        Right (fs) -> Control.return $ Step $ Control.fmap (unfold step) fs
{-# INLINABLE unfold #-}

-- Note. To keep restrictions minimal, we use the `Data.Applicative`
-- instance.
untilJust :: forall f m r . (Control.Monad m, Data.Applicative f) =>
  m (Maybe r) -> Stream f m r
untilJust action = loop
  where
    loop :: Stream f m r
    loop = Effect $ Control.do
      maybeVal  <- action
      maybeVal & \case
        Nothing -> Control.return $ Step $ Data.pure loop
        Just r  -> Control.return $ Return r
{-# INLINABLE untilJust #-}

-- Remark. The linear church encoding of streams has linear
-- return, effect and step functions.
{- | Reflect a church-encoded stream; cp. @GHC.Exts.build@

> streamFold return_ effect_ step_ (streamBuild psi) = psi return_ effect_ step_
-}
streamBuild ::
  (forall b. (r #-> b) -> (m b #-> b) -> (f b #-> b) -> b) -> Stream f m r
streamBuild = \phi -> phi Return Effect Step
{-# INLINE streamBuild #-}

-- Note. To keep requirements minimal, we use the `Data.Applicative`
-- instance instead of the `Control.Applicative` instance.
delays :: forall f r . (Data.Applicative f) => Double -> Stream f IO r
delays seconds = loop
  where
    loop :: Stream f IO r
    loop = Effect $ Control.do
      let delay = fromInteger (Prelude.truncate (1000000 * seconds))
      () <- fromSystemIO $ threadDelay delay
      Control.return $ Step $ Data.pure loop
{-# INLINABLE delays #-}


-- # Transforming streams
-------------------------------------------------------------------------------

{- | Map layers of one functor to another with a transformation.

> maps id = id
> maps f . maps g = maps (f . g)

-}
maps :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x . f x #-> g x) -> Stream f m r #-> Stream g m r
maps = Stream.maps
{-# INLINE maps #-}

{- | Map layers of one functor to another with a transformation.

> mapsPost id = id
> mapsPost f . mapsPost g = mapsPost (f . g)
> mapsPost f = maps f

     @mapsPost@ is essentially the same as 'maps', but it imposes a @Control.Functor@ constraint on
     its target functor rather than its source functor. It should be preferred if @Control.fmap@
     is cheaper for the target functor than for the source functor.
-}
mapsPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> g x) -> Stream f m r #-> Stream g m r
mapsPost = Stream.mapsPost
{-# INLINE mapsPost #-}

-- Note. The transformation function must be linear so that the stream
-- held inside a control functor is used linearly.
{- | Map layers of one functor to another with a transformation involving the base monad.
     'maps' is more fundamental than @mapsM@, which is best understood as a convenience
     for effecting this frequent composition:

> mapsM phi = decompose . maps (Compose . phi)

     The streaming prelude exports the same function under the better name @mapped@,
     which overlaps with the lens libraries.

-}
mapsM :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapsM transform = loop where
  loop :: Stream f m r #-> Stream g m r
  loop stream = stream & \case
    Return r -> Return r
    Step f -> Effect $ Control.fmap Step $ transform $ Control.fmap loop f
    Effect m -> Effect $ Control.fmap loop m
{-# INLINE mapsM #-}

{- | Map layers of one functor to another with a transformation involving the base monad.
     @mapsMPost@ is essentially the same as 'mapsM', but it imposes a @Control.Functor@ constraint on
     its target functor rather than its source functor. It should be preferred if @Control.fmap@
     is cheaper for the target functor than for the source functor.

     @mapsPost@ is more fundamental than @mapsMPost@, which is best understood as a convenience
     for effecting this frequent composition:

> mapsMPost phi = decompose . mapsPost (Compose . phi)

     The streaming prelude exports the same function under the better name @mappedPost@,
     which overlaps with the lens libraries.

-}
mapsMPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapsMPost = Stream.mapsMPost
{-# INLINE mapsMPost #-}

{- | Map layers of one functor to another with a transformation involving the base monad.
     This could be trivial, e.g.

> let noteBeginning text x = (fromSystemIO (System.putStrLn text)) Control.>> (Control.return x)

     this is completely functor-general

     @maps@ and @mapped@ obey these rules:

> maps id              = id
> mapped return        = id
> maps f . maps g      = maps (f . g)
> mapped f . mapped g  = mapped (f <=< g)
> maps f . mapped g    = mapped (fmap f . g)
> mapped f . maps g    = mapped (f <=< fmap g)

     @maps@ is more fundamental than @mapped@, which is best understood as a convenience
     for effecting this frequent composition:

> mapped phi = decompose . maps (Compose . phi)


-}
mapped :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapped = mapsM
{-# INLINE mapped #-}

{-| A version of 'mapped' that imposes a @Control.Functor@ constraint on the target functor rather
    than the source functor. This version should be preferred if @Control.fmap@ on the target
    functor is cheaper.

-}
mappedPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mappedPost = mapsMPost
{-# INLINE mappedPost #-}

-- | A less-efficient version of 'hoist' that works properly even when its
-- argument is not a monad morphism.
hoistUnexposed :: forall f m n r. (Control.Monad m, Control.Functor f)
               => (forall a. m a #-> n a) -> Stream f m r #-> Stream f n r
hoistUnexposed trans = loop where
  loop :: Stream f m r #-> Stream f n r
  loop = Effect
    . trans
    . inspectC
      (Control.return . Return)
      (Control.return . Step . Control.fmap loop)
{-# INLINABLE hoistUnexposed #-}

-- A version of 'inspect' that takes explicit continuations.
-- Note that due to the linear constructors of 'Stream', these continuations
-- are linear.
inspectC :: forall f m r a. Control.Monad m =>
  (r #-> m a) -> (f (Stream f m r) #-> m a) -> Stream f m r #-> m a
inspectC f g = loop where
  loop :: Stream f m r #-> m a
  loop (Return r) = f r
  loop (Step x)   = g x
  loop (Effect m) = m Control.>>= loop
{-# INLINE inspectC #-}

{-| Group layers in an alternating stream into adjoining sub-streams
    of one type or another.
-}
groups :: forall f g m r .
  (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream (Sum f g) m r #-> Stream (Sum (Stream f m) (Stream g m)) m r
groups = loop
  where
    loop :: Stream (Sum f g) m r #-> Stream (Sum (Stream f m) (Stream g m)) m r
    loop str = Control.do
      e <- Control.lift $ inspect str
      e & \case
        Left r -> Control.return r
        Right ostr -> ostr & \case
          InR gstr -> Step $ InR $ Control.fmap loop $ cleanR (Step (InR gstr))
          InL fstr -> Step $ InL $ Control.fmap loop $ cleanL (Step (InL fstr))

    cleanL :: Stream (Sum f g) m r #-> Stream f m (Stream (Sum f g) m r)
    cleanL = go
      where
        go :: Stream (Sum f g) m r #-> Stream f m (Stream (Sum f g) m r)
        go s = Control.do
         e <- Control.lift $ inspect s
         e & \case
          Left r -> Control.return $ Control.return r
          Right (InL fstr) -> Step $ Control.fmap go fstr
          Right (InR gstr) -> Control.return $ Step (InR gstr)

    cleanR  :: Stream (Sum f g) m r #-> Stream g m (Stream (Sum f g) m r)
    cleanR = go
      where
        go :: Stream (Sum f g) m r #-> Stream g m (Stream (Sum f g) m r)
        go s = Control.do
         e <- Control.lift $ inspect s
         e & \case
          Left r           -> Control.return $ Control.return r
          Right (InL fstr) -> Control.return $ Step (InL fstr)
          Right (InR gstr) -> Step$ Control.fmap go gstr
{-# INLINABLE groups #-}


-- # Inspecting a Stream
-------------------------------------------------------------------------------

{-| Inspect the first stage of a freely layered sequence.
    Compare @Pipes.next@ and the replica @Streaming.Prelude.next@.
    This is the 'uncons' for the general 'unfold'.

> unfold inspect = id
> Streaming.Prelude.unfoldr StreamingPrelude.next = id
-}
inspect :: forall f m r . Control.Monad m =>
     Stream f m r #-> m (Either r (f (Stream f m r)))
inspect = loop
  where
    loop :: Stream f m r #-> m (Either r (f (Stream f m r)))
    loop stream = stream & \case
      Return r -> Control.return (Left r)
      Effect m -> m Control.>>= loop
      Step fs  -> Control.return (Right fs)
{-# INLINABLE inspect #-}


-- # Splitting and joining 'Stream's
-------------------------------------------------------------------------------

{-| Split a succession of layers after some number, returning a streaming or
    effectful pair.

>>> rest <- S.print $ S.splitAt 1 $ each' [1..3]
1
>>> S.print rest
2
3

> splitAt 0 = return
> (\stream -> splitAt n stream >>= splitAt m) = splitAt (m+n)

    Thus, e.g.

>>> rest <- S.print $ (\s -> splitsAt 2 s >>= splitsAt 2) each' [1..5]
1
2
3
4
>>> S.print rest
5

-}
splitsAt :: forall f m r .
  (HasCallStack, Control.Monad m, Control.Functor f) =>
  Int -> Stream f m r #-> Stream f m (Stream f m r)
splitsAt n stream = loop n stream
  where
    loop :: Int -> Stream f m r #-> Stream f m (Stream f m r)
    loop n stream = case compare n 0 of
      LT -> Prelude.error "splitsAt called with negative index" $ stream
      EQ -> Return stream
      GT -> stream & \case
        Return r -> Return $ Return r
        Effect m -> Effect $ Control.fmap (loop n) m
        Step f -> Step $ Control.fmap (loop (n-1)) f
{-# INLINABLE splitsAt #-}


{-| Break a stream into substreams each with n functorial layers.

>>>  S.print $ mapped S.sum $ chunksOf 2 $ each' [1,1,1,1,1]
2
2
1
-}
chunksOf :: forall f m r .
  (HasCallStack, Control.Monad m, Control.Functor f) =>
  Int -> Stream f m r #-> Stream (Stream f m) m r
chunksOf n stream = loop n stream
  where
    loop :: Int -> Stream f m r #-> Stream (Stream f m) m r
    loop _ (Return r) = Return r
    loop n stream = Step $ Control.fmap (loop n) $ splitsAt n stream
{-# INLINABLE chunksOf #-}

{-| Dissolves the segmentation into layers of @Stream f m@ layers.

-}
concats :: forall f m r . (Control.Monad m, Control.Functor f) =>
  Stream (Stream f m) m r #-> Stream f m r
concats = loop
  where
    loop :: Stream (Stream f m) m r #-> Stream f m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step f -> Control.do
        rest <- Control.fmap loop f
        rest
{-# INLINE concats #-}

-- Note. To keep the monad of the stream a control monad, we need
-- `(t m)` to be a control monad, and hence `t` to be a control
-- monad transformer.
{-| Interpolate a layer at each segment. This specializes to e.g.

> intercalates :: Stream f m () -> Stream (Stream f m) m r #-> Stream f m r
-}
intercalates :: forall t m r x .
  (Control.Monad m, Control.Monad (t m), Control.MonadTrans t, Consumable x) =>
  t m x -> Stream (t m) m r #-> t m r
intercalates sep = go0
  where
    go0 :: Stream (t m) m r #-> t m r
    go0 f = f & \case
      Return r -> Control.return r
      Effect m -> Control.lift m Control.>>= go0
      Step fstr -> Control.do
        f' <- fstr
        go1 f'

    go1 :: Stream (t m) m r #-> t m r
    go1 f = f & \case
      Return r -> Control.return r
      Effect m -> Control.lift m Control.>>= go1
      Step fstr -> Control.do
        x  <- sep
        Control.return $ consume x
        f' <- fstr
        go1 f'
{-# INLINABLE intercalates #-}


-- # Zipping, unzipping, separating and unseparating streams
-------------------------------------------------------------------------------

unzips :: forall f g m r .
  (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream (Compose f g) m r #-> Stream f (Stream g m) r
unzips str = destroyExposed
  str
  (\(Compose fgstr) -> Step (Control.fmap (Effect . yields) fgstr))
  (Effect . Control.lift)
  Return
{-# INLINABLE unzips #-}

{-| Given a stream on a sum of functors, make it a stream on the left functor,
    with the streaming on the other functor as the governing monad. This is
    useful for acting on one or the other functor with a fold, leaving the
    other material for another treatment. It generalizes
    'Data.Either.partitionEithers', but actually streams properly.

>>> let odd_even = S.maps (S.distinguish even) $ S.each' [1..10::Int]
>>> :t separate odd_even
separate odd_even
  :: Monad m => Stream (Of Int) (Stream (Of Int) m) ()

    Now, for example, it is convenient to fold on the left and right values separately:

>>> S.toList $ S.toList $ separate odd_even
[2,4,6,8,10] :> ([1,3,5,7,9] :> ())


   Or we can write them to separate files or whatever:

>>> S.writeFile "even.txt" . S.show $ S.writeFile "odd.txt" . S.show $ S.separate odd_even
>>> :! cat even.txt
2
4
6
8
10
>>> :! cat odd.txt
1
3
5
7
9

   Of course, in the special case of @Stream (Of a) m r@, we can achieve the above
   effects more simply by using 'Streaming.Prelude.copy'

>>> S.toList . S.filter even $ S.toList . S.filter odd $ S.copy $ each [1..10::Int]
[2,4,6,8,10] :> ([1,3,5,7,9] :> ())


    But 'separate' and 'unseparate' are functor-general.

-}
separate :: forall f g m r .
  (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream (Sum f g) m r -> Stream f (Stream g m) r
separate str = destroyExposed str construct (Effect . Control.lift) Return
  where
    construct :: Sum f g (Stream f (Stream g m) r) #-> Stream f (Stream g m) r
    construct (InL fss) = Step fss
    construct (InR gss) = Effect (yields gss)
{-# INLINABLE separate #-}

unseparate :: (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream f (Stream g m) r -> Stream (Sum f g) m r
unseparate str = destroyExposed
  str
  (Step . InL)
  (Control.join . maps InR)
  Return
{-# INLINABLE unseparate #-}

{-| Rearrange a succession of layers of the form @Compose m (f x)@.

   we could as well define @decompose@ by @mapsM@:

> decompose = mapped getCompose

  but @mapped@ is best understood as:

> mapped phi = decompose . maps (Compose . phi)

  since @maps@ and @hoist@ are the really fundamental operations that preserve the
  shape of the stream:

> maps  :: (Control.Monad m, Control.Functor f) => (forall x. f x #-> g x) -> Stream f m r #-> Stream g m r
> hoist :: (Control.Monad m, Control.Functor f) => (forall a. m a #-> n a) -> Stream f m r #-> Stream f n r

-}
decompose :: forall f m r . (Control.Monad m, Control.Functor f) =>
  Stream (Compose m f) m r #-> Stream f m r
decompose = loop where
  loop :: Stream (Compose m f) m r #-> Stream f m r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop m
    Step (Compose mfs) -> Effect $ Control.do
      fstream <- mfs
      Control.return $ Step (Control.fmap loop fstream)
{-# INLINABLE decompose #-}

-- Note. For 'loop' to recurse over functoral steps, it must be a
-- linear function, and hence, `ext` must be linear in its second argument.
-- Further, the first argument of `ext` ought to be a linear function,
-- because it is typically applied to the input stream in `ext`, and hence
-- should be linear.
-- | If 'Of' had a @Comonad@ instance, then we'd have
--
-- @copy = expand extend@
--
-- See 'expandPost' for a version that requires a @Control.Functor g@
-- instance instead.
expand :: forall f m r g h . (Control.Monad m, Control.Functor f) =>
  (forall a b. (g a #-> b) -> f a #-> h b) ->
  Stream f m r #-> Stream g (Stream h m) r
expand ext = loop where
  loop :: Stream f m r #-> Stream g (Stream h m) r
  loop (Return r) = Return r
  loop (Step f) = Effect $ Step $ ext (Return . Step) (Control.fmap loop f)
  loop (Effect m) = Effect $ Effect $ Control.fmap (Return . loop) m
{-# INLINABLE expand #-}

-- See note on 'expand'.
-- | If 'Of' had a @Comonad@ instance, then we'd have
--
-- @copy = expandPost extend@
--
-- See 'expand' for a version that requires a @Control.Functor f@ instance
-- instead.
expandPost :: forall f m r g h . (Control.Monad m, Control.Functor g) =>
  (forall a b. (g a #-> b) -> f a #-> h b) ->
  Stream f m r #-> Stream g (Stream h m) r
expandPost ext = loop where
  loop :: Stream f m r #-> Stream g (Stream h m) r
  loop (Return r) = Return r
  loop (Step f) = Effect $ Step $ ext (Return . Step . Control.fmap loop) f
  loop (Effect m) = Effect $ Effect $ Control.fmap (Return . loop) m
{-# INLINABLE expandPost #-}


-- # Eliminating a 'Stream'
-------------------------------------------------------------------------------

-- Note. Since the functor step is held linearly in the
-- 'Stream' datatype, the first argument must be a linear function
-- in order to linearly consume the 'Step' case of a stream.
{-| Map each layer to an effect, and run them all.
-}
mapsM_ :: (Control.Functor f, Control.Monad m) =>
  (forall x . f x #-> m x) -> Stream f m r #-> m r
mapsM_ f = run . maps f
{-# INLINE mapsM_ #-}

{-| Run the effects in a stream that merely layers effects.
-}
run :: Control.Monad m => Stream m m r #-> m r
run = loop
  where
    loop :: Control.Monad m => Stream m m r #-> m r
    loop stream = stream & \case
      Return r   -> Control.return r
      Effect  m  -> m Control.>>= loop
      Step mrest -> mrest Control.>>= loop
{-# INLINABLE run #-}

{-| 'streamFold' reorders the arguments of 'destroy' to be more akin
    to @foldr@  It is more convenient to query in ghci to figure out
    what kind of \'algebra\' you need to write.

>>> :t streamFold Control.return Control.join
(Control.Monad m, Control.Functor f) =>
     (f (m a) #-> m a) -> Stream f m a #-> m a        -- iterT

>>> :t streamFold Control.return (Control.join . Control.lift)
(Control.Monad m, Control.Monad (t m), Control.Functor f, Control.MonadTrans t) =>
     (f (t m a) #-> t m a) -> Stream f m a #-> t m a  -- iterTM

>>> :t streamFold Control.return effect
(Control.Monad m, Control.Functor f, Control.Functor g) =>
     (f (Stream g m r) #-> Stream g m r) -> Stream f m r #-> Stream g m r

>>> :t \f -> streamFold Control.return effect (wrap . f)
(Control.Monad m, Control.Functor f, Control.Functor g) =>
     (f (Stream g m a) #-> g (Stream g m a))
     -> Stream f m a #-> Stream g m a                 -- maps

>>> :t \f -> streamFold Control.return effect (effect . Control.fmap wrap . f)
(Control.Monad m, Control.Functor f, Control.Functor g) =>
     (f (Stream g m a) #-> m (g (Stream g m a)))
     -> Stream f m a #-> Stream g m a                 -- mapped

@
    streamFold done eff construct
       = eff . iterT (Control.return . construct . Control.fmap eff) . Control.fmap done
@
-}
streamFold :: (Control.Functor f, Control.Monad m) =>
     (r #-> b) -> (m b #-> b) ->  (f b #-> b) -> Stream f m r #-> b
streamFold done theEffect construct stream =
  destroy stream construct theEffect done
{-# INLINE streamFold #-}

{-| Specialized fold following the usage of @Control.Monad.Trans.Free@

> iterT alg = streamFold Control.return Control.join alg
> iterT alg = runIdentityT . iterTM (IdentityT . alg . Control.fmap runIdentityT)
-}
iterT :: (Control.Functor f, Control.Monad m) =>
  (f (m a) #-> m a) -> Stream f m a #-> m a
iterT out stream = destroyExposed stream out Control.join Control.return
{-# INLINE iterT #-}

{-| Specialized fold following the usage of @Control.Monad.Trans.Free@

> iterTM alg = streamFold Control.return (Control.join . Control.lift)
> iterTM alg = iterT alg . hoist Control.lift
-}
iterTM ::
  ( Control.Functor f, Control.Monad m
  , Control.MonadTrans t, Control.Monad (t m)) =>
  (f (t m a) #-> t m a) -> Stream f m a #-> t m a
iterTM out stream =
  destroyExposed stream out (Control.join . Control.lift) Control.return
{-# INLINE iterTM #-}

-- Note. 'destroy' needs to use linear functions in its church encoding
-- to consume the stream linearly.
{-| Map a stream to its church encoding; compare @Data.List.foldr@.
    'destroyExposed' may be more efficient in some cases when
    applicable, but it is less safe.

    @
    destroy s construct eff done
      = eff .
        iterT (Control.return . construct . Control.fmap eff) .
        Control.fmap done $ s
    @
-}
destroy :: forall f m r b . (Control.Functor f, Control.Monad m) =>
     Stream f m r #-> (f b #-> b) -> (m b #-> b) -> (r #-> b) -> b
destroy stream0 construct theEffect done = theEffect (loop stream0)
  where
    loop :: Stream f m r #-> m b
    loop stream = stream & \case
      Return r -> Control.return $ done r
      Effect m -> m Control.>>= loop
      Step f -> Control.return $ construct $ Control.fmap (theEffect . loop) f
{-# INLINABLE destroy #-}

