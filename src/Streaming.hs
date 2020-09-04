{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streaming
  (
  -- * The 'Stream' and 'Of' types
   module Streaming.Type
  -- -- * Constructing a 'Stream' on a given functor
  , yields
  , effect
  , wrap
  , replicates
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

import Streaming.Type
import Streaming.Process (destroyExposed)
import Data.Functor.Sum
import Data.Functor.Compose
import qualified Streaming.Prelude as Stream
import System.IO.Linear
import Prelude.Linear (($), (.), (&))
import Prelude (Ordering(..), Ord(..), Num(..), Int, Either(..), Double,
               Maybe(..), fromInteger)
import qualified Prelude
import qualified Prelude.Linear as Linear
import qualified Control.Monad.Linear as Control
import qualified Data.Functor.Linear as Data
import Data.Unrestricted.Linear
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)
import Control.Concurrent (threadDelay)
import GHC.Stack


-- # Constructing a 'Stream' on a given functor
-------------------------------------------------------------------------------

-- Remark. By default we require `Control.Monad` and `Control.Functor`
-- instances for the `m` and `f` in a `Stream f m r` since these allow the
-- stream to have a `Control.Monad` instance

yields :: (Control.Monad m, Control.Functor f) => f r #-> Stream f m r
yields fr = Step $ Control.fmap Return fr

-- Note: This must consume its input linearly since it must bind to a
-- `Control.Monad`.
effect :: (Control.Monad m, Control.Functor f) =>
  m (Stream f m r) #-> Stream f m r
effect = Effect

wrap :: (Control.Monad m, Control.Functor f) =>
  f (Stream f m r) #-> Stream f m r
wrap = Step

replicates :: (HasCallStack, Control.Monad m, Control.Functor f) =>
  Int -> f () -> Stream f m ()
replicates n f = case compare n 0 of
  LT -> Prelude.error "replicates called with negative integer"
  EQ -> Return ()
  GT -> Step $ Control.fmap (\() -> replicates (n-1) f) f

unfold :: (Control.Monad m, Control.Functor f) =>
  (s #-> m (Either r (f s))) -> s #-> Stream f m r
unfold step state = Effect $ do
  either <- step state
  either & \case
    Left r -> return $ Return r
    Right (fs) -> return $ Step $ Control.fmap (unfold step) fs
  where
    Builder{..} = monadBuilder

-- Note. To keep restrictions minimal, we use the `Data.Applicative`
-- instance.
untilJust :: forall f m r . (Control.Monad m, Data.Applicative f) =>
  m (Maybe r) -> Stream f m r
untilJust action = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream f m r
    loop = Effect $ do
      maybeVal  <- action
      maybeVal & \case
        Nothing -> return $ Step $ Data.pure loop
        Just r  -> return $ Return r

-- Remark. The linear church encoding of streams has linear
-- return, effect and step functions.
streamBuild ::
  (forall b. (r #-> b) -> (m b #-> b) -> (f b #-> b) -> b) -> Stream f m r
streamBuild = \phi -> phi Return Effect Step

-- Note. To keep requirements minimal, we use the `Data.Applicative`
-- instance instead of the `Control.Applicative` instance.
delays :: forall f r . (Data.Applicative f) => Double -> Stream f IO r
delays seconds = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream f IO r
    loop = Effect $ do
      let delay = fromInteger (Prelude.truncate (1000000 * seconds))
      () <- fromSystemIO $ threadDelay delay
      return $ Step $ Data.pure loop


-- # Transforming streams
-------------------------------------------------------------------------------

maps :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x . f x #-> g x) -> Stream f m r #-> Stream g m r
maps = Stream.maps

mapsPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> g x) -> Stream f m r #-> Stream g m r
mapsPost = Stream.mapsPost

-- Note. The transformation function must be linear so that the stream
-- held inside a control functor is used linearly.
mapsM :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapsM transform = loop where
  loop :: Stream f m r #-> Stream g m r
  loop stream = stream & \case
    Return r -> Return r
    Step f -> Effect $ Control.fmap Step $ transform $ Control.fmap loop f
    Effect m -> Effect $ Control.fmap loop m

mapsMPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapsMPost = Stream.mapsMPost

mapped :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapped = mapsM

mappedPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mappedPost = mapsMPost

hoistUnexposed :: forall f m n r . (Control.Monad m, Control.Functor f) =>
  (forall a. m a #-> n a) -> Stream f m r #-> Stream f n r
hoistUnexposed hoist = loop
  where
    loop :: Stream f m r #-> Stream f n r
    loop stream = stream & \case
      Return r -> Return r
      Step f -> Step $ Control.fmap loop f
      Effect m -> Effect $ hoist $ Control.fmap loop m

groups :: forall f g m r .
  (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream (Sum f g) m r #-> Stream (Sum (Stream f m) (Stream g m)) m r
groups = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream (Sum f g) m r #-> Stream (Sum (Stream f m) (Stream g m)) m r
    loop str = do
      e <- Control.lift $ inspect str
      e & \case
        Left r -> return r
        Right ostr -> ostr & \case
          InR gstr -> Step $ InR $ Control.fmap loop $ cleanR (Step (InR gstr))
          InL fstr -> Step $ InL $ Control.fmap loop $ cleanL (Step (InL fstr))

    cleanL :: Stream (Sum f g) m r #-> Stream f m (Stream (Sum f g) m r)
    cleanL = go
      where
        go :: Stream (Sum f g) m r #-> Stream f m (Stream (Sum f g) m r)
        go s = do
         e <- Control.lift $ inspect s
         e & \case
          Left r -> return $ return r
          Right (InL fstr) -> Step $ Control.fmap go fstr
          Right (InR gstr) -> return $ Step (InR gstr)

    cleanR  :: Stream (Sum f g) m r #-> Stream g m (Stream (Sum f g) m r)
    cleanR = go
      where
        go :: Stream (Sum f g) m r #-> Stream g m (Stream (Sum f g) m r)
        go s = do
         e <- Control.lift $ inspect s
         e & \case
          Left r           -> return $ return r
          Right (InL fstr) -> return $ Step (InL fstr)
          Right (InR gstr) -> Step$ Control.fmap go gstr


-- # Inspecting a Stream
-------------------------------------------------------------------------------

inspect :: forall f m r . Control.Monad m =>
     Stream f m r #-> m (Either r (f (Stream f m r)))
inspect = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream f m r #-> m (Either r (f (Stream f m r)))
    loop stream = stream & \case
      Return r -> return (Left r)
      Effect m -> m >>= loop
      Step fs  -> return (Right fs)


-- # Splitting and joining 'Stream's
-------------------------------------------------------------------------------

splitsAt :: forall f m r .
  (HasCallStack, Control.Monad m, Control.Functor f) =>
  Int -> Stream f m r #-> Stream f m (Stream f m r)
splitsAt n stream = loop n stream
  where
    Builder{..} = monadBuilder
    loop :: Int -> Stream f m r #-> Stream f m (Stream f m r)
    loop n stream = case compare n 0 of
      LT -> Prelude.error "splitsAt called with negative index" $ stream
      EQ -> Return stream
      GT -> stream & \case
        Return r -> Return $ Return r
        Effect m -> Effect $ Control.fmap (loop n) m
        Step f -> Step $ Control.fmap (loop (n-1)) f

chunksOf :: forall f m r .
  (HasCallStack, Control.Monad m, Control.Functor f) =>
  Int -> Stream f m r #-> Stream (Stream f m) m r
chunksOf n stream = loop n stream
  where
    Builder{..} = monadBuilder
    loop :: Int -> Stream f m r #-> Stream (Stream f m) m r
    loop n stream = Step $ Control.fmap (loop n) $ splitsAt n stream

concats :: forall f m r . (Control.Monad m, Control.Functor f) =>
  Stream (Stream f m) m r #-> Stream f m r
concats = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream (Stream f m) m r #-> Stream f m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step f -> do
        rest <- Control.fmap loop f
        rest

-- Note. To keep the monad of the stream a control monad, we need
-- `(t m)` to be a control monad, and hence `t` to be a control
-- monad transformer.
intercalates :: forall t m r x .
  (Control.Monad m, Control.Monad (t m), Control.MonadTrans t, Consumable x) =>
  t m x -> Stream (t m) m r #-> t m r
intercalates sep = go0
  where
    Builder{..} = monadBuilder
    go0 :: Stream (t m) m r #-> t m r
    go0 f = f & \case
      Return r -> return r
      Effect m -> Control.lift m >>= go0
      Step fstr -> do
        f' <- fstr
        go1 f'

    go1 :: Stream (t m) m r #-> t m r
    go1 f = f & \case
      Return r -> return r
      Effect m -> Control.lift m >>= go1
      Step fstr -> do
        x  <- sep
        return $ consume x
        f' <- fstr
        go1 f'


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

separate :: forall f g m r .
  (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream (Sum f g) m r -> Stream f (Stream g m) r
separate str = destroyExposed str construct (Effect . Control.lift) Return
  where
    construct :: Sum f g (Stream f (Stream g m) r) #-> Stream f (Stream g m) r
    construct (InL fss) = Step fss
    construct (InR gss) = Effect (yields gss)

unseparate :: (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream f (Stream g m) r -> Stream (Sum f g) m r
unseparate str = destroyExposed
  str
  (Step . InL)
  (Control.join . maps InR)
  Return

decompose :: forall f m r . (Control.Monad m, Control.Functor f) =>
  Stream (Compose m f) m r #-> Stream f m r
decompose = loop where
  Builder{..} = monadBuilder
  loop :: Stream (Compose m f) m r #-> Stream f m r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop m
    Step (Compose mfs) -> Effect $ do
      fstream <- mfs
      return $ Step (Control.fmap loop fstream)

-- Note. For 'loop' to recurse over functoral steps, it must be a
-- linear function, and hence, `ext` must be linear in its second argument.
-- Further, the first argument of `ext` ought to be a linear function,
-- because it is typically applied to the input stream in `ext`, and hence
-- should be linear.
expand :: forall f m r g h . (Control.Monad m, Control.Functor f) =>
  (forall a b. (g a #-> b) -> f a #-> h b) ->
  Stream f m r #-> Stream g (Stream h m) r
expand ext = loop where
  loop :: Stream f m r #-> Stream g (Stream h m) r
  loop (Return r) = Return r
  loop (Step f) = Effect $ Step $ ext (Return . Step) (Control.fmap loop f)
  loop (Effect m) = Effect $ Effect $ Control.fmap (Return . loop) m

-- See note on 'expand'.
expandPost :: forall f m r g h . (Control.Monad m, Control.Functor g) =>
  (forall a b. (g a #-> b) -> f a #-> h b) ->
  Stream f m r #-> Stream g (Stream h m) r
expandPost ext = loop where
  loop :: Stream f m r #-> Stream g (Stream h m) r
  loop (Return r) = Return r
  loop (Step f) = Effect $ Step $ ext (Return . Step . Control.fmap loop) f
  loop (Effect m) = Effect $ Effect $ Control.fmap (Return . loop) m


-- # Eliminating a 'Stream'
-------------------------------------------------------------------------------

-- Note. Since the functor step is held linearly in the
-- 'Stream' datatype, the first argument must be a linear function
-- in order to linearly consume the 'Step' case of a stream.
mapsM_ :: (Control.Functor f, Control.Monad m) =>
  (forall x . f x #-> m x) -> Stream f m r #-> m r
mapsM_ f = run . maps f

run :: Control.Monad m => Stream m m r #-> m r
run = loop
  where
    Builder{..} = monadBuilder
    loop :: Control.Monad m => Stream m m r #-> m r
    loop stream = stream & \case
      Return r   -> return r
      Effect  m  -> m >>= loop
      Step mrest -> mrest >>= loop

streamFold :: (Control.Functor f, Control.Monad m) =>
     (r #-> b) -> (m b #-> b) ->  (f b #-> b) -> Stream f m r #-> b
streamFold done theEffect construct stream =
  destroy stream construct theEffect done

iterT :: (Control.Functor f, Control.Monad m) =>
  (f (m a) #-> m a) -> Stream f m a #-> m a
iterT out stream = destroyExposed stream out Control.join return
  where
    Builder{..} = monadBuilder

iterTM ::
  ( Control.Functor f, Control.Monad m
  , Control.MonadTrans t, Control.Monad (t m)) =>
  (f (t m a) #-> t m a) -> Stream f m a #-> t m a
iterTM out stream =
  destroyExposed stream out (Control.join . Control.lift) return
  where
    Builder{..} = monadBuilder

-- Note. 'destroy' needs to use linear functions in its church encoding
-- to consume the stream linearly.
destroy :: forall f m r b . (Control.Functor f, Control.Monad m) =>
     Stream f m r #-> (f b #-> b) -> (m b #-> b) -> (r #-> b) -> b
destroy stream0 construct theEffect done = theEffect (loop stream0)
  where
    Builder{..} = monadBuilder
    loop :: Stream f m r #-> m b
    loop stream = stream & \case
      Return r -> return $ done r
      Effect m -> m >>= loop
      Step f -> return $ construct $ Control.fmap (theEffect . loop) f

