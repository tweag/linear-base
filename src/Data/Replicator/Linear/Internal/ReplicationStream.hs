{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Replicator.Linear.Internal.ReplicationStream
  ( ReplicationStream (..),
    consume,
    duplicate,
    map,
    pure,
    (<*>),
    liftA2,
  )
where

import Data.Unrestricted.Linear.Internal.Ur
import Prelude.Linear.Internal

-- | @ReplicationStream s g dup2 c@ is the infinite linear stream
-- @repeat (g s)@ where @dup2@ is used to make as many copies of @s@ as
-- necessary, and @c@ is used to consume @s@ when consuming the stream.
--
-- Although it isn't enforced at type level, @dup2@ should abide by the same
-- laws as 'Data.Unrestricted.Linear.dup2':
-- * @first c (dup2 a) ≃ a ≃ second c (dup2 a)@ (neutrality)
-- * @first dup2 (dup2 a) ≃ (second dup2 (dup2 a))@ (associativity)
--
-- This type is solely used to implement 'Data.Replicator.Linear'
data ReplicationStream a where
  ReplicationStream ::
    s %1 ->
    (s %1 -> a) ->
    (s %1 -> (s, s)) ->
    (s %1 -> ()) ->
    ReplicationStream a

consume :: ReplicationStream a %1 -> ()
consume (ReplicationStream s _ _ consumes) = consumes s
{-# INLINEABLE consume #-}

duplicate :: ReplicationStream a %1 -> ReplicationStream (ReplicationStream a)
duplicate (ReplicationStream s give dups consumes) =
  ReplicationStream
    s
    (\s' -> ReplicationStream s' give dups consumes)
    dups
    consumes

map :: (a %1 -> b) -> ReplicationStream a %1 -> ReplicationStream b
map f (ReplicationStream s give dups consumes) =
  ReplicationStream s (f . give) dups consumes

pure :: a -> ReplicationStream a
pure x =
  ReplicationStream
    (Ur x)
    unur
    ( \case
        Ur x' -> (Ur x', Ur x')
    )
    ( \case
        Ur _ -> ()
    )

(<*>) :: ReplicationStream (a %1 -> b) %1 -> ReplicationStream a %1 -> ReplicationStream b
(ReplicationStream sf givef dupsf consumesf) <*> (ReplicationStream sx givex dupsx consumesx) =
  ReplicationStream
    (sf, sx)
    (\(sf', sx') -> givef sf' (givex sx'))
    ( \(sf', sx') ->
        case (dupsf sf', dupsx sx') of
          ((sf1, sf2), (sx1, sx2)) -> ((sf1, sx1), (sf2, sx2))
    )
    ( \(sf', sx') ->
        case consumesf sf' of
          () -> consumesx sx'
    )

liftA2 :: (a %1 -> b %1 -> c) -> ReplicationStream a %1 -> ReplicationStream b %1 -> ReplicationStream c
liftA2 f (ReplicationStream sa givea dupsa consumesa) (ReplicationStream sb giveb dupsb consumesb) =
  ReplicationStream
    (sa, sb)
    (\(sa', sb') -> f (givea sa') (giveb sb'))
    ( \(sa', sb') ->
        case (dupsa sa', dupsb sb') of
          ((sa1, sa2), (sb1, sb2)) -> ((sa1, sb1), (sa2, sb2))
    )
    ( \(sa', sb') ->
        case consumesa sa' of
          () -> consumesb sb'
    )
-- We need to inline this to get good results with generic deriving
-- of Dupable.
{-# INLINE liftA2 #-}

infixl 4 <*> -- same fixity as base.<*>
