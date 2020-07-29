{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}


module Streaming.Type
  ( Stream (..)
  , Of (..)
  ) where

-- # Data Definitions
-------------------------------------------------------------------------------

data Stream f m r where
  Cons :: m (f (Stream f m r)) -> Stream f m r
  Nil :: r -> Stream f m r

data Of a b where


-- Control.Monad for Stream
-- MonadTrans for Stream f m
-- Control.Functor for Of


