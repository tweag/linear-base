{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Compact.Pure
  ( ShowHeap (_showHeap),
    _showHeapPrim,
    showHeap,
    Region,
    IsRegion,
    RegionContext,
    pattern RegionContext,
    withRegion,
    Dest,
    fill,
    (<|),
    fillComp,
    (<|.),
    fillLeaf,
    (<|..),
    pattern C,
    complete,
    completeExtract,
    Incomplete,
    alloc,
  )
where

import Compact.Pure.Internal
