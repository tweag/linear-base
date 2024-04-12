{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Compact.Pure
  ( ShowHeap (_showHeap),
    _showHeapPrim,
    showHeap,
    RegionContext,
    RegionToken,
    withRegion,
    Dest,
    Fill (fill),
    DestsOf,
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
