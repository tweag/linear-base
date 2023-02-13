{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}

module Compact.Pure where

import Control.Applicative ((<$>))
import Control.Monad (Monad (return, (>>=)), forM)
import Foreign (plusPtr)
import Foreign.Storable
import GHC.Classes
import GHC.Compact (Compact (..), compact, compactAdd, getCompact)
import GHC.Exts
import GHC.IO (unsafePerformIO)
import Prelude.Linear hiding (Any, seq, (==), (||))
import Unsafe.Linear (toLinear, toLinear2)
import qualified Data.Functor.Linear as Data
import qualified Control.Functor.Linear as Control
import Control.Functor.Linear.Internal.Instances (Data)

getWordSize :: IO Int
getWordSize = return 8

getHeaderSize :: IO Int
getHeaderSize = return 8

debugEnabled :: Bool
debugEnabled = True

putDebugLn :: String -> IO ()
putDebugLn x = if debugEnabled then putStrLn $ "[DEBUG] " ++ x else return ()

-------------------------------------------------------------------------------
-- Helpers to display heap objects
-------------------------------------------------------------------------------
class ShowHeap a where
  _showHeap :: Int -> a -> IO String
  dispHeap :: a -> IO ()
  dispHeap x = _showHeap 0 x >>= putStrLn

getInfoPtr :: a -> IO Word
getInfoPtr x = do
  rawPtr <- aToRawPtr x
  peek rawPtr

getPairInfoPtr :: IO Word
getPairInfoPtr = do
  let !p = (0 :: Int, 0 :: Int)
  getInfoPtr p

getLeftInfoPtr :: IO Word
getLeftInfoPtr = do
  let !p = Left (0 :: Int)
  getInfoPtr p

getRightInfoPtr :: IO Word
getRightInfoPtr = do
  let !p = Right (0 :: Int)
  getInfoPtr p

getUrInfoPtr :: IO Word
getUrInfoPtr = do
  let !p = Ur (0 :: Int)
  getInfoPtr p

getIntInfoPtr :: IO Word
getIntInfoPtr = getInfoPtr (1 :: Int)

getCharInfoPtr :: IO Word
getCharInfoPtr = getInfoPtr 'A'

instance ShowHeap Int where
  _showHeap indent x = do
    xAddr <- ptrToWord <$> aToRawPtr x
    rawX <- showRaw 1 x
    infoPtr <- getInfoPtr x
    evaluatedInfoPtr <- getIntInfoPtr
    if infoPtr == evaluatedInfoPtr
      then do
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show xAddr
            ++ ": "
            ++ show x
            ++ " "
            ++ rawX
      else do
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show xAddr
            ++ ": THUNK <Int> "
            ++ rawX

instance ShowHeap Char where
  _showHeap indent x = do
    xAddr <- ptrToWord <$> aToRawPtr x
    rawX <- showRaw 1 x
    infoPtr <- getInfoPtr x
    evaluatedInfoPtr <- getCharInfoPtr
    if infoPtr == evaluatedInfoPtr
      then do
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show xAddr
            ++ ": "
            ++ show x
            ++ " "
            ++ rawX
      else do
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show xAddr
            ++ ": THUNK <Char> "
            ++ rawX

instance ShowHeap String where
  _showHeap indent x = do
    rawX <- showRaw 9 x
    xAddr <- ptrToWord <$> aToRawPtr x
    return $
      (replicate (2 * indent) ' ')
        ++ "@"
        ++ show xAddr
        ++ ": String "
        ++ rawX

instance (Show a, Show b, ShowHeap a, ShowHeap b) => ShowHeap (a, b) where
  _showHeap indent p = do
    headerSize <- getHeaderSize
    wordSize <- getWordSize
    pAddr <- ptrToWord <$> aToRawPtr p
    rawP <- showRaw 2 p
    infoPtr <- getInfoPtr p
    evaluatedInfoPtr <- getPairInfoPtr
    if infoPtr == evaluatedInfoPtr
      then do
        pp <- aToRawPtr p
        lAsWord <- peek $ pp `plusPtr` headerSize
        rAsWord <- peek $ pp `plusPtr` headerSize `plusPtr` wordSize
        !showL <- case wordToPtr' lAsWord :: Ptr' a of Ptr' l -> _showHeap (indent + 1) l
        !showR <- case wordToPtr' rAsWord :: Ptr' b of Ptr' r -> _showHeap (indent + 1) r
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show pAddr
            ++ ": (_, _) "
            ++ rawP
            ++ "\n"
            ++ showL
            ++ "\n"
            ++ showR
      else do
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show pAddr
            ++ ": THUNK <(a, b)> "
            ++ rawP

instance (Show a, Show b, ShowHeap a, ShowHeap b) => ShowHeap (Either a b) where
  _showHeap indent v = do
    vAddr <- ptrToWord <$> aToRawPtr v
    headerSize <- getHeaderSize
    rawV <- showRaw 1 v
    infoPtr <- getInfoPtr v
    evaluatedLeftInfoPtr <- getLeftInfoPtr
    evaluatedRightInfoPtr <- getRightInfoPtr
    px <- aToRawPtr v
    xAsWord <- peek $ px `plusPtr` headerSize
    if infoPtr == evaluatedLeftInfoPtr
      then do
        !showX <- case wordToPtr' xAsWord :: Ptr' a of Ptr' x -> _showHeap (indent + 1) x
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show vAddr
            ++ ": Left _ "
            ++ rawV
            ++ "\n"
            ++ showX
      else
        if infoPtr == evaluatedRightInfoPtr
          then do
            !showX <- case wordToPtr' xAsWord :: Ptr' b of Ptr' x -> _showHeap (indent + 1) x
            return $
              (replicate (2 * indent) ' ')
                ++ "@"
                ++ show vAddr
                ++ ": Right _ "
                ++ rawV
                ++ "\n"
                ++ showX
          else do
            return $
              (replicate (2 * indent) ' ')
                ++ "@"
                ++ show vAddr
                ++ ": THUNK <Either a b> "
                ++ rawV

instance (Show a, ShowHeap a) => ShowHeap (Ur a) where
  _showHeap indent u = do
    uAddr <- ptrToWord <$> aToRawPtr u
    headerSize <- getHeaderSize
    rawU <- showRaw 1 u
    infoPtr <- getInfoPtr u
    evaluatedInfoPtr <- getUrInfoPtr
    if infoPtr == evaluatedInfoPtr
      then do
        pu <- aToRawPtr u
        xAsWord <- peek $ pu `plusPtr` headerSize
        !showX <- case wordToPtr' xAsWord :: Ptr' a of Ptr' x -> _showHeap (indent + 1) x
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show uAddr
            ++ ": Ur _ "
            ++ rawU
            ++ "\n"
            ++ showX
      else do
        return $
          (replicate (2 * indent) ' ')
            ++ "@"
            ++ show uAddr
            ++ ": THUNK <Ur a> "
            ++ rawU

showRaw :: Int -> a -> IO String
showRaw n x =
  unwords <$> do
    p <- aToRawPtr x
    wordSize <- getWordSize
    headerSize <- getHeaderSize
    h <- forM [0 .. (headerSize `div` wordSize) - 1] $ \k -> do
      w <- peek (p `plusPtr` (k * wordSize)) :: IO Word
      return $ "[0]" ++ show w
    r <- forM [0 .. n - 1] $ \k -> do
      w <- peek (p `plusPtr` headerSize `plusPtr` (k * wordSize)) :: IO Word
      return $ "[h+" ++ show k ++ "]" ++ show w
    return $ h ++ r
{-# NOINLINE showRaw #-}

-------------------------------------------------------------------------------
-- Region and dests
-------------------------------------------------------------------------------

data FirstInhabitant = FirstInhabitant Int

firstInhabitant :: FirstInhabitant
firstInhabitant = FirstInhabitant 1234

data RegionId

data CompactRegion (r :: RegionId) where CompactRegion :: Compact FirstInhabitant -> CompactRegion r

instance Consumable (CompactRegion r) where
  consume (CompactRegion _) = ()

instance Dupable (CompactRegion r) where
  dup2 (CompactRegion c) = (CompactRegion c, CompactRegion c)

-- withRegion :: (forall r. CompactRegion r %1 -> Ur b) %1 -> Ur b
-- withRegion = toLinear _withRegion

{-# NOINLINE withRegion #-}
withRegion :: forall b. (forall (r :: RegionId). CompactRegion r %1 -> Ur b) -> Ur b
withRegion f =
  unsafePerformIO $ do
    c <- (compact firstInhabitant)
    firstPtr <- ptrToWord <$> (aToRawPtr $ getCompact c)
    putDebugLn $
      "withRegion: allocating new region around @"
        ++ (show firstPtr)
    return $! f (CompactRegion c)

data Dest r a = Dest {parentWriteLoc :: Ptr Word, region :: CompactRegion r}

-------------------------------------------------------------------------------
-- Primitives to do unsafe things
-------------------------------------------------------------------------------

data Ptr' a = Ptr' a

ptrToPtr' :: Ptr a -> Ptr' a
ptrToPtr' p = let !r = p in unsafeCoerce# r

ptr'ToPtr :: Ptr' a -> Ptr a
ptr'ToPtr p = let !r = p in unsafeCoerce# r

instance Show a => Show (Ptr' a) where
  show (Ptr' x) = "Ptr' " ++ show x

align# :: Int -> Word# -> Word#
align# wordSize w =
  case intToWord (wordSize - 1) of
    W# mask -> w `and#` (not# (mask))

-------------------------------------------------------------------------------
-- Helpers to do unsafe things derived from primitives above
-------------------------------------------------------------------------------

addr2Word# :: Addr# -> Word#
addr2Word# addr# = int2Word# (addr2Int# addr#)

word2Addr# :: Word# -> Addr#
word2Addr# word# = int2Addr# (word2Int# word#)

align :: Ptr a -> IO (Ptr Word)
align (Ptr addr#) = do
  wordSize <- getWordSize
  let word# = addr2Word# addr#
      wordAligned# = align# wordSize word#
      addrAligned# = word2Addr# wordAligned#
  -- putDebugLn $
  --   "addr: "
  --     ++ (show $ Ptr addr#)
  --     ++ " word: "
  --     ++ (show $ W# word#)
  --     ++ " wordAligned: "
  --     ++ (show $ W# wordAligned#)
  --     ++ " addrAligned: "
  --     ++ (show $ Ptr addrAligned#)
  return $ Ptr addrAligned#

ptrToA :: Ptr a -> a
ptrToA p =
  case ptrToPtr' p of
    Ptr' res -> res

aToPtr :: a -> Ptr a
aToPtr x = ptr'ToPtr (Ptr' x)

aToRawPtr :: a -> IO (Ptr Word)
aToRawPtr x = align (aToPtr x)

aToWord :: a -> Word
aToWord x = ptrToWord (aToPtr x)

wordToA :: Word -> a
wordToA w = ptrToA (wordToPtr w)

ptrToWord :: Ptr a -> Word
ptrToWord (Ptr addr#) = W# (addr2Word# addr#)

wordToPtr :: Word -> Ptr a
wordToPtr (W# word#) = Ptr (word2Addr# word#)

intToWord :: Int -> Word
intToWord (I# int#) = W# (int2Word# int#)

wordToInt :: Word -> Int
wordToInt (W# word#) = I# (word2Int# word#)

wordToPtr' :: Word -> Ptr' a
wordToPtr' w = ptrToPtr' (wordToPtr w)

ptr'ToWord :: Ptr' a -> Word
ptr'ToWord p = ptrToWord (ptr'ToPtr p)

-------------------------------------------------------------------------------
-- Alloc & fill API
-------------------------------------------------------------------------------

placeholder :: Int
placeholder = 1339

fillLeaf :: Dest r a %1 -> a -> ()
fillLeaf = toLinear2 _fillLeaf

{-# NOINLINE _fillLeaf #-}
_fillLeaf :: Dest r a -> a -> ()
_fillLeaf Dest {region = CompactRegion c, parentWriteLoc} x =
  unsafePerformIO $ do
    !xInRegion <- getCompact <$> (compactAdd c x)
    let childrenPtrAsWord = aToWord xInRegion
    putDebugLn $
      "fillLeaf: @"
        ++ show (ptrToWord parentWriteLoc)
        ++ " <- #"
        ++ show childrenPtrAsWord
        ++ ": [value]"
    poke parentWriteLoc childrenPtrAsWord

fillPair :: Dest r (a, b) %1 -> (Dest r a, Dest r b)
fillPair = toLinear _fillPair

{-# NOINLINE _fillPair #-}
_fillPair :: Dest r (a, b) -> (Dest r a, Dest r b)
_fillPair Dest {region = r@(CompactRegion c), parentWriteLoc} =
  unsafePerformIO $ do
    wordSize <- getWordSize
    headerSize <- getHeaderSize
    !pairInRegion <- getCompact <$> (compactAdd c (unsafeCoerce# placeholder :: a, unsafeCoerce# placeholder :: b))
    p <- aToRawPtr pairInRegion
    let childrenPtrAsWord = aToWord pairInRegion
        !pl = p `plusPtr` headerSize
        !dl = Dest {region = r, parentWriteLoc = pl}
        !pr = p `plusPtr` headerSize `plusPtr` wordSize
        !dr = Dest {region = r, parentWriteLoc = pr}
    putDebugLn $
      "fillPair: @"
        ++ show (ptrToWord parentWriteLoc)
        ++ " <- #"
        ++ show childrenPtrAsWord
        ++ ": (_ , _) with slots ["
        ++ (show $ ptrToWord pl)
        ++ ", "
        ++ (show $ ptrToWord pr)
        ++ "]"
    poke parentWriteLoc childrenPtrAsWord
    return $! (dl, dr)

fillLeft :: Dest r (Either a b) %1 -> Dest r a
fillLeft = toLinear $ _fillLeft

{-# NOINLINE _fillLeft #-}
_fillLeft :: Dest r (Either a b) -> Dest r a
_fillLeft Dest {region = r@(CompactRegion c), parentWriteLoc} =
  unsafePerformIO $ do
    headerSize <- getHeaderSize
    !leftInRegion <- getCompact <$> (compactAdd c $ Left (unsafeCoerce# placeholder :: a))
    p <- aToRawPtr leftInRegion
    let childrenPtrAsWord = aToWord leftInRegion
        !px = p `plusPtr` headerSize
        !dx = Dest {region = r, parentWriteLoc = px}
    putDebugLn $
      "fillLeft: @"
        ++ show (ptrToWord parentWriteLoc)
        ++ " <- #"
        ++ show childrenPtrAsWord
        ++ ": Left _ with slot ["
        ++ (show $ ptrToWord px)
        ++ "]"
    poke parentWriteLoc childrenPtrAsWord
    return dx

fillRight :: Dest r (Either a b) %1 -> Dest r b
fillRight = toLinear $ _fillRight

{-# NOINLINE _fillRight #-}
_fillRight :: Dest r (Either a b) -> Dest r b
_fillRight Dest {region = r@(CompactRegion c), parentWriteLoc} =
  unsafePerformIO $ do
    headerSize <- getHeaderSize
    !rightInRegion <- getCompact <$> (compactAdd c $ Right (unsafeCoerce# placeholder :: b))
    p <- aToRawPtr rightInRegion
    let childrenPtrAsWord = aToWord rightInRegion
        !px = p `plusPtr` headerSize
        !dx = Dest {region = r, parentWriteLoc = px}
    putDebugLn $
      "fillRight: @"
        ++ show (ptrToWord parentWriteLoc)
        ++ " <- #"
        ++ show childrenPtrAsWord
        ++ ": Right _ with slot ["
        ++ (show $ ptrToWord px)
        ++ "]"
    poke parentWriteLoc childrenPtrAsWord
    return dx

data Incomplete (r :: RegionId) a b = Incomplete {rootReceiver :: Ur a, dests :: b, pInitialParentWriteLoc :: Ptr Word} deriving Data.Functor via Data (Incomplete r a)

alloc :: CompactRegion r %1 -> Incomplete r a (Dest r a)
alloc = toLinear _alloc

{-# NOINLINE _alloc #-}
_alloc :: CompactRegion r -> Incomplete r a (Dest r a)
_alloc r@(CompactRegion c) =
  unsafePerformIO $ do
    headerSize <- getHeaderSize
    !rootReceiver <- getCompact <$> (compactAdd c $ (unsafeCoerce# (Ur placeholder) :: Ur a))
    p <- aToRawPtr rootReceiver
    let parentWriteLoc = p `plusPtr` headerSize
    !pwlHolder <- getCompact <$> (compactAdd c $ parentWriteLoc)
    let !initialDest = Dest {parentWriteLoc = pwlHolder, region = r}
    pHolder <- aToRawPtr pwlHolder
    let !pParentWriteLoc = pHolder `plusPtr` headerSize
    putDebugLn $
      "alloc: [region] <- #"
        ++ (show $ aToWord rootReceiver)
        ++ ": Ur _ with slot ["
        ++ (show $ ptrToWord parentWriteLoc)
        ++ "]"
    testParentWriteLoc <- peek pParentWriteLoc
    putDebugLn $
      "alloc: parentWriteLoc in initial dest is stored at @"
        ++ (show $ ptrToWord pParentWriteLoc) ++ " and is #" ++ (show testParentWriteLoc)
    return $! Incomplete rootReceiver initialDest pParentWriteLoc

instance Control.Functor (Incomplete r a) where
  fmap f (Incomplete u d pp) = Incomplete u (f d) pp

fillComp :: Dest r a %1 -> Incomplete r a b %1 -> b
fillComp = toLinear2 _fillComp

-- idea: put PwlHolder itself in the compact region so that I can update master parentWriteLoc if needed in compose
{-# NOINLINE _fillComp #-}
_fillComp :: Dest r a -> Incomplete r a b -> b
_fillComp Dest {parentWriteLoc = bParentWriteLoc} Incomplete {rootReceiver = sRootReceiver, dests = sDests, pInitialParentWriteLoc} =
  unsafePerformIO $ do
    headerSize <- getHeaderSize
    pSRootReceiver <- aToRawPtr sRootReceiver
    valueInSRootReceiver <- peek $ pSRootReceiver `plusPtr` headerSize
    poke bParentWriteLoc valueInSRootReceiver -- in case something as already been written to the initial dest, we write the value stored in rootReceiver of the small struct at parentWriteLoc of the big one.
    rawSRootReceiver <- showRaw 1 sRootReceiver
    putDebugLn $
      "fillComp: @"
        ++ (show $ ptrToWord bParentWriteLoc)
        ++ " <- #"
        ++ (show $ valueInSRootReceiver)
        ++ " (copied from root receiver Ur _: "
        ++ rawSRootReceiver
        ++ ")"

    oldSlot <- peek pInitialParentWriteLoc
    poke pInitialParentWriteLoc (ptrToWord bParentWriteLoc) -- in case the initial dest of the small struct hasn't been used yet, then we replace parentWriteLoc with the one of the big struct. That can only happen when the small struct is the result of a fresh alloc
    putDebugLn $
      "fillComp: @" ++ (show $ ptrToWord pInitialParentWriteLoc) ++ " <- #" ++ (show $ ptrToWord bParentWriteLoc) ++ " (changing slot carried by Dest from " ++ (show oldSlot) ++ " to " ++ (show $ ptrToWord bParentWriteLoc) ++ ")"

    return $! sDests

complete :: Incomplete r a () %1 -> Ur a
complete = toLinear _complete

{-# NOINLINE _complete #-}
_complete :: Incomplete r a () -> Ur a
_complete (Incomplete u d _) = d `lseq` u
