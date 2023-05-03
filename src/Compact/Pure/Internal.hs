{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O -ddump-to-file #-}
{-# OPTIONS_HADDOCK hide #-}

module Compact.Pure.Internal where

import Control.Functor.Linear (Data)
import Control.Functor.Linear qualified as Control
import Control.Monad (forM)
import Data.Data (Proxy (Proxy))
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Semigroup (stimesMonoid)
import Data.Unrestricted.Linear.Internal.Consumable
import Data.Unrestricted.Linear.Internal.Dupable
import Data.Unrestricted.Linear.Internal.Ur
import Foreign (Storable (poke), peek, plusPtr)
import GHC.Compact (Compact, compact, compactAdd, getCompact)
import GHC.Exts
import GHC.Generics
import GHC.IO (unsafePerformIO)
import GHC.TypeLits
import Unsafe.Linear (toLinear, toLinear2)

-------------------------------------------------------------------------------
-- Helpers for display/debug
-------------------------------------------------------------------------------

getWordSize :: IO Int
getWordSize = return 8

getHeaderSize :: IO Int
getHeaderSize = return 8

debugEnabled :: Bool
debugEnabled = False

-- TODO: is this any useful?
{-# INLINE putDebugLn #-}
putDebugLn :: String -> IO ()
putDebugLn x = if debugEnabled then putStrLn $ "[DEBUG] " ++ x else return ()

placeholder :: Int
placeholder = 1339

unknownName :: String
unknownName = "<unknown>"

-------------------------------------------------------------------------------
-- Primitives to do unsafe things
-------------------------------------------------------------------------------

data Ptr' a = Ptr' a

ptrToPtr' :: Ptr a -> Ptr' a
ptrToPtr' p = let !r = p in unsafeCoerce# r

ptr'ToPtr :: Ptr' a -> Ptr a
ptr'ToPtr p = let !r = p in unsafeCoerce# r

instance (Show a) => Show (Ptr' a) where
  show (Ptr' x) = "Ptr' " ++ show x

{-# INLINE align# #-}
align# :: Int# -> Word# -> Word#
align# wordSize# w# =
  let mask = int2Word# (wordSize# -# 1#)
   in w# `and#` (not# mask)

-------------------------------------------------------------------------------
-- Helpers to do unsafe things derived from primitives above
-------------------------------------------------------------------------------

addr2Word# :: Addr# -> Word#
addr2Word# addr# = int2Word# (addr2Int# addr#)

word2Addr# :: Word# -> Addr#
word2Addr# word# = int2Addr# (word2Int# word#)

align :: Ptr a -> IO (Ptr Word)
align (Ptr addr#) = do
  I# wordSize# <- getWordSize
  let word# = addr2Word# addr#
      wordAligned# = align# wordSize# word#
      addrAligned# = word2Addr# wordAligned#
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

getInfoPtr :: a -> IO Word
getInfoPtr x = do
  rawPtr <- aToRawPtr x
  peek rawPtr

getCtorInfoPtr :: forall (symCtor :: Symbol) (a :: Type). (Generic a, GShallow symCtor (Rep a ())) => IO Word
getCtorInfoPtr = let !evaluated = shallowTerm @symCtor @a in getInfoPtr evaluated

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

-------------------------------------------------------------------------------
-- Helpers to do unsafe things derived from primitives above
-------------------------------------------------------------------------------

data DatatypeData = DatatypeData
  { dtypeName :: String,
    dtypeModName :: String,
    dtypePackageName :: String,
    dtypeIsNewType :: Bool
  }

getDatatypeData :: forall meta. (Datatype meta) => DatatypeData
getDatatypeData =
  DatatypeData
    { dtypeName = datatypeName @meta undefined,
      dtypeModName = moduleName @meta undefined,
      dtypePackageName = packageName @meta undefined,
      dtypeIsNewType = isNewtype @meta undefined
    }

data CtorData = CtorData {ctorName :: String, ctorFixity :: Fixity, ctorIsRecord :: Bool}

getCtorData :: forall meta. (Constructor meta) => CtorData
getCtorData =
  CtorData
    { ctorName = conName @meta undefined,
      ctorFixity = conFixity @meta undefined,
      ctorIsRecord = conIsRecord @meta undefined
    }

data SelectorData = SelectorData
  { selecName :: String,
    selecUnpackedness :: SourceUnpackedness,
    selecSrcStrictness :: SourceStrictness,
    selecFinalStrictness :: DecidedStrictness
  }

getSelectorData :: forall meta. (Selector meta) => SelectorData
getSelectorData =
  SelectorData
    { selecName = let n = selName @meta undefined in if null n then "" else "." ++ n ++ " ", -- TODO: detect when no sel and return Nothing
      selecUnpackedness = selSourceUnpackedness @meta undefined,
      selecSrcStrictness = selSourceStrictness @meta undefined,
      selecFinalStrictness = selDecidedStrictness @meta undefined
    }

class ShowHeap (a :: Type) where
  _showHeap :: Int -> String -> a -> IO String

instance {-# OVERLAPPING #-} ShowHeap Int where
  _showHeap = _showHeapPrim "Int" 1 0

instance {-# OVERLAPPING #-} ShowHeap Char where
  _showHeap = _showHeapPrim "Char" 1 'a'

instance (Generic a, repA ~ Rep a (), ctors ~ GCtorsOf repA, ShowTryCtors ctors a) => ShowHeap a where
  _showHeap = _showTryCtors @ctors @a

_showHeapPrim :: String -> Int -> a -> (Int -> String -> a -> IO String)
_showHeapPrim typeName n representative indent selectorRepr x = do
  pX <- aToRawPtr x
  evaluatedInfoPtr <- let !r = representative in getInfoPtr r
  actualInfoPtr <- getInfoPtr x
  if actualInfoPtr == evaluatedInfoPtr
    then do
      rawX <- showRaw n x
      return $
        (replicate (2 * indent) ' ')
          ++ selectorRepr
          ++ "@"
          ++ show (ptrToWord pX)
          ++ " = [value] :: "
          ++ typeName
          ++ " "
          ++ rawX
    else do
      rawX <- showRaw 0 x
      return $
        (replicate (2 * indent) ' ')
          ++ "@"
          ++ show (ptrToWord pX)
          ++ " = THUNK :: "
          ++ typeName
          ++ " "
          ++ rawX

class ShowTryCtors (ctors :: [(Meta, [(Meta, Type)])]) (a :: Type) where
  _showTryCtors :: Int -> String -> a -> IO String

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA) => ShowTryCtors '[] a where
  _showTryCtors indent selectorRepr x = do
    pAddr <- ptrToWord <$> aToRawPtr x
    rawP <- showRaw 0 x
    let DatatypeData {..} = getDatatypeData @metaA
    return $
      (replicate (2 * indent) ' ')
        ++ selectorRepr
        ++ "@"
        ++ show pAddr
        ++ " = THUNK :: "
        ++ dtypeName
        ++ " "
        ++ rawP

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, Constructor metaCtor, 'MetaCons symCtorbol f r ~ metaCtor, GShallow symCtor repA, ShowTryCtors otherCtors a, ShowFields fields, arity ~ Length fields, KnownNat arity) => ShowTryCtors ('(metaCtor, fields) : otherCtors) a where
  _showTryCtors indent selectorRepr x = do
    evaluatedInfoPtr <- getCtorInfoPtr @symCtor @a
    actualInfoPtr <- getInfoPtr x
    if evaluatedInfoPtr == actualInfoPtr
      then do
        pX <- aToRawPtr x
        let arity = fromInteger $ natVal (Proxy :: Proxy arity)
        rawP <- showRaw arity x
        let DatatypeData {..} = getDatatypeData @metaA
        let CtorData {..} = getCtorData @metaCtor
        next <- _showFields @fields (indent + 1) pX 0
        return $
          (replicate (2 * indent) ' ')
            ++ selectorRepr
            ++ "@"
            ++ show (ptrToWord pX)
            ++ " = "
            ++ ctorName
            ++ " "
            ++ (stimesMonoid arity "_ ")
            ++ ":: "
            ++ dtypeName
            ++ " "
            ++ rawP
            ++ "\n"
            ++ next
      else _showTryCtors @otherCtors @a indent selectorRepr x

class ShowFields (fields :: [(Meta, Type)]) where
  _showFields :: Int -> Ptr a -> Int -> IO String

instance ShowFields '[] where
  _showFields _ _ _ = return ""

instance (ShowHeap fieldType, ShowFields others, Selector metaSel) => ShowFields ('(metaSel, fieldType) : others) where
  _showFields indent pX fieldOffset = do
    headerSize <- getHeaderSize
    wordSize <- getWordSize
    fieldAsWord <- peek $ pX `plusPtr` headerSize `plusPtr` (wordSize * fieldOffset)
    let SelectorData {..} = getSelectorData @metaSel
    showField <- case wordToPtr' fieldAsWord :: Ptr' t of Ptr' field -> _showHeap @fieldType indent selecName field
    showNext <- _showFields @others indent pX (fieldOffset + 1)
    return $ showField ++ "\n" ++ showNext

showHeap :: forall a. (ShowHeap a) => a -> String
showHeap x = unsafePerformIO $ _showHeap @a 0 "" x

class GShallow (n :: Symbol) a where
  gShallowTerm :: a

instance (GShallow n (f p), GShallow n (g p)) => GShallow n ((f :*: g) p) where
  gShallowTerm = gShallowTerm @n @(f p) :*: gShallowTerm @n @(g p)

instance GShallow n (U1 p) where
  gShallowTerm = U1

instance GShallow n (K1 i c p) where
  gShallowTerm = K1 (unsafeCoerce# placeholder :: c)

instance (GShallow n (f p)) => GShallow n (M1 i c f p) where
  gShallowTerm = M1 (gShallowTerm @n @(f p))

instance (b ~ IsJust (GCtorInfoOf symCtor (f p)), IfT b (GShallow symCtor (f p)) (GShallow symCtor (g p)), KnownBool b) => GShallow symCtor ((f :+: g) p) where
  gShallowTerm = ifV @b (L1 $ gShallowTerm @symCtor @(f p)) (R1 $ gShallowTerm @symCtor @(g p))

shallowTerm :: forall (symCtor :: Symbol) a. (Generic a, GShallow symCtor (Rep a ())) => a
shallowTerm = to @a $ gShallowTerm @symCtor @(Rep a ())

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

data Dest a r = Dest {parentWriteLoc :: Ptr Word, region :: CompactRegion r}

data CtorSelector (symCtor :: Symbol) = C

(<|) :: forall (symCtor :: Symbol) a (r :: RegionId) ctor. ('Just ctor ~ GCtorInfoOf symCtor (Rep a ()), Fill ctor a) => Dest a r %1 -> CtorSelector symCtor %1 -> DestsOf ctor a r
d <| C = fill @symCtor d

(<|.) :: Dest a r %1 -> Incomplete a b r %1 -> b
(<|.) = fillComp

(<|..) :: Dest a r %1 -> a -> ()
(<|..) = fillLeaf

-- (.<|.) :: Control.Functor f => Dest a1 r %1 -> Incomplete a1 (a2 %1 -> b) r %1 -> f a2 %1 -> f b
-- x .<|. y = (<&> (x <|. y))

-- (.<|) :: (GCtorInfoOf symCtor (Rep a1 ()) ~ 'Just ctor, DestsOf ctor a1 r ~ (a2 %1 -> b), Control.Functor f, Fill ctor a1) => Dest a1 r %1 -> CtorSelector symCtor %1 -> f a2 %1 -> f b
-- x .<| y = (<&> (x <| y))

fillComp :: Dest a r %1 -> Incomplete a b r %1 -> b
fillComp = toLinear2 _fillComp

fillLeaf :: Dest a r %1 -> a -> ()
fillLeaf = toLinear2 _fillLeaf

isNullPtr :: Ptr a -> Bool
isNullPtr (Ptr addr#) = isTrue# (addr2Int# addr# ==# 0#)

nullPtr :: Ptr a
nullPtr = Ptr (int2Addr# 0#)

{-# NOINLINE _fillComp #-}
_fillComp :: Dest a r -> Incomplete a b r -> b
_fillComp Dest {parentWriteLoc = bParentWriteLoc} Incomplete {rootReceiver = sRootReceiver, dests = sDests, pInitialParentWriteLoc} =
  unsafePerformIO $ do
    headerSize <- getHeaderSize
    pSRootReceiver <- aToRawPtr sRootReceiver
    valueInSRootReceiver <- peek $ pSRootReceiver `plusPtr` headerSize
    poke bParentWriteLoc valueInSRootReceiver -- in case something as already been written to the initial dest, we write the value stored in rootReceiver of the small struct at parentWriteLoc of the big one.
    if (isNullPtr pInitialParentWriteLoc)
      then
        putDebugLn $
          "fillComp: @"
            ++ (show $ ptrToWord bParentWriteLoc)
            ++ " <- [value]"
      else do
        poke pInitialParentWriteLoc (ptrToWord bParentWriteLoc) -- in case the initial dest of the small struct hasn't been used yet, then we replace parentWriteLoc with the one of the big struct. That can only happen when the small struct is the result of a fresh alloc
        putDebugLn $
          "fillComp: @"
            ++ (show $ ptrToWord bParentWriteLoc)
            ++ " <- #"
            ++ (show $ valueInSRootReceiver)
            ++ " (copying address stored in root receiver of small struct)"
            ++ "  /\\  @"
            ++ (show $ ptrToWord pInitialParentWriteLoc)
            ++ " <- #"
            ++ (show $ ptrToWord bParentWriteLoc)
            ++ " (changing slot carried by initial dest of small struct)"
    return $! sDests

{-# NOINLINE _fillLeaf #-}
_fillLeaf :: Dest a r -> a -> ()
_fillLeaf Dest {region = CompactRegion c, parentWriteLoc} x =
  unsafePerformIO $ do
    !xInRegion <- getCompact <$> (compactAdd c x)
    let pXAsWord = aToWord xInRegion
    poke parentWriteLoc pXAsWord
    putDebugLn $
      "fillLeaf: @"
        ++ show (ptrToWord parentWriteLoc)
        ++ " <- #"
        ++ show pXAsWord
        ++ ": [value]"

complete :: Incomplete a () r %1 -> Ur a
complete = toLinear _complete

completeExtract :: Incomplete a (Ur b) r %1 -> Ur (a, b)
completeExtract = toLinear _completeExtract

{-# NOINLINE _complete #-}
_complete :: Incomplete a () r -> Ur a
_complete (Incomplete u d _) = case d of () -> _hide u

-- TODO: should we put the new Ur wrapper inside the compact region?
{-# NOINLINE _completeExtract #-}
_completeExtract :: Incomplete a (Ur b) r -> Ur (a, b)
_completeExtract (Incomplete u d _) = case d of
  Ur y -> case _hide u of Ur x -> Ur (x, y)

type family DestsOf (ctor :: (Meta, [(Meta, Type)])) (a :: Type) (r :: RegionId) where
  -- DestsOf '( ('MetaCons "leaf" _ _), '[]) a r = a %1 -> ()
  -- DestsOf '( ('MetaCons "comp" _ _), '[]) a r = forall b. Incomplete a b r %1 -> b
  DestsOf '(_, '[]) _ r = ()
  DestsOf '(_, '[ '(_, t)]) _ r = Dest t r
  DestsOf '(_, '[ '(_, t1), '(_, t2)]) _ r = (Dest t1 r, Dest t2 r)
  DestsOf '(_, '[ '(_, t1), '(_, t2), '(_, t3)]) _ r = (Dest t1 r, Dest t2 r, Dest t3 r)
  DestsOf '(_, '[ '(_, t1), '(_, t2), '(_, t3), '(_, t4)]) _ r = (Dest t1 r, Dest t2 r, Dest t3 r, Dest t4 r)
  DestsOf '(_, '[ '(_, t1), '(_, t2), '(_, t3), '(_, t4), '(_, t5)]) _ r = (Dest t1 r, Dest t2 r, Dest t3 r, Dest t4 r, Dest t5 r)
  DestsOf '(_, '[ '(_, t1), '(_, t2), '(_, t3), '(_, t4), '(_, t5), '(_, t6)]) _ r = (Dest t1 r, Dest t2 r, Dest t3 r, Dest t4 r, Dest t5 r, Dest t6 r)
  DestsOf '(_, '[ '(_, t1), '(_, t2), '(_, t3), '(_, t4), '(_, t5), '(_, t6), '(_, t7)]) _ r = (Dest t1 r, Dest t2 r, Dest t3 r, Dest t4 r, Dest t5 r, Dest t6 r, Dest t7 r)
  DestsOf _ _ _ = TypeError ('Text "DestsOf not implemented for constructors with more than 7 fields")

newtype FillComp a b r = FillComp (Incomplete a b r)

fill :: forall (symCtor :: Symbol) a (r :: RegionId) ctor. ('Just ctor ~ GCtorInfoOf symCtor (Rep a ()), Fill ctor a) => Dest a r %1 -> DestsOf ctor a r
fill = toLinear (\d -> unsafePerformIO (_fill @ctor @a d))

class Fill (ctor :: (Meta, [(Meta, Type)])) (a :: Type) where
  _fill :: forall (r :: RegionId). Dest a r -> IO (DestsOf ctor a r)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Fill '(metaCtor, '[]) a where
  _fill :: forall (r :: RegionId). Dest a r -> IO ()
  _fill Dest {region = CompactRegion c, parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd c (shallowTerm @symCtor @a))
    let CtorData {..} = getCtorData @metaCtor
        pXAsWord = aToWord xInRegion
    poke parentWriteLoc pXAsWord
    putDebugLn $
      "fill0: @"
        ++ show (ptrToWord parentWriteLoc)
        ++ " <- #"
        ++ show pXAsWord
        ++ ": "
        ++ ctorName

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Fill '(metaCtor, '[ '( 'MetaSel f u ss ds, t)]) a where
  _fill :: forall (r :: RegionId). Dest a r -> IO (Dest t r)
  _fill Dest {region = r@(CompactRegion c), parentWriteLoc} = do
    headerSize <- getHeaderSize
    !xInRegion <- getCompact <$> (compactAdd c (shallowTerm @symCtor @a))
    pXRaw <- aToRawPtr xInRegion
    let CtorData {..} = getCtorData @metaCtor
        pXAsWord = aToWord xInRegion
        pF = pXRaw `plusPtr` headerSize
        dF = Dest {region = r, parentWriteLoc = pF}
    poke parentWriteLoc pXAsWord
    putDebugLn $
      "fill1: @"
        ++ show (ptrToWord parentWriteLoc)
        ++ " <- #"
        ++ show pXAsWord
        ++ ": "
        ++ ctorName
        ++ " _@"
        ++ (show $ ptrToWord pF)
    return dF

_untypedFillN :: forall metaCtor a (r :: RegionId) repA metaA symCtor fix hasSel. (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Dest a r -> Int -> IO [Dest Any r]
_untypedFillN Dest {region = r@(CompactRegion c), parentWriteLoc} n = do
  headerSize <- getHeaderSize
  wordSize <- getWordSize
  !xInRegion <- getCompact <$> (compactAdd c (shallowTerm @symCtor @a))
  pXRaw <- aToRawPtr xInRegion
  let CtorData {..} = getCtorData @metaCtor
      pXAsWord = aToWord xInRegion
      pF0 = pXRaw `plusPtr` headerSize
      (fieldsRepr, dests) = unzip (makeDest <$> [0, 1 .. n - 1])
      makeDest k = let pF = pF0 `plusPtr` (k * wordSize) in ("_@" ++ (show $ ptrToWord pF), Dest {region = r, parentWriteLoc = pF})
  poke parentWriteLoc pXAsWord
  putDebugLn $
    "fill"
      ++ (show n)
      ++ ": @"
      ++ show (ptrToWord parentWriteLoc)
      ++ " <- #"
      ++ show pXAsWord
      ++ ": "
      ++ ctorName
      ++ " "
      ++ (intercalate " " fieldsRepr)
  return dests

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Fill '(metaCtor, '[ '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2)]) a where
  _fill :: forall (r :: RegionId). Dest a r -> IO (Dest t1 r, Dest t2 r)
  _fill d = do
    [d1, d2] <- _untypedFillN @metaCtor @a @r d 2
    return $ (unsafeCoerce# d1 :: Dest t1 r, unsafeCoerce# d2 :: Dest t2 r)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Fill '(metaCtor, '[ '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3)]) a where
  _fill :: forall (r :: RegionId). Dest a r -> IO (Dest t1 r, Dest t2 r, Dest t3 r)
  _fill d = do
    [d1, d2, d3] <- _untypedFillN @metaCtor @a @r d 3
    return $ (unsafeCoerce# d1 :: Dest t1 r, unsafeCoerce# d2 :: Dest t2 r, unsafeCoerce# d3 :: Dest t3 r)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Fill '(metaCtor, '[ '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4)]) a where
  _fill :: forall (r :: RegionId). Dest a r -> IO (Dest t1 r, Dest t2 r, Dest t3 r, Dest t4 r)
  _fill d = do
    [d1, d2, d3, d4] <- _untypedFillN @metaCtor @a @r d 4
    return $ (unsafeCoerce# d1 :: Dest t1 r, unsafeCoerce# d2 :: Dest t2 r, unsafeCoerce# d3 :: Dest t3 r, unsafeCoerce# d4 :: Dest t4 r)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Fill '(metaCtor, '[ '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5)]) a where
  _fill :: forall (r :: RegionId). Dest a r -> IO (Dest t1 r, Dest t2 r, Dest t3 r, Dest t4 r, Dest t5 r)
  _fill d = do
    [d1, d2, d3, d4, d5] <- _untypedFillN @metaCtor @a @r d 5
    return $ (unsafeCoerce# d1 :: Dest t1 r, unsafeCoerce# d2 :: Dest t2 r, unsafeCoerce# d3 :: Dest t3 r, unsafeCoerce# d4 :: Dest t4 r, unsafeCoerce# d5 :: Dest t5 r)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Fill '(metaCtor, '[ '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6)]) a where
  _fill :: forall (r :: RegionId). Dest a r -> IO (Dest t1 r, Dest t2 r, Dest t3 r, Dest t4 r, Dest t5 r, Dest t6 r)
  _fill d = do
    [d1, d2, d3, d4, d5, d6] <- _untypedFillN @metaCtor @a @r d 6
    return $ (unsafeCoerce# d1 :: Dest t1 r, unsafeCoerce# d2 :: Dest t2 r, unsafeCoerce# d3 :: Dest t3 r, unsafeCoerce# d4 :: Dest t4 r, unsafeCoerce# d5 :: Dest t5 r, unsafeCoerce# d6 :: Dest t6 r)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => Fill '(metaCtor, '[ '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6), '( 'MetaSel f7 u7 ss7 ds7, t7)]) a where
  _fill :: forall (r :: RegionId). Dest a r -> IO (Dest t1 r, Dest t2 r, Dest t3 r, Dest t4 r, Dest t5 r, Dest t6 r, Dest t7 r)
  _fill d = do
    [d1, d2, d3, d4, d5, d6, d7] <- _untypedFillN @metaCtor @a @r d 7
    return $ (unsafeCoerce# d1 :: Dest t1 r, unsafeCoerce# d2 :: Dest t2 r, unsafeCoerce# d3 :: Dest t3 r, unsafeCoerce# d4 :: Dest t4 r, unsafeCoerce# d5 :: Dest t5 r, unsafeCoerce# d6 :: Dest t6 r, unsafeCoerce# d7 :: Dest t7 r)

type family Length (a :: [k]) :: Nat where
  Length '[] = 0
  Length (_ : xs) = 1 + Length xs
  Length _ = TypeError ('Text "No match for Length")

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ y = y
  (x : xs) ++ y = x : (xs ++ y)
  _ ++ _ = TypeError ('Text "No match for ++")

type family GDatatypeMetaOf (repA :: Type) :: Meta where
  GDatatypeMetaOf (D1 meta f p) = meta
  GDatatypeMetaOf (M1 _ _ f p) = GDatatypeMetaOf (f p)
  GDatatypeMetaOf _ = TypeError ('Text "No match for GDatatypeMetaOf")

type family GFieldsOf (repA :: Type) :: [(Meta, Type)] where
  GFieldsOf (S1 meta f p) = '[ '(meta, GSlotTypeOf (f p))]
  GFieldsOf (U1 _) = '[]
  GFieldsOf ((f :*: g) p) = GFieldsOf (f p) ++ GFieldsOf (g p)
  GFieldsOf (M1 _ _ f p) = GFieldsOf (f p)
  GFieldsOf _ = TypeError ('Text "No match for GFieldsOf")

type family GSlotTypeOf (repA :: Type) :: Type where
  GSlotTypeOf (K1 _ c _) = c
  GSlotTypeOf (M1 _ _ f p) = GSlotTypeOf (f p)
  GSlotTypeOf _ = TypeError ('Text "No match for GSlotTypeOf")

type family GCtorsOf (repA :: Type) :: [(Meta, [(Meta, Type)])] where
  GCtorsOf (C1 meta f p) = '[ '(meta, GFieldsOf (f p))]
  GCtorsOf (M1 _ _ f p) = GCtorsOf (f p)
  GCtorsOf ((f :+: g) p) = GCtorsOf (f p) ++ GCtorsOf (g p)
  GCtorsOf _ = TypeError ('Text "No match for GCtorsOf")

type family GCtorInfoOf (symCtor :: Symbol) (repA :: Type) :: Maybe (Meta, [(Meta, Type)]) where
  -- GCtorInfoOf "leaf" _ = 'Just '( ('MetaCons "leaf" 'PrefixI 'False), '[])
  -- GCtorInfoOf "comp" _ = 'Just '( ('MetaCons "comp" 'PrefixI 'False), '[])
  GCtorInfoOf symCtor (C1 ('MetaCons symCtor x y) f p) = 'Just '(('MetaCons symCtor x y), GFieldsOf (f p))
  GCtorInfoOf symCtor (C1 ('MetaCons _ _ _) _ _) = 'Nothing
  GCtorInfoOf symCtor ((f :+: g) p) = GCtorInfoOf symCtor (f p) <|> GCtorInfoOf symCtor (g p)
  GCtorInfoOf symCtor (V1 _) = 'Nothing
  GCtorInfoOf symCtor (M1 _ _ f p) = GCtorInfoOf symCtor (f p)
  GCtorInfoOf _ _ = TypeError ('Text "No match for GHasCtor")

type family IsJust (x :: Maybe k) :: Bool where
  IsJust ('Just v) = 'True
  IsJust 'Nothing = 'False

type family (x :: Maybe k) <|> (y :: Maybe k) :: Maybe k where
  ('Just v) <|> _ = 'Just v
  'Nothing <|> y = y

type family (a :: Bool) || (b :: Bool) :: Bool where
  'False || b = b
  'True || _ = 'True

type family IfT (a :: Bool) (x :: k) (y :: k) :: k where
  IfT 'True x _ = x
  IfT 'False _ y = y

class KnownBool (b :: Bool) where
  ifV :: ((b ~ 'True) => a) -> ((b ~ 'False) => a) -> a

instance KnownBool 'True where
  ifV t _ = t

instance KnownBool 'False where
  ifV _ f = f

type Incomplete a b r = Incomplete' r a b

data Incomplete' (r :: RegionId) a b = Incomplete {rootReceiver :: Ur a, dests :: b, pInitialParentWriteLoc :: Ptr Word}
  deriving (Data.Functor) via Data (Incomplete' r a)

intoR :: CompactRegion r %1 -> a -> Incomplete a () r
intoR = toLinear2 _intoR

_intoR :: CompactRegion r -> a -> Incomplete a () r
_intoR (CompactRegion c) x =
  unsafePerformIO $ do
    !rootReceiver <- getCompact <$> (compactAdd c $ Ur x)
    putDebugLn $
      "intoR: [region] <- #"
        ++ (show $ aToWord rootReceiver)
        ++ ": Ur [value]"
    return $! Incomplete rootReceiver () nullPtr

instance Control.Functor (Incomplete' r a) where
  fmap f (Incomplete u d pp) = Incomplete u (f d) pp

alloc :: CompactRegion r %1 -> Incomplete a (Dest a r) r
alloc = toLinear _alloc

{-# NOINLINE _hide #-}
_hide :: a -> a
_hide x = x

{-# NOINLINE _alloc #-}
_alloc :: CompactRegion r -> Incomplete a (Dest a r) r
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
        ++ ": Ur _@"
        ++ (show $ ptrToWord parentWriteLoc)
        ++ " (slot address stored at "
        ++ (show $ ptrToWord pParentWriteLoc)
        ++ ")"
    return $! Incomplete rootReceiver initialDest pParentWriteLoc
