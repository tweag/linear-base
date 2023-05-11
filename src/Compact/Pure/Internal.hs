{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Data.Reflection (Reifies (reflect), reify)
import Data.Replicator.Linear.Internal (Replicator (Moved))
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
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Linear (toLinear, toLinear2)

-------------------------------------------------------------------------------
-- Helpers for display/debug
-------------------------------------------------------------------------------

isProfilingEnabled :: Bool
isProfilingEnabled = unsafePerformIO $ do
  intInReg <- getCompact <$> compact (1 :: Word)
  let intAddr = aToRawPtr intInReg
  v <- peek $ intAddr `plusPtr` wordSize
  return $ v /= (1 :: Word)

wordSize :: Int
wordSize = 8

headerSize :: Int
headerSize = if isProfilingEnabled then 24 else 8

debugEnabled :: Bool
debugEnabled = False

-- TODO: is this any useful?
{-# INLINE putDebugLn #-}
putDebugLn :: String -> IO ()
putDebugLn x = if debugEnabled then putStrLn $ "[DEBUG] " ++ x else return ()

placeholder :: Int
placeholder = 1339

-------------------------------------------------------------------------------
-- Primitives to do unsafe things
-------------------------------------------------------------------------------

data Ptr' a = Ptr' a

ptrToPtr' :: Ptr a -> Ptr' a
ptrToPtr' p = let !r = p in unsafeCoerce r

ptr'ToPtr :: Ptr' a -> Ptr a
ptr'ToPtr p = let !r = p in unsafeCoerce r

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

align :: Ptr a -> Ptr Word
align (Ptr addr#) = do
  let !(I# wordSize#) = wordSize
      word# = addr2Word# addr#
      wordAligned# = align# wordSize# word#
      addrAligned# = word2Addr# wordAligned#
   in Ptr addrAligned#

ptrToA :: Ptr a -> a
ptrToA p =
  case ptrToPtr' p of
    Ptr' res -> res

aToPtr :: a -> Ptr a
aToPtr x = ptr'ToPtr (Ptr' x)

aToRawPtr :: a -> Ptr Word
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
  let rawPtr = aToRawPtr x
  peek rawPtr

getCtorInfoPtr :: forall (symCtor :: Symbol) (a :: Type). (Generic a, GShallow symCtor (Rep a ())) => IO Word
getCtorInfoPtr = let !evaluated = shallowTerm @symCtor @a in getInfoPtr evaluated

showRaw :: Int -> a -> IO String
showRaw n x =
  unwords <$> do
    let p = aToRawPtr x
    h <- forM [0 .. (headerSize `div` wordSize) - 1] $ \k -> do
      w <- peek (p `plusPtr` (k * wordSize)) :: IO Word
      return $ "[" ++ show k ++ "]" ++ show w
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
  let pX = aToRawPtr x
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
    let pAddr = ptrToWord $ aToRawPtr x
        DatatypeData {..} = getDatatypeData @metaA
    rawP <- showRaw 0 x
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
        let pX = aToRawPtr x
            arity = fromInteger $ natVal (Proxy :: Proxy arity)
            DatatypeData {..} = getDatatypeData @metaA
            CtorData {..} = getCtorData @metaCtor
        rawP <- showRaw arity x
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
    let SelectorData {..} = getSelectorData @metaSel
    fieldAsWord <- peek $ pX `plusPtr` headerSize `plusPtr` (wordSize * fieldOffset)
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
  gShallowTerm = K1 (unsafeCoerce placeholder :: c)

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

newtype Region = Region {root :: Compact FirstInhabitant}

data RegionToken r where RegionToken :: Region -> RegionToken r

instance Consumable (RegionToken r) where
  consume (RegionToken _) = ()

instance Dupable (RegionToken r) where
  dupR (RegionToken c) = Moved (RegionToken c)

type RegionContext r = Reifies r Region

getRegionRoot :: forall r. (RegionContext r) => Compact FirstInhabitant
getRegionRoot = root $ reflect (Proxy :: Proxy r)

{-# NOINLINE withRegion #-}
withRegion :: forall b. (forall (r :: Type). (RegionContext r) => RegionToken r %1 -> Ur b) -> Ur b
withRegion f =
  unsafePerformIO $ do
    c <- (compact firstInhabitant)
    let !firstInhabitantInRegion = getCompact c
        firstPtr = ptrToWord $ aToRawPtr $ firstInhabitantInRegion
    putDebugLn $
      "withRegion: allocating new region around @"
        ++ (show firstPtr)
    return $! reify (Region {root = c}) (\(proxy :: Proxy s) -> f (RegionToken @s (reflect proxy)))

newtype Dest r a = Dest {parentWriteLoc :: Ptr Word}

data CtorSelector (symCtor :: Symbol) = C

(<|) :: forall (symCtor :: Symbol) r a. (Fill symCtor r a) => Dest r a %1 -> CtorSelector symCtor %1 -> DestsOf symCtor r a
d <| C = fill @symCtor d

(<|.) :: forall r a b. (RegionContext r) => Dest r a %1 -> Incomplete r a b %1 -> b
(<|.) = fillComp

(<|..) :: forall r a. (RegionContext r) => Dest r a %1 -> a -> ()
(<|..) = fillLeaf

fillComp :: forall r a b. (RegionContext r) => Dest r a %1 -> Incomplete r a b %1 -> b
fillComp = toLinear2 _fillComp

fillLeaf :: forall r a. (RegionContext r) => Dest r a %1 -> a -> ()
fillLeaf = toLinear2 _fillLeaf

isNullPtr :: Ptr a -> Bool
isNullPtr (Ptr addr#) = isTrue# (addr2Int# addr# ==# 0#)

nullPtr :: Ptr a
nullPtr = Ptr (int2Addr# 0#)

{-# NOINLINE _fillComp #-}
_fillComp :: forall r a b. (RegionContext r) => Dest r a -> Incomplete r a b -> b
_fillComp Dest {parentWriteLoc = bParentWriteLoc} Incomplete {rootReceiver = sRootReceiver, dests = sDests, pInitialParentWriteLoc} =
  unsafePerformIO $ do
    let pSRootReceiver = aToRawPtr sRootReceiver
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
    return $ sDests

{-# NOINLINE _fillLeaf #-}
_fillLeaf :: forall r a. (RegionContext r) => Dest r a -> a -> ()
_fillLeaf Dest {parentWriteLoc} x =
  unsafePerformIO $ do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) x)
    let pXAsWord = aToWord xInRegion
    poke parentWriteLoc pXAsWord
    putDebugLn $
      "fillLeaf: @"
        ++ show (ptrToWord parentWriteLoc)
        ++ " <- #"
        ++ show pXAsWord
        ++ ": [value]"

complete :: forall r a. Incomplete r a () %1 -> Ur a
complete = toLinear _complete

completeExtract :: forall r a b. Incomplete r a (Ur b) %1 -> Ur (a, b)
completeExtract = toLinear _completeExtract

{-# NOINLINE _complete #-}
_complete :: forall r a. Incomplete r a () -> Ur a
_complete (Incomplete u d _) = case d of () -> _hide u

-- TODO: should we put the new Ur wrapper inside the compact region?
{-# NOINLINE _completeExtract #-}
_completeExtract :: forall r a b. Incomplete r a (Ur b) -> Ur (a, b)
_completeExtract (Incomplete u d _) = case d of
  Ur y -> case _hide u of Ur x -> Ur (x, y)

type family GDestsOf (ctor :: (Meta, [(Meta, Type)])) r (a :: Type) where
  -- GDestsOf '( ('MetaCons "leaf" _ _), '[]) a r = a %1 -> ()
  -- GDestsOf '( ('MetaCons "comp" _ _), '[]) a r = forall b. Incomplete r a b %1 -> b
  GDestsOf '(_, '[]) _ _ = ()
  GDestsOf '(_, '[ '(_, t)]) r _ = Dest r t
  GDestsOf '(_, '[ '(_, t0), '(_, t1)]) r _ = (Dest r t0, Dest r t1)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2)]) r _ = (Dest r t0, Dest r t1, Dest r t2)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2), '(_, t3)]) r _ = (Dest r t0, Dest r t1, Dest r t2, Dest r t3)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2), '(_, t3), '(_, t4)]) r _ = (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2), '(_, t3), '(_, t4), '(_, t5)]) r _ = (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4, Dest r t5)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2), '(_, t3), '(_, t4), '(_, t5), '(_, t6)]) r _ = (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4, Dest r t5, Dest r t6)
  GDestsOf _ _ _ = TypeError ('Text "GDestsOf not implemented for constructors with more than 7 fields")

type family DestsOf (symCtor :: Symbol) r (a :: Type) where
  DestsOf symCtor r a = GDestsOf (FromJust (GCtorInfoOf symCtor (Rep a ()))) r a

class Fill (symCtor :: Symbol) r (a :: Type) where
  fill :: Dest r a %1 -> DestsOf symCtor r a

instance ('Just ctor ~ GCtorInfoOf symCtor (Rep a ()), GDestsOf ctor r a ~ DestsOf symCtor r a, GFill ctor a, RegionContext r) => Fill symCtor r a where
  fill = toLinear (\d -> unsafePerformIO (gFill @ctor @a d))

class GFill (ctor :: (Meta, [(Meta, Type)])) (a :: Type) where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO (GDestsOf ctor r a)

showFill :: Ptr Word -> Word -> String -> [Ptr Word] -> String
showFill parentWriteLoc pXAsWord ctorName slots =
  "fill"
    ++ (show n)
    ++ ": @"
    ++ show (ptrToWord parentWriteLoc)
    ++ " <- #"
    ++ show pXAsWord
    ++ ": "
    ++ ctorName
    ++ " "
    ++ showSlots slots
  where
    n = length slots
    showSlots = intercalate " " . fmap showSlot
    showSlot ptr = "_@" ++ (show $ ptrToWord ptr)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => GFill '(metaCtor, '[]) a where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO ()
  gFill Dest {parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) (shallowTerm @symCtor @a))
    let CtorData {..} = getCtorData @metaCtor
        pXAsWord = aToWord xInRegion
    poke parentWriteLoc pXAsWord
    putDebugLn $ showFill parentWriteLoc pXAsWord ctorName []

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => GFill '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0)]) a where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO (Dest r t0)
  gFill Dest {parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) (shallowTerm @symCtor @a))
    let pXRaw = aToRawPtr xInRegion
        pXAsWord = aToWord xInRegion
        !pF0 = pXRaw `plusPtr` headerSize
        dF0 = Dest {parentWriteLoc = pF0}
        CtorData {..} = getCtorData @metaCtor
    poke parentWriteLoc pXAsWord
    putDebugLn $ showFill parentWriteLoc pXAsWord ctorName [pF0]
    return dF0

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => GFill '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1)]) a where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO (Dest r t0, Dest r t1)
  gFill Dest {parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) (shallowTerm @symCtor @a))
    let pXRaw = aToRawPtr xInRegion
        pXAsWord = aToWord xInRegion
        !pF0 = pXRaw `plusPtr` headerSize
        !pF1 = pF0 `plusPtr` (1 * wordSize)
        CtorData {..} = getCtorData @metaCtor
    poke parentWriteLoc pXAsWord
    putDebugLn $ showFill parentWriteLoc pXAsWord ctorName [pF0, pF1]
    return (Dest pF0, Dest pF1)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => GFill '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2)]) a where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO (Dest r t0, Dest r t1, Dest r t2)
  gFill Dest {parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) (shallowTerm @symCtor @a))
    let pXRaw = aToRawPtr xInRegion
        pXAsWord = aToWord xInRegion
        !pF0 = pXRaw `plusPtr` headerSize
        !pF1 = pF0 `plusPtr` (1 * wordSize)
        !pF2 = pF0 `plusPtr` (2 * wordSize)
        CtorData {..} = getCtorData @metaCtor
    poke parentWriteLoc pXAsWord
    putDebugLn $ showFill parentWriteLoc pXAsWord ctorName [pF0, pF1, pF2]
    return (Dest pF0, Dest pF1, Dest pF2)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => GFill '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3)]) a where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO (Dest r t0, Dest r t1, Dest r t2, Dest r t3)
  gFill Dest {parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) (shallowTerm @symCtor @a))
    let pXRaw = aToRawPtr xInRegion
        pXAsWord = aToWord xInRegion
        !pF0 = pXRaw `plusPtr` headerSize
        !pF1 = pF0 `plusPtr` (1 * wordSize)
        !pF2 = pF0 `plusPtr` (2 * wordSize)
        !pF3 = pF0 `plusPtr` (3 * wordSize)
        CtorData {..} = getCtorData @metaCtor
    poke parentWriteLoc pXAsWord
    putDebugLn $ showFill parentWriteLoc pXAsWord ctorName [pF0, pF1, pF2, pF3]
    return (Dest pF0, Dest pF1, Dest pF2, Dest pF3)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => GFill '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4)]) a where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4)
  gFill Dest {parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) (shallowTerm @symCtor @a))
    let pXRaw = aToRawPtr xInRegion
        pXAsWord = aToWord xInRegion
        !pF0 = pXRaw `plusPtr` headerSize
        !pF1 = pF0 `plusPtr` (1 * wordSize)
        !pF2 = pF0 `plusPtr` (2 * wordSize)
        !pF3 = pF0 `plusPtr` (3 * wordSize)
        !pF4 = pF0 `plusPtr` (4 * wordSize)
        CtorData {..} = getCtorData @metaCtor
    poke parentWriteLoc pXAsWord
    putDebugLn $ showFill parentWriteLoc pXAsWord ctorName [pF0, pF1, pF2, pF3, pF4]
    return (Dest pF0, Dest pF1, Dest pF2, Dest pF3, Dest pF4)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => GFill '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5)]) a where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4, Dest r t5)
  gFill Dest {parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) (shallowTerm @symCtor @a))
    let pXRaw = aToRawPtr xInRegion
        pXAsWord = aToWord xInRegion
        !pF0 = pXRaw `plusPtr` headerSize
        !pF1 = pF0 `plusPtr` (1 * wordSize)
        !pF2 = pF0 `plusPtr` (2 * wordSize)
        !pF3 = pF0 `plusPtr` (3 * wordSize)
        !pF4 = pF0 `plusPtr` (4 * wordSize)
        !pF5 = pF0 `plusPtr` (5 * wordSize)
        CtorData {..} = getCtorData @metaCtor
    poke parentWriteLoc pXAsWord
    putDebugLn $ showFill parentWriteLoc pXAsWord ctorName [pF0, pF1, pF2, pF3, pF4, pF5]
    return (Dest pF0, Dest pF1, Dest pF2, Dest pF3, Dest pF4, Dest pF5)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, GShallow symCtor repA) => GFill '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6)]) a where
  gFill :: forall r. (RegionContext r) => Dest r a -> IO (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4, Dest r t5, Dest r t6)
  gFill Dest {parentWriteLoc} = do
    !xInRegion <- getCompact <$> (compactAdd (getRegionRoot @r) (shallowTerm @symCtor @a))
    let pXRaw = aToRawPtr xInRegion
        pXAsWord = aToWord xInRegion
        !pF0 = pXRaw `plusPtr` headerSize
        !pF1 = pF0 `plusPtr` (1 * wordSize)
        !pF2 = pF0 `plusPtr` (2 * wordSize)
        !pF3 = pF0 `plusPtr` (3 * wordSize)
        !pF4 = pF0 `plusPtr` (4 * wordSize)
        !pF5 = pF0 `plusPtr` (5 * wordSize)
        !pF6 = pF0 `plusPtr` (6 * wordSize)
        CtorData {..} = getCtorData @metaCtor
    poke parentWriteLoc pXAsWord
    putDebugLn $ showFill parentWriteLoc pXAsWord ctorName [pF0, pF1, pF2, pF3, pF4, pF5, pF6]
    return (Dest pF0, Dest pF1, Dest pF2, Dest pF3, Dest pF4, Dest pF5, Dest pF6)

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

type family FromJust (x :: Maybe k) :: k where
  FromJust ('Just v) = v
  FromJust 'Nothing = TypeError ('Text "FromJust error: got 'Nothing")

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

data Incomplete r a b = Incomplete {rootReceiver :: Ur a, dests :: b, pInitialParentWriteLoc :: Ptr Word} deriving (Data.Functor) via Data (Incomplete r a)

instance Control.Functor (Incomplete r a) where
  fmap f (Incomplete u d pp) = Incomplete u (f d) pp

-- TODO: should we add the redundant '(RegionContext r) =>' here?
intoR :: forall r a. RegionToken r %1 -> a -> Incomplete r a ()
intoR = toLinear2 _intoR

{-# NOINLINE _intoR #-}
_intoR :: forall r a. RegionToken r -> a -> Incomplete r a ()
_intoR (RegionToken (Region c)) x =
  unsafePerformIO $ do
    !rootReceiver <- getCompact <$> (compactAdd c $ Ur x)
    putDebugLn $
      "intoR: [region] <- #"
        ++ (show $ aToWord rootReceiver)
        ++ ": Ur [value]"
    return $ Incomplete rootReceiver () nullPtr

{-# NOINLINE _hide #-}
_hide :: a -> a
_hide x = x

-- TODO: should we add the redundant '(RegionContext r) =>' here?
alloc :: forall r a. RegionToken r %1 -> Incomplete r a (Dest r a)
alloc = toLinear _alloc

{-# NOINLINE _alloc #-}
_alloc :: forall r a. RegionToken r -> Incomplete r a (Dest r a)
_alloc (RegionToken (Region c)) =
  unsafePerformIO $ do
    !rootReceiver <- getCompact <$> (compactAdd c $ (unsafeCoerce (Ur placeholder) :: Ur a))
    let p = aToRawPtr rootReceiver
        parentWriteLoc = p `plusPtr` headerSize
    !pwlHolder <- getCompact <$> (compactAdd c $ parentWriteLoc)
    let !initialDest = Dest {parentWriteLoc = pwlHolder}
        pHolder = aToRawPtr pwlHolder
        !pParentWriteLoc = pHolder `plusPtr` headerSize
    putDebugLn $
      "alloc: [region] <- #"
        ++ (show $ aToWord rootReceiver)
        ++ ": Ur _@"
        ++ (show $ ptrToWord parentWriteLoc)
        ++ " (slot address stored at "
        ++ (show $ ptrToWord pParentWriteLoc)
        ++ ")"
    return $ Incomplete rootReceiver initialDest pParentWriteLoc
