{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Compact.SExpr where

import Compact.Pure.Internal
import Control.DeepSeq (NFData)
import Control.Functor.Linear ((<&>))
import Data.Char (isSpace)
import GHC.Generics (Generic)
import Prelude.Linear
import Text.Read (readMaybe)
import qualified Prelude as NonLinear

loadSampleData :: IO String
loadSampleData = readFile "memory/Compact/test_data.sexpr"

data SExpr
  = SList [SExpr]
  | SFloat Float
  | SInteger Int
  | SString String
  | SSymbol String
  deriving (NonLinear.Eq, Generic, NFData)

showSExpr :: Bool -> Int -> SExpr %1 -> String
showSExpr cont indent = \case
  SList [] -> makeIndent cont indent ++ "()"
  SList (x : xs) ->
    makeIndent cont indent
      ++ "("
      ++ showSExpr True (indent + 1) x
      ++ concatMap (\x' -> "\n" ++ showSExpr False (indent + 1) x') xs
      ++ ")"
  SFloat f -> makeIndent cont indent ++ show f
  SInteger i -> makeIndent cont indent ++ show i
  SString s -> makeIndent cont indent ++ show s
  SSymbol s -> makeIndent cont indent ++ s
  where
    makeIndent isCont n = if isCont then "" else replicate n ' '

instance Show SExpr where
  show x = showSExpr False 0 x

data SContext
  = NotInSList
  | InSList [SExpr]
  deriving (Generic, NFData)

data DSContext r
  = DNotInSList (Dest SExpr r)
  | DInSList (Dest [SExpr] r)

data SExprParseError
  = UnexpectedClosingParen String
  | UnexpectedEOFSExpr
  | UnexpectedEOFSList (Maybe [SExpr])
  | UnexpectedEOFSString Bool (Maybe String)
  | UnexpectedContentAfter SExpr (Maybe String)
  deriving (Generic, NFData)

instance Show SExprParseError where
  show = \case
    UnexpectedClosingParen remaining ->
      "Parse error: Encountered an unexpected closing parentheses in:\n"
        ++ remaining
    UnexpectedEOFSExpr -> "Parse error: Ecountered EOF while expecting an SExpr."
    UnexpectedEOFSList mContext ->
      "Parse error: Encountered EOF in the middle of parsing an SList.\n"
        ++ ifAvailableShow mContext "\n"
    UnexpectedEOFSString escaped mContext ->
      "Parse error: Encountered EOF in the middle of parsing a quoted string\n"
        ++ "Escape mode for next character: "
        ++ (if escaped then "on" else "off")
        ++ ifAvailableShow mContext "\nThese chars have been parsed so far: "
    UnexpectedContentAfter parsedExpr mContext ->
      "Parse error: This SExpr has been successfully parsed:\n"
        ++ show parsedExpr
        ++ "\nBut some unexpected content is present after"
        ++ ifAvailableShow mContext ":\n"
    where
      ifAvailableShow :: (Show a) => Maybe a -> String -> String
      ifAvailableShow mContext header = case mContext of
        Nothing -> ""
        Just c -> header ++ show c

readStringWithoutDest :: String -> Bool -> String -> Either SExprParseError (SExpr, String)
readStringWithoutDest = \cases
  acc escaped [] -> Left $ UnexpectedEOFSString escaped (Just acc)
  acc True ('n' : xs) -> readStringWithoutDest ('\n' : acc) False xs -- TODO: add other escape chars
  acc False ('\\' : xs) -> readStringWithoutDest acc True xs
  acc False ('"' : xs) -> Right $ (SString $ reverse acc, xs)
  acc _ (x : xs) -> readStringWithoutDest (x : acc) False xs

parseWithoutDest' :: SContext -> String -> Either SExprParseError (SExpr, String)
parseWithoutDest' = \cases
  NotInSList [] -> Left $ UnexpectedEOFSExpr
  (InSList exprs) [] -> Left $ UnexpectedEOFSList (Just exprs)
  ctx s@(x : xs) -> case x of
    '(' -> appendOrRet ctx $ parseWithoutDest' (InSList []) xs
    ')' -> case ctx of
      InSList exprs -> Right (SList $ reverse exprs, xs)
      NotInSList -> Left $ UnexpectedClosingParen s
    '"' -> appendOrRet ctx $ readStringWithoutDest [] False xs
    _ ->
      if isSpace x
        then parseWithoutDest' ctx xs
        else case NonLinear.break (\c -> isSpace c || c `NonLinear.elem` ['(', ')', '"']) s of
          (raw, remaining) -> case readMaybe @Int raw of
            Just int -> appendOrRet ctx $ Right (SInteger int, remaining)
            Nothing -> case readMaybe @Float raw of
              Just float -> appendOrRet ctx $ Right (SFloat float, remaining)
              Nothing -> appendOrRet ctx $ Right (SSymbol raw, remaining)
    where
      appendOrRet :: SContext -> Either SExprParseError (SExpr, String) -> Either SExprParseError (SExpr, String)
      appendOrRet = \cases
        (InSList exprs) (Right (expr, remaining)) -> parseWithoutDest' (InSList $ expr : exprs) remaining
        _ res -> res -- left is Nothing or right is error

parseWithoutDest :: String -> Either SExprParseError SExpr
parseWithoutDest s = case parseWithoutDest' NotInSList s of
  Right (expr, remaining) | NonLinear.all isSpace remaining -> Right expr
  Right (expr, remaining) -> Left $ UnexpectedContentAfter expr (Just remaining)
  Left err -> Left err

defaultSExpr :: SExpr
defaultSExpr = SInteger 0

readStringUsingDest :: Dest String r %1 -> Bool -> String -> Either (Ur SExprParseError) String
readStringUsingDest = \cases
  d escaped [] -> d <| C @"[]" `lseq` Left (Ur $ UnexpectedEOFSString escaped Nothing)
  d True ('n' : xs) -> case d <| C @":" of (dx, dxs) -> dx <|.. '\n' `lseq` readStringUsingDest dxs False xs -- TODO: add other escape chars
  d False ('\\' : xs) -> readStringUsingDest d True xs
  d False ('"' : xs) -> d <| C @"[]" `lseq` Right xs
  d _ (x : xs) -> case d <| C @":" of (dx, dxs) -> dx <|.. x `lseq` readStringUsingDest dxs False xs

parseUsingDest' :: DSContext r %1 -> String -> Either (Ur SExprParseError) String
parseUsingDest' = \cases
  (DNotInSList d) [] -> d <|.. defaultSExpr `lseq` Left $ Ur UnexpectedEOFSExpr
  (DInSList d) [] -> d <| C @"[]" `lseq` Left (Ur $ UnexpectedEOFSList Nothing)
  ctx s@(x : xs) -> case x of
    '(' -> appendOrRet ctx (\dExpr -> parseUsingDest' (DInSList $ dExpr <| C @"SList")) xs
    ')' -> case ctx of
      DInSList d -> d <| C @"[]" `lseq` Right xs
      DNotInSList d -> d <|.. defaultSExpr `lseq` Left (Ur $ UnexpectedClosingParen s)
    '"' -> appendOrRet ctx (\dExpr -> readStringUsingDest (dExpr <| C @"SString") False) xs
    _ ->
      if isSpace x
        then parseUsingDest' ctx xs
        else case NonLinear.break (\c -> isSpace c || c `NonLinear.elem` ['(', ')', '"']) s of
          (raw, remaining) -> case readMaybe @Int raw of
            Just int -> appendOrRet ctx (\dExpr -> dExpr <| C @"SInteger" <|.. int `lseq` Right) remaining
            Nothing -> case readMaybe @Float raw of
              Just float -> appendOrRet ctx (\dExpr -> dExpr <| C @"SFloat" <|.. float `lseq` Right) remaining
              Nothing -> appendOrRet ctx (\dExpr -> dExpr <| C @"SSymbol" <|.. raw `lseq` Right) remaining
    where
      appendOrRet :: DSContext r %1 -> (Dest SExpr r %1 -> String -> Either (Ur SExprParseError) String) %1 -> String -> Either (Ur SExprParseError) String
      appendOrRet context f str = case context of
        DNotInSList d -> f d str
        DInSList d ->
          case d <| C @":" of
            (dExpr, dRem) -> case f dExpr str of
              Right str' -> parseUsingDest' (DInSList dRem) str'
              Left err -> dRem <| C @"[]" `lseq` Left err

parseUsingDest :: String -> Either SExprParseError SExpr
parseUsingDest str =
  case withRegion $ \r ->
    case completeExtract $ alloc r <&> DNotInSList <&> flip parseUsingDest' str <&> finalizeResults of
      Ur (expr, Right ()) -> Ur (Right expr)
      Ur (expr, Left errFn) -> Ur (Left $ errFn expr) of
    Ur res -> res
  where
    finalizeResults :: Either (Ur SExprParseError) String %1 -> Ur (Either (SExpr -> SExprParseError) ())
    finalizeResults = \case
      Right s -> move s & \case Ur s' -> if NonLinear.all isSpace s' then Ur (Right ()) else Ur (Left $ \expr -> UnexpectedContentAfter expr (Just s'))
      Left (Ur err) -> Ur (Left $ const err)
