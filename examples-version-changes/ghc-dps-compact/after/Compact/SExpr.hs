{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Compact.SExpr where

import Compact.Destination.Internal
import Control.DeepSeq (NFData)
import Control.Functor.Linear ((<&>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isSpace)
import Data.Proxy (Proxy)
import GHC.Generics (Generic)
import Prelude.Linear
import qualified Prelude as NonLinear

data SExpr
  = SList Int [SExpr]
  | SInteger Int Int
  | SString Int String
  | SSymbol Int String
  deriving (NonLinear.Eq, Generic, NFData)

{-# INLINE endPos #-}
endPos :: SExpr -> Int
endPos = \case
  SList n _ -> n
  SInteger n _ -> n
  SString n _ -> n
  SSymbol n _ -> n

showSExpr :: Bool -> Int -> SExpr -> String
showSExpr cont indent = \case
  SList _ [] -> makeIndent cont indent ++ "()"
  SList _ (x : xs) ->
    makeIndent cont indent
      ++ "("
      ++ showSExpr True (indent + 1) x
      ++ NonLinear.concatMap (\x' -> "\n" ++ showSExpr False (indent + 1) x') xs
      ++ ")"
  SInteger _ i -> makeIndent cont indent ++ show i
  SString _ s -> makeIndent cont indent ++ show s
  SSymbol _ s -> makeIndent cont indent ++ s
  where
    makeIndent isCont n = if isCont then "" else replicate n ' '

instance Show SExpr where
  show x = showSExpr False 0 x

data SExprParseError
  = UnexpectedClosingParen Int
  | UnexpectedEOFSExpr Int
  | UnexpectedEOFSList Int
  | UnexpectedEOFSString Int
  | UnexpectedContentAfter Int
  deriving (NonLinear.Eq, Generic, NFData)

errEndPos :: SExprParseError -> Int
errEndPos = \case
  UnexpectedClosingParen n -> n
  UnexpectedEOFSExpr n -> n
  UnexpectedEOFSList n -> n
  UnexpectedEOFSString n -> n
  UnexpectedContentAfter n -> n

instance Consumable SExprParseError where
  consume = \case
    UnexpectedClosingParen n -> consume n
    UnexpectedEOFSExpr n -> consume n
    UnexpectedEOFSList n -> consume n
    UnexpectedEOFSString n -> consume n
    UnexpectedContentAfter n -> consume n

instance Dupable SExprParseError where
  dup2 = \case
    UnexpectedClosingParen n -> let !(n1, n2) = dup2 n in (UnexpectedClosingParen n1, UnexpectedClosingParen n2)
    UnexpectedEOFSExpr n -> let !(n1, n2) = dup2 n in (UnexpectedEOFSExpr n1, UnexpectedEOFSExpr n2)
    UnexpectedEOFSList n -> let !(n1, n2) = dup2 n in (UnexpectedEOFSList n1, UnexpectedEOFSList n2)
    UnexpectedEOFSString n -> let !(n1, n2) = dup2 n in (UnexpectedEOFSString n1, UnexpectedEOFSString n2)
    UnexpectedContentAfter n -> let !(n1, n2) = dup2 n in (UnexpectedContentAfter n1, UnexpectedContentAfter n2)

instance Movable SExprParseError where
  move = \case
    UnexpectedClosingParen n -> let !(Ur n') = move n in Ur (UnexpectedClosingParen n')
    UnexpectedEOFSExpr n -> let !(Ur n') = move n in Ur (UnexpectedEOFSExpr n')
    UnexpectedEOFSList n -> let !(Ur n') = move n in Ur (UnexpectedEOFSList n')
    UnexpectedEOFSString n -> let !(Ur n') = move n in Ur (UnexpectedEOFSString n')
    UnexpectedContentAfter n -> let !(Ur n') = move n in Ur (UnexpectedContentAfter n')

instance Show SExprParseError where
  show = \case
    UnexpectedClosingParen n -> "Parse error: Encountered an unexpected closing parentheses at position " ++ show n
    UnexpectedEOFSExpr n -> "Parse error: Ecountered EOF while expecting an SExpr at position " ++ show n
    UnexpectedEOFSList n -> "Parse error: Encountered EOF in the middle of parsing an SList at position " ++ show n
    UnexpectedEOFSString n -> "Parse error: Encountered EOF in the middle of parsing a quoted string at position " ++ show n
    UnexpectedContentAfter n -> "Parse error: Encountered unexpected content after the SExpr at position " ++ show n

defaultSExpr :: SExpr
defaultSExpr = SInteger (-1) 0

extractNextToken :: ByteString -> Int -> ByteString
extractNextToken bs i = fst $ BSC.span (\c -> not (isSpace c) && c /= '(' && c /= ')' && c /= '"') (snd $ BSC.splitAt i bs)

parseStringWithoutDest' :: ByteString -> Int -> Bool -> [Char] -> Either SExprParseError SExpr
parseStringWithoutDest' bs i escape acc = case bs BSC.!? i of
  Nothing -> Left $ UnexpectedEOFSString i
  Just c -> case c of
    '"' | not escape -> Right $ SString i (reverse acc)
    '\\' | not escape -> parseStringWithoutDest' bs (i + 1) True acc
    'n' | escape -> parseStringWithoutDest' bs (i + 1) False ('\n' : acc)
    _ -> parseStringWithoutDest' bs (i + 1) False (c : acc)

parseStringWithDest' :: (Region r) => ByteString -> Int -> Bool -> Dest r Int %1 -> Dest r [Char] %1 -> Either SExprParseError Int
parseStringWithDest' bs i escape dEndPos d = case bs BSC.!? i of
  Nothing -> dEndPos & fillLeaf (-1) `lseq` d & fill @'[] `lseq` Left $ UnexpectedEOFSString i
  Just c -> case c of
    '"' | not escape -> dEndPos & fillLeaf i `lseq` d & fill @'[] `lseq` Right i
    '\\' | not escape -> parseStringWithDest' bs (i + 1) True dEndPos d
    'n' | escape -> let !(dh, dt) = d & fill @'(:) in dh & fillLeaf '\n' `lseq` parseStringWithDest' bs (i + 1) False dEndPos dt
    _ -> let !(dh, dt) = d & fill @'(:) in dh & fillLeaf c `lseq` parseStringWithDest' bs (i + 1) False dEndPos dt

parseListWithoutDest' :: ByteString -> Int -> [SExpr] -> Either SExprParseError SExpr
parseListWithoutDest' bs i acc = case bs BSC.!? i of
  Nothing -> Left $ UnexpectedEOFSList i
  Just c ->
    if
      | c == ')' -> Right $ SList i (reverse acc)
      -- we need this case for final spaces before the closing paren
      -- parseWithoutDest' know how to handle leading spaces, but will expect a token after, whereas this case allows for trailing spaces
      | isSpace c -> parseListWithoutDest' bs (i + 1) acc
      | otherwise -> case parseWithoutDest' bs i of
          Left err -> Left err
          Right children -> parseListWithoutDest' bs (endPos children + 1) (children : acc)

parseListWithDest' :: (Region r) => ByteString -> Int -> Dest r Int %1 -> Dest r [SExpr] %1 -> Either SExprParseError Int
parseListWithDest' bs i dEndPos d = case bs BSC.!? i of
  Nothing -> dEndPos & fillLeaf (-1) `lseq` d & fill @'[] `lseq` Left $ UnexpectedEOFSList i
  Just c ->
    if
      | c == ')' -> dEndPos & fillLeaf i `lseq` d & fill @'[] `lseq` Right i
      -- we need this case for final spaces before the closing paren
      -- parseWithoutDest' know how to handle leading spaces, but will expect a token after, whereas this case allows for trailing spaces
      | isSpace c -> parseListWithDest' bs (i + 1) dEndPos d
      | otherwise ->
          let !(dh, dt) = d & fill @'(:)
           in case parseWithDest' bs i dh of
                Left err -> dEndPos & fillLeaf (-1) `lseq` dt & fill @'[] `lseq` Left err
                Right childrenEndPos -> parseListWithDest' bs (childrenEndPos + 1) dEndPos dt

parseWithoutDest' :: ByteString -> Int -> Either SExprParseError SExpr
parseWithoutDest' bs i = case bs BSC.!? i of
  Nothing -> Left $ UnexpectedEOFSExpr i
  Just c -> case c of
    ')' -> Left $ UnexpectedClosingParen i
    '(' -> parseListWithoutDest' bs (i + 1) []
    '"' -> parseStringWithoutDest' bs (i + 1) False []
    _ ->
      let token = extractNextToken bs i
       in if BSC.null token
            then -- c is a (leading) space because we matched against the other cases before
              parseWithoutDest' bs (i + 1)
            else case BSC.readInt token of
              Just (int, remaining) | BSC.null remaining -> Right $ SInteger (i + BSC.length token - 1) int
              _ -> Right $ SSymbol (i + BSC.length token - 1) (BSC.unpack token)

parseWithDest' :: (Region r) => ByteString -> Int -> Dest r SExpr %1 -> Either SExprParseError Int
parseWithDest' bs i d = case bs BSC.!? i of
  Nothing -> d & fillLeaf defaultSExpr `lseq` Left $ UnexpectedEOFSExpr i
  Just c -> case c of
    ')' -> d & fillLeaf defaultSExpr `lseq` Left $ UnexpectedClosingParen i
    '(' -> let !(dEndPos, dList) = d & fill @'SList in parseListWithDest' bs (i + 1) dEndPos dList
    '"' -> let !(dEndPos, dStr) = d & fill @'SString in parseStringWithDest' bs (i + 1) False dEndPos dStr
    _ ->
      let token = extractNextToken bs i
       in if BSC.null token
            then -- c is a (leading) space because we matched against the other cases before
              parseWithDest' bs (i + 1) d
            else case BSC.readInt token of
              Just (int, remaining)
                | BSC.null remaining ->
                    let !(dEndPos, dInt) = d & fill @'SInteger
                        endPos = i + BSC.length token - 1
                     in dEndPos & fillLeaf endPos `lseq` dInt & fillLeaf int `lseq` Right endPos
              _ ->
                let !(dEndPos, dSym) = d & fill @'SSymbol
                    endPos = i + BSC.length token - 1
                 in dEndPos & fillLeaf endPos `lseq` dSym & fillLeaf (BSC.unpack token) `lseq` Right endPos

parseWithoutDest :: ByteString -> Either SExprParseError SExpr
parseWithoutDest bs = case parseWithoutDest' bs 0 of
  Left err -> Left err
  Right sexpr ->
    let i = endPos sexpr
     in if
          | i >= BSC.length bs - 1 -> Right sexpr
          | rem <- snd (BSC.splitAt (i + 1) bs), BSC.all isSpace rem -> Right sexpr
          | otherwise -> Left $ UnexpectedContentAfter (i + 1)

parseWithDest :: ByteString -> Either SExprParseError SExpr
parseWithDest bs =
  let Ur (sexpr, res) =
        withRegion $ \(_ :: Proxy r) token ->
          fromIncomplete $
            alloc @r token
              <&> \d ->
                move $ parseWithDest' bs 0 d
   in case res of
        Left err -> Left err
        Right i ->
          if
            | i >= BSC.length bs - 1 -> Right sexpr
            | rem <- snd (BSC.splitAt (i + 1) bs), BSC.all isSpace rem -> Right sexpr
            | otherwise -> Left $ UnexpectedContentAfter (i + 1)

impls :: [(ByteString -> Either SExprParseError SExpr, String, Bool)]
impls =
  [ (parseWithoutDest, "parseWithoutDest", True),
    (parseWithDest, "parseWithDest", False)
  ]
