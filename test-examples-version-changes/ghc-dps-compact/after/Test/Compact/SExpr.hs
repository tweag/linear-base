module Test.Compact.SExpr where

import Compact.SExpr
import Data.ByteString.Char8 (ByteString, pack)
import Test.Compact.Utils
import Test.Tasty (TestTree)

dataset :: ByteString
dataset = pack "(ert-deftest company-shows-keywords-alongside-completions-alphabetically () :tags '(company) (switch-to-buffer \"*TESTING COMPANY MODE ~ Python*\"))"

expected :: Either SExprParseError SExpr
expected =
  Right (SList 145 [SSymbol 11 "ert-deftest", SSymbol 71 "company-shows-keywords-alongside-completions-alphabetically", SList 74 [], SSymbol 80 ":tags", SSymbol 82 "'", SList 91 [SSymbol 90 "company"], SList 144 [SSymbol 109 "switch-to-buffer", SString 143 "*TESTING COMPANY MODE ~ Python*"]])

sexprTests :: TestTree
sexprTests = safetySameAsExpected "S-expression parser" impls dataset expected
