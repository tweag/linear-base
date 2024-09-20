module Test.Compact.SExpr where

import Data.ByteString.Char8 (ByteString, unpack)
import Compact.SExpr
import Test.Compact.Utils

dataset :: ByteString
dataset = unpack "(ert-deftest company-shows-keywords-alongside-completions-alphabetically () :tags '(company) (switch-to-buffer \"*TESTING COMPANY MODE ~ Python*\"))"

expected :: Either SExprParseError SExpr
expected =
  Right
    ( SList
        105
        [ SSymbol 1 "ert-deftest",
          SSymbol 17 "company-shows-keywords-alongside-completions-alphabetically",
          SList
            74
            [ SSymbol 1 "tags",
              SList
                7
                [ SSymbol 2 "quote",
                  SList
                    3
                    [ SSymbol 3 "company"
                    ]
                ],
              SList
                74
                [ SList
                    1
                    [ SSymbol 1 "switch-to-buffer" ],
                  SString 54 "*TESTING COMPANY MODE ~ Python*"
                ]
            ]
        ]
    )

sexprTests :: TestTree
sexprTests = safetySameAsExpected "S-expression parser" impls dataset expected
