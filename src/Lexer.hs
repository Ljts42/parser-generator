module Lexer
    ( TokenLex (..)
    , scanTokens
    ) where

import Text.Regex.PCRE ( (=~) )

data TokenLex = CODEt String
              | MODULEt String
              | REGEXt String
              | TYPEt String
              | NAMEt String
              | COLONt
              | SEMICOLONt
              | VERTBARt
              | SKIP
              deriving (Show, Eq)

scanTokens :: String -> [TokenLex]
scanTokens = match regList

match :: [(String, String -> TokenLex)] -> String -> [TokenLex]
match [] "" = []
match [] input = error $ "string prefix does not match any token: \"" ++ input ++ "\""
match ((regex, getToken) : other) input =
    case input =~ regex :: (String, String, String) of
      ("", current, rest) | current /= "" ->
        let token = getToken current
        in [token | token /= SKIP] ++ match regList rest
      (_, _, _) -> match other input

regList :: [(String, String -> TokenLex)]
regList = [
  ("\\%[^\\%]+\\%",     MODULEt . init . tail),
  ("\\<[^\\<\\>]*\\>",  TYPEt . init . tail),
  ("\\{[^\\{\\}]*\\}",  CODEt . init . tail),
  ("\\\"[^\\\"]+\\\"",  REGEXt . init . tail),
  ("[A-Z][a-zA-Z0-9]*", NAMEt),
  ("\\:",               const COLONt),
  ("\\;",               const SEMICOLONt),
  ("\\|",               const VERTBARt),
  ("\\s+",              const SKIP)]
