module Main where

-- import qualified Lab3.PythonLexer as Lex3
-- import qualified Lab3.PythonParser as Par3
-- import           Data.List (intercalate, nub)
import qualified Calc.MyLexer as Lex1
import qualified Calc.MyParser as Par1
-- import qualified Lab2.CLexer as Lex2
-- import qualified Lab2.CParser as Par2
import Parser
import Lexer
import Control.Monad (mapM_)

generate :: String -> String -> String -> IO ()
generate fileInp fileLex filePar = do
    grammar <- readFile fileInp
    let tokens = scanTokens grammar
    let (lexer, parser) = parseGrammar tokens
    writeFile fileLex $ show lexer
    writeFile filePar $ show parser

check1 :: String -> Double
check1 input = do
    let tokens = Lex1.tokenize input
    let result = Par1.parse tokens
    result

-- check2 :: String -> Par2.Tree
-- check2 input = do
--     let tokens = Lex2.tokenize input
--     let tree = Par2.parse tokens
--     tree

main :: IO ()
main = do
    -- generate "calc/calculator.gram" "calc/MyLexer.hs" "calc/MyParser.hs"
    -- generate "lab2/lab2.gram" "lab2/CLexer.hs" "lab2/CParser.hs"
    -- generate "lab3/lab3.gram" "lab3/PythonLexer.hs" "lab3/PythonParser.hs"
    input <- getContents
    mapM_ (print . check1) (lines input)

-- check3 :: String -> IO ()
-- check3 file = do
--     input <- readFile file
--     pythonToC input

-- getVariables :: [PythonToken] -> String
-- getVariables tokens = intercalate ", " $ nub [name | (VAR name) <- tokens]

-- pythonToC :: String -> String
-- pythonToC input = case Lex3.scanTokens input of
--     tokens -> show tokens ++ "\n\n" ++ begin ++ code
--         where
--             variables = getVariables tokens
--             begin = if variables == "" then ""
--                     else "int " ++ variables ++ ";\n\n"
--             lines = Lex3.parse tokens
--             code = "int main() {\n"
--                 ++ intercalate "\n" (map show lines) ++ "\n\treturn 0;\n}"
