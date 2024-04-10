module Calc.MyLexer
	( tokenize, CalcToken (..) ) where

import Text.Regex.PCRE ( (=~) )
import Control.Monad.State


data CalcToken = NUMBER String | LP | RP
               | MUL | DIV | SUB | ADD
               | SKIP | EPS | END | LOG
                  deriving (Eq, Show)


type Stored = ()
type Returned = CalcToken
type SimpleReturn = State Stored Returned
type ListReturn = State Stored [Returned]

tokenize :: String -> [Returned]
tokenize input = evalState (match regList input) (())

match :: [(String, String -> SimpleReturn)] -> String -> ListReturn
match [] "" = return []
match [] input = error $ "string prefix does not match any token: \"" ++ input ++ "\""
match ((regex, getToken) : other) input =
	case input =~ regex :: (String, String, String) of
		("", current, rest) | current /= "" -> do
			token <- getToken current
			right <- match regList rest
			return ([token | token /= (SKIP)] ++ right)
		(_, _, _) -> match other input

regList :: [(String, String -> SimpleReturn)]
regList = [
	("lg", \_ -> return LOG),
	("\\*", \_ -> return MUL),
	("\\/", \_ -> return DIV),
	("\\+", \_ -> return ADD),
	("\\-", \_ -> return SUB),
	("\\(", \_ -> return LP),
	("\\)", \_ -> return RP),
	("[0-9]+\\.?[0-9]*", \s -> return $ NUMBER s),
	("\\s+", \_ -> return (SKIP))]
