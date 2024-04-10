module Lab2.CLexer
	( tokenize, CToken (..) ) where

import Text.Regex.PCRE ( (=~) )
import Control.Monad.State


data CToken = NAME String | POINTER | COMMA | SEMICOLON
            | SKIP | EPS | END
                deriving (Eq, Show)


type Stored = ()
type Returned = CToken
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
	("\\*", \_ -> return POINTER),
	("\\;", \_ -> return SEMICOLON),
	("\\,", \_ -> return COMMA),
	("[a-zA-Z_][a-zA-Z0-9_]*", \s -> return $ NAME s),
	("\\s+", \_ -> return (SKIP))]
