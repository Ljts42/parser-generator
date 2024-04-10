module Lab3.PythonLexer
	( tokenize, PythonToken (..) ) where

import Text.Regex.PCRE ( (=~) )
import Control.Monad.State


data PythonToken = INDENT | DEDENT
                 | INPUT | PRINT | RANGE | WHILE | FOR | INT | IF | IN
                 | TRUE | FALSE | NOT | AND | OR | EQUAL | NEQ | LEQ | LESS | GEQ | GREATER
                 | ASSIGN | COLON | COMMA | LEFTP | RIGHTP
                 | MUL | DIV | MOD | ADD | SUB
                 | NUMBER String | VAR String
                 | SKIP | EPS | END
                    deriving (Show, Eq)


type Stored = Int
type Returned = [PythonToken]
type SimpleReturn = State Stored Returned
type ListReturn = State Stored [Returned]

tokenize :: String -> [Returned]
tokenize input = evalState (match regList input) (0)

match :: [(String, String -> SimpleReturn)] -> String -> ListReturn
match [] "" = return []
match [] input = error $ "string prefix does not match any token: \"" ++ input ++ "\""
match ((regex, getToken) : other) input =
	case input =~ regex :: (String, String, String) of
		("", current, rest) | current /= "" -> do
			token <- getToken current
			right <- match regList rest
			return ([token | token /= ([SKIP])] ++ right)
		(_, _, _) -> match other input

regList :: [(String, String -> SimpleReturn)]
regList = [
	("\\n([\\ ]{4}|\\t)*", \s -> do
                            previous <- get
                            let current = if (take 2 s) == "\n\t" 
                                then (length s) - 1
                                else ((length s) - 1) `div` 4
                            if (current > previous)
                                then do
                                    put current
                                    return $ replicate (current - previous) INDENT
                                else if (current < previous)
                                    then do
                                        put current
                                        return $ replicate (previous - current) DEDENT
                                    else return []),
	("\\s", \_ -> return ([SKIP])),
	("input", \_ -> return [INPUT]),
	("print", \_ -> return [PRINT]),
	("range", \_ -> return [RANGE]),
	("while", \_ -> return [WHILE]),
	("for", \_ -> return [FOR]),
	("int", \_ -> return [INT]),
	("if", \_ -> return [IF]),
	("in", \_ -> return [IN]),
	("True", \_ -> return [TRUE]),
	("False", \_ -> return [FALSE]),
	("not", \_ -> return [NOT]),
	("and", \_ -> return [AND]),
	("or", \_ -> return [OR]),
	("==", \_ -> return [EQUAL]),
	("!=", \_ -> return [NEQ]),
	("<=", \_ -> return [LEQ]),
	("<", \_ -> return [LESS]),
	(">=", \_ -> return [GEQ]),
	(">", \_ -> return [GREATER]),
	("=", \_ -> return [ASSIGN]),
	(":", \_ -> return [COLON]),
	(",", \_ -> return [COMMA]),
	("\\(", \_ -> return [LEFTP]),
	("\\)", \_ -> return [RIGHTP]),
	("\\*", \_ -> return [MUL]),
	("//", \_ -> return [DIV]),
	("%", \_ -> return [MOD]),
	("\\+", \_ -> return [ADD]),
	("-", \_ -> return [SUB]),
	("[0-9]+", \s -> return [NUMBER s]),
	("[a-zA-Z_][a-zA-Z0-9_]*", \s -> return [VAR s])]
