module Calc.MyParser
	( parse ) where

import Control.Monad.State

import Control.Monad (when)


import Calc.MyLexer (CalcToken (..))


type Parser a = State [CalcToken] a

token :: Parser CalcToken
token = do
	ts <- get
	case ts of
		(t:ts') -> do
			put ts'
			return t
		[] -> return END

peek :: Parser CalcToken
peek = do
	ts <- get
	case ts of
		(t:_) -> return t
		[] -> return END

parse :: [CalcToken] -> Double
parse = evalState rS

rS :: Parser Double
rS = do
	t <- peek
	case t of
		LP -> do
			w1 <- rE (0 +)
			return w1
		(NUMBER _) -> do
			w1 <- rE (0 +)
			return w1
		SUB -> do
			w1 <- rE (0 +)
			return w1
		_ -> error "unexpected token"

rE :: (Double -> Double) -> Parser Double
rE f = do
	t <- peek
	case t of
		LP -> do
			w1 <- rP
			w2 <- rEP (f w1)
			return w2
		(NUMBER _) -> do
			w1 <- rP
			w2 <- rEP (f w1)
			return w2
		SUB -> do
			w1 <- rP
			w2 <- rEP (f w1)
			return w2
		_ -> error "unexpected token"

rEP :: Double -> Parser Double
rEP a = do
	t <- peek
	case t of
		ADD -> do
			w1 <- token
			when(w1 /= ADD) $ error "unexpected token"
			w2 <- rE (a +)
			return w2
		SUB -> do
			w1 <- token
			when(w1 /= SUB) $ error "unexpected token"
			w2 <- rE (a -)
			return w2
		END -> do
			return a
		RP -> do
			return a
		_ -> error "unexpected token"

rP :: Parser Double
rP = do
	t <- peek
	case t of
		LP -> do
			w1 <- rT (1 *)
			return w1
		(NUMBER _) -> do
			w1 <- rT (1 *)
			return w1
		SUB -> do
			w1 <- rT (1 *)
			return w1
		_ -> error "unexpected token"

rT :: (Double -> Double) -> Parser Double
rT f = do
	t <- peek
	case t of
		LP -> do
			w1 <- rD
			w2 <- rTP (f w1)
			return w2
		(NUMBER _) -> do
			w1 <- rD
			w2 <- rTP (f w1)
			return w2
		SUB -> do
			w1 <- rD
			w2 <- rTP (f w1)
			return w2
		_ -> error "unexpected token"

rTP :: Double -> Parser Double
rTP a = do
	t <- peek
	case t of
		MUL -> do
			w1 <- token
			when(w1 /= MUL) $ error "unexpected token"
			w2 <- rT (a *)
			return w2
		DIV -> do
			w1 <- token
			when(w1 /= DIV) $ error "unexpected token"
			w2 <- rT (a /)
			return w2
		ADD -> do
			return a
		END -> do
			return a
		RP -> do
			return a
		SUB -> do
			return a
		_ -> error "unexpected token"

rD :: Parser Double
rD = do
	t <- peek
	case t of
		LP -> do
			w1 <- rF
			w2 <- rDP
			return (if (w2 == 1) then w1 else logBase w2 w1)
		(NUMBER _) -> do
			w1 <- rF
			w2 <- rDP
			return (if (w2 == 1) then w1 else logBase w2 w1)
		SUB -> do
			w1 <- rF
			w2 <- rDP
			return (if (w2 == 1) then w1 else logBase w2 w1)
		_ -> error "unexpected token"

rDP :: Parser Double
rDP = do
	t <- peek
	case t of
		LOG -> do
			w1 <- token
			when(w1 /= LOG) $ error "unexpected token"
			w2 <- rD
			return w2
		ADD -> do
			return 1
		DIV -> do
			return 1
		END -> do
			return 1
		MUL -> do
			return 1
		RP -> do
			return 1
		SUB -> do
			return 1
		_ -> error "unexpected token"

rF :: Parser Double
rF = do
	t <- peek
	case t of
		SUB -> do
			w1 <- token
			when(w1 /= SUB) $ error "unexpected token"
			w2 <- rF
			return (-w2)
		LP -> do
			w1 <- token
			when(w1 /= LP) $ error "unexpected token"
			w2 <- rS
			w3 <- token
			when(w3 /= RP) $ error "unexpected token"
			return w2
		(NUMBER _) -> do
			w1 <- token
			let (NUMBER s1) = w1
			return (read s1 :: Double)
		_ -> error "unexpected token"
