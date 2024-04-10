module Lab2.CParser
	( parse, Tree (..) ) where

import Control.Monad.State

import Control.Monad (when)


import Lab2.GraphViz
import Lab2.CLexer (CToken (..))


type Parser a = State [CToken] a

token :: Parser CToken
token = do
	ts <- get
	case ts of
		(t:ts') -> do
			put ts'
			return t
		[] -> return END

peek :: Parser CToken
peek = do
	ts <- get
	case ts of
		(t:_) -> return t
		[] -> return END

parse :: [CToken] -> Tree
parse = evalState rS

rS :: Parser Tree
rS = do
	t <- peek
	case t of
		(NAME _) -> do
			w1 <- token
			let (NAME s1) = w1
			w2 <- rP 
			w3 <- rN 
			w4 <- token
			when(w4 /= SEMICOLON) $ error "unexpected token"
			w5 <- rS 
			return $ Node "S" [Node s1 [], w2, w3, Node ";" [], w5]
		SEMICOLON -> do
			w1 <- token
			when(w1 /= SEMICOLON) $ error "unexpected token"
			w2 <- rS 
			return $ Node "S" [Node ";" [], w2]
		END -> do
			return $ Node "S" []
		_ -> error "unexpected token"

rP :: Parser Tree
rP = do
	t <- peek
	case t of
		POINTER -> do
			w1 <- token
			when(w1 /= POINTER) $ error "unexpected token"
			w2 <- rP 
			return $ Node "P" [Node "*" [], w2]
		(NAME _) -> do
			w1 <- token
			let (NAME s1) = w1
			return $ Node "P" [Node s1 []]
		_ -> error "unexpected token"

rN :: Parser Tree
rN = do
	t <- peek
	case t of
		COMMA -> do
			w1 <- token
			when(w1 /= COMMA) $ error "unexpected token"
			w2 <- rP 
			w3 <- rN 
			return $ Node "N" [Node "," [], w2, w3]
		SEMICOLON -> do
			return $ Node "N" []
		_ -> error "unexpected token"
