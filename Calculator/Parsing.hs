{-# LANGUAGE FlexibleContexts #-}

module Calculator.Parsing (compile, Term (..)) where

import Text.Parsec.Expr
import Text.Parsec hiding (State)
import Control.Applicative hiding ((<|>), many)

type Name = String

data Term = Var Name
						| Con Float
						| Neg Term
						| Add Term Term
						| Mul Term Term
						| Div Term Term 
						| Sin Term 
						| Cos Term
						| App Term Term 
						| Lam Name Term
						| Call Name Term deriving (Show)

term :: Stream s m Char => ParsecT s u m Term
term = buildExpressionParser table factor <?> "term"

table :: Stream s m Char => OperatorTable s u m Term
table = [[op "*" (Mul) AssocLeft, op "/" (Div) AssocLeft]
				,[op "+" (Add) AssocLeft, op "-" (\x y -> Add x (Neg y)) AssocLeft]
				]
  where op s f assoc = Infix (string s >> (return f)) assoc

number :: Stream s m Char => ParsecT s u m Term
number = do
	xs <- many1 digit
	return $ Con $ read xs

neg :: Stream s m Char => ParsecT  s u m Term
neg = do
	char '('
	char '-'
	x <- number
	char ')'
	return $ Neg x

callOrVar :: Stream s m Char => ParsecT s u m Term
callOrVar = getName >>= \name -> (fmap (Call name) call) <|> (return $ Var name)
	where
		getName = do
			x  <- letter
			xs <- many (letter <|> digit)
			return $ (x:xs)
		call    = do
			char '('
			arg <- term
			char ')'
			return arg

factor :: Stream s m Char => ParsecT s u m Term
factor = ((skipMany space) *> (parens <|> callOrVar <|> number) <* (skipMany space)) <?> "simple expression"
	where
		inner  = (char '-' >> (fmap Neg term)) <|> term 
		parens = do
			char '('
			x <- inner
			char ')'
			return x
			
compile :: String -> Either ParseError Term
compile = parse term ""
