module Calculator.Parser.Expression (compile, Term (..)) where

import Calculator.Parser 
import Control.Applicative
import Calculator.Validation

data Term = Con Float
            | Neg Term
            | Add Term Term
            | Mul Term Term  
            | Div Term Term  
            | Sin Term
            | Cos Term deriving (Show)  
                                
                                
operatorTable :: OperatorTable Term
operatorTable = [[infixOp "*" (Mul) LeftAssoc, infixOp "/" (Div) LeftAssoc]
                ,[infixOp "+" (Add) LeftAssoc, infixOp "-" (\x y -> Add x (Neg y)) LeftAssoc]
                ] 
                                
term :: Parser Term
term = buildExpressionParser operatorTable expr

expr :: Parser Term
expr = (skipMany space) *> (parens <|> number) <* (skipMany space)

number :: Parser Term
number = do
  xs <- some digit
  return $ Con $ read xs
  
neg :: Parser Term  
neg = do
  char '('
  char '-'
  x <- term
  char ')'
  return $ Neg x
  
parens :: Parser Term
parens = do
  char '('
  x <- inner
  char ')'
  return x
      where inner = (char '-' >> fmap Neg term) <|> term
  
infixOp :: String -> (a -> a -> a) -> Associativity -> Operator a  
infixOp symbol f assoc = Infix (string symbol >> return f) assoc

compile :: String -> Validation String Term
compile = fmap snd . parse term