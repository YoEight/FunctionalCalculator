module Calculator.Parser where

import Prelude hiding (id, (.), head)
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad
import Calculator.Validation
import Data.Char

data Associativity = LeftAssoc | RightAssoc -- | NonAssoc

data Operator a = Prefix (Parser (a -> a))
                  | Postfix (Parser (a -> a))
                  | Infix (Parser (a -> a -> a)) Associativity

type OperatorTable a = [[Operator a]]
				  
newtype Parser a = Parser { parse :: String -> Validation String (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (second f) . p  

instance Applicative Parser where
  pure  = return
  (<*>) = ap
  
instance Monad Parser where
  return a = Parser $ \s -> Success (s, a)
  (Parser pa) >>= f = Parser $ \s -> pa s >>= \(s', a) -> parse (f a) s'
  fail e = Parser $ const Failure e

{-instance MonadPlus Parser where
  mzero = fail ""
  mfail =-} 

instance Alternative Parser where
  empty = fail ""
  (Parser a) <|> (Parser b) = Parser $ \s -> case a s of
                                (Failure _) -> b s
                                r @ _       -> r 
	
head :: Parser Char
head = Parser $ step
  where step (x:xs) = Success (xs, x)
        step _      = Failure "empty input"
		
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- head
  if f c then return c
  else fail "not satisfying predicate"
  
char :: Char -> Parser Char
char c = satisfy (== c)

letter :: Parser Char
letter = satisfy isLetter

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

string :: String -> Parser ()
string (x:xs) = char x >> string xs
string _      = return ()

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

skipMany :: Parser a -> Parser ()
skipMany p = many p >> return ()

buildExpressionParser :: OperatorTable a -> Parser a -> Parser a
buildExpressionParser operators parser = foldl makeParser parser operators
  where makeParser expr ops = 
          let (rassoc, lassoc, preassoc, postassoc) = foldr splitOperators ([], [], [], []) ops
              rassocOp    = choice rassoc
              lassocOp    = choice lassoc
              preassocOp  = choice preassoc
              postassocOp = choice postassoc
		
              prefixP    = preassocOp <|> return id
              postfixP   = postassocOp <|> return id
		
	      rassocP x = do
                f <- rassocOp
                y <- do
                  z <- termP
                  rassocP1 z
                return $ f x y
		  
              lassocP x = do
                f <- lassocOp
                y <- termP
                lassocP1 $ f x y

	      termP = do
                pre  <- prefixP
                x    <- expr
                post <- postfixP
                return $ post $ pre x
		  
              rassocP1 x = rassocP x <|> return x
              lassocP1 x = lassocP x <|> return x 
          in do
            x <- termP
            rassocP x <|> lassocP x <|> return x

splitOperators ::  Operator a -> 
                  ([Parser (a -> a -> a)], [Parser (a -> a -> a)], [Parser (a -> a)], [Parser (a -> a)]) -> 
                  ([Parser (a -> a -> a)], [Parser (a -> a -> a)], [Parser (a -> a)], [Parser (a -> a)])
				  
splitOperators (Prefix op)      (rassoc, lassoc, preassoc, postassoc) = (rassoc, lassoc, op:preassoc, postassoc)
splitOperators (Postfix op)     (rassoc, lassoc, preassoc, postassoc) = (rassoc, lassoc, preassoc, op:postassoc)
splitOperators (Infix op assoc) (rassoc, lassoc, preassoc, postassoc) = case assoc of
  LeftAssoc  -> (rassoc, op:lassoc, preassoc, postassoc)
  RightAssoc -> (op:rassoc, lassoc, preassoc, postassoc)