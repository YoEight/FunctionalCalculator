-----------------------------------------------------------------------------
--
-- Module      :  Tokens
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Tokens where

import Parsing.Instances
import Parsing.Functions
import Control.Applicative

data Operator = Add | Multiply | Divide | Substract | Modulo deriving (Show)

data Function = Cos | Sin | Tan | Exp deriving (Show)

data Expression = Val Int
                | Op { left :: Expression, op :: Operator, right :: Expression }
                | Fn { func :: Function,  expr :: Expression }

plus :: Parser Operator
plus = is '+' >> return Add

minus :: Parser Operator
minus = is '-' >> return Substract

multiply :: Parser Operator
multiply = is '*' >> return Multiply

divide :: Parser Operator
divide = is '/' >> return Divide

modulo :: Parser Operator
modulo = is '%' >> return Modulo

operator :: Parser Expression
operator = outer expression expression (multiply <|> divide <|> plus <|> minus) >>= return . binder where
    binder (left, op, right) = (Op left op right)

function :: Parser Expression
function = do
    name <- (many letter)
    fn   <- func name
    expr <- between (is '(') (is ')') expression
    return (Fn fn expr) where
        func "cos" = return Cos
        func "sin" = return Sin
        func "tan" = return Tan
        func "exp" = return Exp
        func other = fail ("unknown function name => " ++ other)

value :: Parser Expression
value = (many digit) >>= (return . Val . read)

parenthesis :: Parser Expression
parenthesis = function <|> (between (is '(') (is ')') expression)

expression :: Parser Expression
expression = fail "yo mama"


