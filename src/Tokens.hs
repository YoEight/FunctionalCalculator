-----------------------------------------------------------------------------
--
-- Module      :  Tokens
-- Copyright   :
-- License     :  AllRightsReserved
--
---- Maintainer  : Y. Laupa
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Tokens where

import Parsing.Instances
import Parsing.Functions
import Control.Applicative hiding (some, many)

data Operator = Add | Multiply | Divide | Substract | Modulo deriving (Show)

data Function = Cos | Sin | Tan | Exp deriving (Show)

data Expression = Val Int
                | Op { left :: Expression, op :: Operator, right :: Expression }
                | Fn { func :: Function,  expr :: Expression }
                | Pending { unparsed :: String } deriving (Show)


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
operator = (parenthesis operator) |||
 (outer (value ||| delay) (value ||| delay) (multiply ||| divide ||| plus ||| minus) >>= return . binder) where
    binder (left, op, right) = (Op left op right)

function :: Parser Expression
function = (parenthesis function) ||| do
    name <- (some letter)
    fn   <- func name
    expr <- parenthesis delay
    return (Fn fn expr) where
        func "cos" = return Cos
        func "sin" = return Sin
        func "tan" = return Tan
        func "exp" = return Exp
        func other = fail ("unknown function name => " ++ other)

value :: Parser Expression
value = (parenthesis value) ||| ((some digit) >>= (return . Val . read))

parenthesis :: Parser a -> Parser a
parenthesis ma = between (is '(') (is ')') ma

delay :: Parser Expression
delay = (Pending) <$> (some character)

expression :: Parser Expression
expression = (parenthesis expression) ||| (function ||| operator ||| value)


