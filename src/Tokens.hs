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

import Parsing

data Operator = Add | Multiply | Divide | Substract | Modulo deriving (Show)

data Function = Cos | Sin | Tan | Exp deriving (Show)

data Expression = Val Int |
                  Op { left :: Expression, op :: Operator, right :: Expression } ||
                  Fn { func :: Function,  expr :: Expression }

operators = ['+', '-', '*', '/']

plus :: Parser Operator
plus = is '+' >> return Add

minus :: Parser Operator
minus = is '-' >> return Substract

multiply :: Parser Operator
multiply = is '*' >> return Multiply

divide :: Parser Operator
divide = is '/' >> return Divide

operator :: Parser Expression
operator = outer character character (multiply <|> divide <|> plus <|> minus)

exp :: Parser Expression
exp = fail "yo mama"


