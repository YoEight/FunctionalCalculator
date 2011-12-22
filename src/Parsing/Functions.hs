-----------------------------------------------------------------------------
--
-- Module      :  Parsing.Functions
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

module Parsing.Functions where

import Parsing.Instances
import Utils.Instances
import Data.Char
import Control.Applicative
import Control.Monad.State.Lazy

character :: Parser Char
character = Parser (compute) where
    determine [] = StateT (\_ -> (Failure "empty input"))
    determine (x:xs) = put xs >> return x
    compute = do
        s <- get
        determine s


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = character >>= (\c -> if f c then return c else fail "predicate not satisfied")

is :: Char -> Parser Char
is c = satisfy (== c)

letter = satisfy isLetter

digit = satisfy isDigit

space = satisfy isSpace

spaces = some space

string :: Parser String
string = many letter

number :: Parser Int
number = read <$> (many digit)

between :: Parser a -> Parser b -> Parser c -> Parser c
between open close val = open >> val <* close

skipWhile :: Parser a -> Parser b -> Parser b
skipWhile rubbish val = some rubbish >> val

outer :: Parser a -> Parser b -> Parser c -> Parser (a, c, b)
outer open close val = do
    left <- open
    v <- val
    right <- close
    return (left, v, right)



