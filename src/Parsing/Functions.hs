-----------------------------------------------------------------------------
--
-- Module      :  Parsing.Functions
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  : Y. Laupa
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
import Control.Applicative hiding (some, many)
--import Control.Monad.State.Strict

character :: Parser Char
character = Parser (\s -> fun s) where
    fun (x:xs) = (Success (xs, x))
    fun _ = Failure "empty input"


get :: Parser String
get = Parser (\s -> return (s, s))

gets :: (String -> a) -> Parser a
gets f = (f) <$> get

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = character >>= (\c -> sat (f c) c) where
    sat True c = return c
    sat _ c = fail ("predicate not satisfied [" ++ show c ++ "]")

is :: Char -> Parser Char
is c = satisfy (== c)

letter = satisfy isLetter

digit = satisfy isDigit

space = satisfy isSpace

spaces = some space

(|||) :: Parser a -> Parser a -> Parser a
ma ||| ma' = Parser (\s -> alter (parse ma s) s) where
        alter r@(Success _) _ = r
        alter _ s = parse ma' s

some :: Parser a -> Parser [a]
some ma = (:) <$> ma <*> (many ma)

many :: Parser a -> Parser [a]
many ma = (some ma) ||| return []

string :: Parser String
string = many letter

number :: Parser Int
number = read <$> (many digit)

between :: Parser a -> Parser b -> Parser c -> Parser c
between open close val = do
    _ <- open
    c <- val
    _ <- close
    return c

skipWhile :: Parser a -> Parser b -> Parser b
skipWhile rubbish val = some rubbish >> val

outer :: Parser a -> Parser b -> Parser c -> Parser (a, c, b)
outer open close val = do
    left <- open
    v <- val
    right <- close
    return (left, v, right)

--ignore :: (Char -> Bool) -> Parser (Maybe Char)
--ignore f = character >>= (\c if f c then Just)


