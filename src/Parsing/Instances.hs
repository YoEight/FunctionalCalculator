-----------------------------------------------------------------------------
--
-- Module      :  Parsing.Instances
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

module Parsing.Instances where

import Utils.Instances
import Data.Char
import Control.Applicative
import Data.Monoid
import Control.Monad.State.Lazy


newtype Parser a = Parser { computation :: StateT String (Validation String) a }

runParser :: Parser a -> String -> Validation String a
runParser = evalStateT . computation

instance Monad Parser where
    return a = Parser (return a)
    (Parser ms) >>= f = Parser (ms >>= (computation . f))

    fail msg = Parser (StateT (\_ -> (Failure msg)))

instance Functor Parser where
    fmap f m = m >>= (return . f)

instance Applicative Parser where
    pure = return
    mf <*> ma = mf >>= (\f -> ma >>= \a -> return (f a))

instance Monoid a => Monoid (Parser a) where
    mempty = fail "empty"
    ma `mappend` ma' = return (mappend) <*> ma <*> ma'

instance Alternative (Parser) where
    empty = fail "error"
    ma <|> ma' = Parser (get >>= (\s -> fun (runStateT (computation ma) s) s)) where
        fun  r@(Success _) _ = StateT (\_ -> r)
        fun (Failure _) s = StateT (\_ -> runStateT (computation ma') s)


