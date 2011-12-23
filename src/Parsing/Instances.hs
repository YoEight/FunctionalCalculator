-----------------------------------------------------------------------------
--
-- Module      :  Parsing.Instances
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

module Parsing.Instances where

import Utils.Instances
import Data.Char
import Control.Applicative
import Data.Monoid


newtype Parser a = Parser { parse :: String -> Validation String (String, a) }

instance Monad Parser where
    return a = Parser (\s -> (Success (s, a)))
    ma >>= f = Parser (\s -> binder (parse ma s)) where
        binder (Success (s', a)) = parse (f a) s'
        binder (Failure e) = (Failure e)

    fail msg = Parser (const (Failure msg))

instance Functor Parser where
    fmap f m = m >>= (return . f)

instance Applicative Parser where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return $ f a

--instance Monoid a => Monoid (Parser a) where
--    mempty = fail "empty"
--    ma `mappend` ma' = (mappend) <$> ma <*> ma'

--instance Alternative (Parser) where
--    empty = fail "error"
--    ma <|> ma' = Parser (\s -> alter (parse ma s) s) where
--        alter r@(Success _) _ = r
--        alter _ s = parse ma' s


