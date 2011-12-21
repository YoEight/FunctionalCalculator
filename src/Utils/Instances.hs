-----------------------------------------------------------------------------
--
-- Module      :  Utils.Instances
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

module Utils.Instances where

import Control.Applicative

data Validation e a = Failure e | Success a

instance Monad (Validation e) where
    return = Success

    (Failure e) >>= _ = (Failure e)
    (Success a) >>= f = f a

instance Functor (Validation e) where
    fmap f m = m >>= (return . f)

instance Applicative (Validation e) where
    pure = return
    mf <*> ma = mf >>= (\f -> ma >>= \a -> return (f a))

