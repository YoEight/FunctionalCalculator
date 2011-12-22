-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Data.Functor
import Parsing
import Tokens

sums n = csums n n
    where
      csums 0 _ = return []
      csums n m = [1..min n m] >>= \x -> (x:) <$> (csums (n-x) x)


main = putStrLn . show $ sums 5

