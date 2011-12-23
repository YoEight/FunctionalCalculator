-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Data.Functor
import Parsing.Functions
import Parsing.Instances
import Tokens
import Control.Applicative hiding (some, many)



main = do
    input <- return "(1+2)+1"
    parsed <- return $ parse (expression) input
    putStrLn . show $ parsed

