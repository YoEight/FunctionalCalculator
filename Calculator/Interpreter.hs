module Calculator.Interpreter (interpret) where

import Calculator.Parser.Expression (Term (..))
import Calculator.Validation
import Control.Monad
		
interpret :: Term -> Validation String Float
interpret (Con n)   = return n
interpret (Neg t)   = fmap (* (-1)) (interpret t) 
interpret (Sin t)   = fmap sin (interpret t)
interpret (Cos t)   = fmap cos (interpret t)
interpret (Add x y) = liftM2 (+) (interpret x) (interpret y) 
interpret (Mul x y) = liftM2 (*) (interpret x) (interpret y)
interpret (Div x y) = do
  a <- interpret x
  b <- interpret y
  if b == 0 then Failure "Divide by 0"
                 else return $ a / b
  
	