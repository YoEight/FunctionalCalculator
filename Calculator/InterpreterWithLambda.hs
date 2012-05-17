module Calculator.Interpreter (interpret, run, runWithEnv) where

import Prelude hiding (lookup, div)
import Control.Applicative
import Control.Monad.State
import Calculator.Parsing hiding (compile)
import Calculator.Validation

type Name     = String
type Msg      = String
type Env      = [(Name, Value)]
type Result a = Validation String a 

data Value = Fun (Value -> State Env (Result Value))
             | Num Float
						
instance Show Value where
  show (Num n) = "Num(" ++ (show n) ++ ")"
  show (Fun _) = "Fun"
	
lookup :: Name -> Env -> Result Value
lookup n [] = Failure $ "Unknown value '" ++ n ++ "'" 
lookup n ((n', v):xs)
  | n == n'   = Success v
  | otherwise = lookup n xs

add :: Result Value -> Result Value -> Result Value
add l r = pure (\(Num a) (Num b) -> Num $ a + b) <*> l <*> r

mul :: Result Value -> Result Value -> Result Value
mul l r = pure (\(Num a) (Num b) -> Num $ a * b) <*> l <*> r

div :: Result Value -> Result Value -> Result Value
div l r = pure (\(Num a) (Num b) -> Num $ a / b) <*> l <*> (verify =<< r) where
  verify (Num 0) = Failure "Divide by 0"
  verify x       = return x

sin' :: Result Value -> Result Value
sin' = liftA (\(Num x) -> Num $ sin x) 

cos' :: Result Value -> Result Value
cos' = liftA (\(Num x) -> Num $ cos x) 

apply :: Result Value -> Result Value -> State Env (Result Value)
apply rf rx = let applying (Fun k) x = return $ k x
                  applying o _       = Failure $ "Cannot applying '" ++ (show o) ++ "'"
                  compute            = do
                    f <- rf
                    x <- rx
                    applying f x
              in validation (return . Failure) id compute

sinLam = (Lam "x" (Sin (Var "x")))
cosLam = (Lam "x" (Cos (Var "x")))

defaultEnv = [
              ("sin", extract $ runWithEnv [] sinLam),
              ("cos", extract $ runWithEnv [] cosLam)
             ]
  where extract (Success a) = a

pureResult :: a -> State Env (Result a)
pureResult = pure . Success 

interpret :: Term -> State Env (Result Value)
interpret (Con n)      = pureResult $ Num n
interpret (Var x)      = liftA (lookup x) get
interpret (Sin t)      = liftA sin' (interpret t)
interpret (Cos t)      = liftA cos' (interpret t)
interpret (Neg t)      = liftA2 mul (pureResult (Num (-1))) (interpret t) 
interpret (Add ta tb)  = liftA2 add (interpret ta) (interpret tb)   
interpret (Mul ta tb)  = liftA2 mul (interpret ta) (interpret tb)
interpret (Div ta tb)  = liftA2 div (interpret ta) (interpret tb)
interpret (Lam n b)    = pureResult $ Fun (inner n b)
  where inner name body value = do
    modify ((name, value):)
    interpret body
	
interpret (App tf ta)  = do
  f <- interpret tf
  a <- interpret ta
  apply f a
	
interpret (Call n arg) = do
  env <- get
  a   <- interpret arg
  apply (lookup n env) a
	
runWithEnv :: Env -> Term -> Result Value
runWithEnv env t = fst $ runState (interpret t) env

run = runWithEnv defaultEnv