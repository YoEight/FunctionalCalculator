module Calculator.Interpreter (interpret, run, runWithEnv) where

import Prelude hiding (lookup, div)
import Control.Applicative
import Control.Monad.State
import Calculator.Parsing hiding (compile)

type Name = String
type Msg  = String
type Env  = [(Name, Value)] 

data Value = Wrong
						| Fun (Value -> State Env Value)
						| Num Float
						
instance Show Value where
	show Wrong   = "Wrong"
	show (Num n) = "Num(" ++ (show n) ++ ")"
	show (Fun _) = "Fun"
	
lookup :: Name -> Env -> Value
lookup _ [] = Wrong
lookup n ((n', v):xs)
	| n == n'   = v
	| otherwise = lookup n xs

add :: Value -> Value -> Value
add (Num a) (Num b) = Num $ a + b
add _ _             = Wrong

mul :: Value -> Value -> Value
mul (Num a) (Num b) = Num $ a * b
mul _ _             = Wrong

div :: Value -> Value -> Value
div (Num a) (Num b) = Num $ a / b
div _ _             = Wrong

sin' :: Value -> Value
sin' (Num x) = Num $ sin x

cos' :: Value -> Value
cos' (Num x) = Num $ cos x

apply :: Value -> Value -> State Env Value
apply (Fun k) a = k a
apply _ _       = return Wrong

sinLam = (Lam "x" (Sin (Var "x")))
cosLam = (Lam "x" (Cos (Var "x")))

defaultEnv = [
							("sin", runWithEnv [] sinLam),
							("cos", runWithEnv [] cosLam)
						 ]

interpret :: Term -> State Env Value
interpret (Con n)      = pure $ Num n
interpret (Var x)      = liftA (lookup x) get
interpret (Sin t)      = liftA sin' (interpret t)
interpret (Cos t)      = liftA cos' (interpret t)
interpret (Neg t)      = liftA2 mul (pure (Num (-1))) (interpret t) 
interpret (Add ta tb)  = liftA2 add (interpret ta) (interpret tb)   
interpret (Mul ta tb)  = liftA2 mul (interpret ta) (interpret tb)
interpret (Div ta tb)  = liftA2 div (interpret ta) (interpret tb)
interpret (Lam n b)    = return $ Fun (inner n b)
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
	
runWithEnv :: Env -> Term -> Value
runWithEnv env t = fst $ runState (interpret t) env

run = runWithEnv defaultEnv