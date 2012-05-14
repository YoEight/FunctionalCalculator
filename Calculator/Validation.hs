module Calculator.Validation where

import Data.Monoid
import Control.Applicative

data Validation e a = Success a | Failure e

instance (Show e, Show a) => Show (Validation e a) where
  show (Success a) = "Success(" ++ (show a) ++ ")"
  show (Failure e) = "Failure(" ++ (show e) ++ ")"
	
instance Functor (Validation e) where
  fmap f (Success a) = Success $ f a
  fmap f (Failure e) = Failure e
	
instance (Monoid e) => Applicative (Validation e) where
  pure = Success
  (Success f) <*> (Success a) = Success $ f a
  (Failure e) <*> (Failure u) = Failure $ mappend e u
  _           <*> (Failure e) = Failure e
  (Failure e) <*> _           = Failure e

instance Monad (Validation e) where
  return = Success
  (Success a) >>= f = f a
  (Failure e) >>= _ = Failure e
	
validation :: (e -> c) -> (a -> c) -> Validation e a -> c
validation f _ (Failure e) = f e
validation _ f (Success a) = f a 
	