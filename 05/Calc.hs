{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

-- e1
eval :: ExprT -> Integer
eval (Lit x)     = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- e2
evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Just e -> Just (eval e)
  _      -> Nothing 

-- or simply
evalStr' :: String -> Maybe Integer
evalStr' = fmap eval . parseExp Lit Add Mul
    
-- e3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x     = Lit x
  mul e1 e2 = Add e1 e2
  add e1 e2 = Mul e1 e2

reify :: ExprT -> ExprT
reify = id  
