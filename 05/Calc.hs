{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as VM

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
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

reify :: ExprT -> ExprT
reify = id  

-- e4
instance Expr Integer where
  lit x     = x 
  add x1 x2 = x1 + x2
  mul x1 x2 = x1 * x2
  
instance Expr Bool where
  lit x     = x > 0
  add x1 x2 = x1 || x2
  mul x1 x2 = x1 && x2

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x                       = MinMax x
  add (MinMax x1) (MinMax x2) = MinMax (max x1 x2)
  mul (MinMax x1) (MinMax x2) = MinMax (min x1 x2)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x                   = Mod7 (mod x 7)
  add (Mod7 x1) (Mod7 x2) = lit (x1 + x2)
  mul (Mod7 x1) (Mod7 x2) = lit (x1 * x2)

-- test
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp 

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

-- e5
instance Expr VM.Program where
  lit x     = [VM.PushI x]
  add x1 x2 = x1 ++ x2 ++ [VM.Add]
  mul x1 x2 = x1 ++ x2 ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
  
