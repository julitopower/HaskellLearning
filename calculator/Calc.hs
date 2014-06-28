{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where
import ExprT
import Parser (parseExp)
import StackVM (StackVal(..), StackExp(..), Program, stackVM)

-- Type class for arithmetic expressions
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- Our symbolic expressions can be treated as arithmetic types
instance Expr ExprT where
  lit x = ExprT.Lit x
  add x y = ExprT.Add x y
  mul x y = ExprT.Mul x y

-- Integets can be treated as arithmetic
instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y
  
-- This allow the compilation of expressions into programs
instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

-- A not polymorphic expression evaluation
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = operate (+) x y
eval (ExprT.Mul x y) = operate (*) x y

operate :: (Integer -> Integer -> Integer) -> ExprT -> ExprT -> Integer
operate f a b = f (eval a) (eval b) 

evalStr :: String -> Maybe Integer
evalStr = evalExpr . parseExp Lit ExprT.Add ExprT.Mul
   
evalExpr :: Maybe ExprT -> Maybe Integer
evalExpr (Just e) = Just (eval e)
evalExpr Nothing = Nothing
---------------------------------------------------------------             

-- The following code demonstrates how to evaluate an expression
-- into different types by taking advantage of type classes and
-- polimorphism
testExp :: Expr a => Maybe a                   
testExp = parseExp lit add mul "(3 * -4) + 5"

-- Evaluate as integer
testInteger = testExp :: Maybe Integer
-- Evaluate as program
testProgram = testExp :: Maybe Program

-- Provide user friendly compilation interface
compile :: String -> Maybe Program
compile s = e::Maybe Program
  where e = parseExp lit add mul s

-- Execute program in VM
run :: Maybe Program -> Either String StackVM.StackVal
run (Just p) = stackVM p
run Nothing = Left "Invalid Input"