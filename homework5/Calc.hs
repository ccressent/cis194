module Calc where

import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

eval :: ExprT -> Integer
eval (Lit n)           = n
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Nothing   -> Nothing
    Just expr -> Just (eval expr)
