module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n)           = n
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Nothing   -> Nothing
    Just expr -> Just (eval expr)
