module Calc where

import ExprT

eval :: ExprT -> Integer
eval (Lit n)           = n
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2
