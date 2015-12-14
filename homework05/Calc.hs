{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import Data.Map as M

import ExprT
import VarExprT
import Parser
import StackVM as VM


class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

class HasVars a where
    var :: String -> a


instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance HasVars VarExprT where
    var = VarExprT.Var

instance Expr VarExprT where
    lit = VarExprT.Lit
    add = VarExprT.Add
    mul = VarExprT.Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n _ = Just n

    add expr1 expr2 vs = case expr1 vs of
                           Nothing -> Nothing
                           Just x  -> case expr2 vs of
                                        Nothing -> Nothing
                                        Just y  -> Just (x+y)

    mul expr1 expr2 vs = case expr1 vs of
                           Nothing -> Nothing
                           Just x  -> case expr2 vs of
                                        Nothing -> Nothing
                                        Just y  -> Just (x*y)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7   = Mod7   Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

instance Expr Program where
    lit n           = [VM.PushI n]
    add prog1 prog2 = prog1 ++ prog2 ++ [VM.Add]
    mul prog1 prog2 = prog1 ++ prog2 ++ [VM.Mul]


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7


eval :: ExprT -> Integer
eval (ExprT.Lit n)           = n
eval (ExprT.Add expr1 expr2) = eval expr1 + eval expr2
eval (ExprT.Mul expr1 expr2) = eval expr1 * eval expr2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp ExprT.Lit ExprT.Add ExprT.Mul s of
    Nothing   -> Nothing
    Just expr -> Just (eval expr)

compile :: String -> Maybe Program
compile = parseExp lit add mul

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
