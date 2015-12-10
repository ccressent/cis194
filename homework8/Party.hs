{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xs fun1) (GL ys fun2) = GL (xs ++ ys) (fun1 + fun2)

glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs fun) = GL (x : xs) (fun + empFun x)
