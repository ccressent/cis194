{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xs fun1) (GL ys fun2) = GL (xs ++ ys) (fun1 + fun2)

glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs fun) = GL (x : xs) (fun + empFun x)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = case compare gl1 gl2 of
                    GT -> gl1
                    _  -> gl2
