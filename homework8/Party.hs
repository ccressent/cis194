module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs fun) = GL (x : xs) (fun + empFun x)
