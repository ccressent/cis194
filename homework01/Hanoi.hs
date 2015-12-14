module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- Move the Hanoi tower of size n from a to b using c as the temporary peg
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a,b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a
