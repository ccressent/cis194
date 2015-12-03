module Homework where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Int] -> Int
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)

fun2':: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
    where f x
            | even x    = x `div` 2
            | otherwise = 3*x + 1


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert val Leaf = Node 0 Leaf val Leaf
insert val (Node h left this right)
    | height left < height right = Node h (insert val left) this right
    | height left > height right = Node h left this (insert val right)
    | otherwise                  = Node (1 + height r') left this r'
    where r' = insert val right

height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

isBalanced :: Tree a -> Bool
isBalanced Leaf                  = True
isBalanced (Node _ left _ right) = abs (height left - height right) <= 1
                                   && isBalanced left
                                   && isBalanced right


xor :: [Bool] -> Bool
xor = odd . foldr (\p acc -> if p then acc + 1 else acc) (0 :: Int)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (`notElem` excluded) [1..n]
    where excluded = filter (<= n) [i + j + 2*i*j | i <- [1..n], j <- [i..n]]
