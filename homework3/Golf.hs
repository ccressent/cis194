module Golf where

import Data.List

-- Take 1 element from the list every n, starting at the first
every :: Int -> [a] -> [a]
every _ [] = []
every n xs = (take 1 . drop (n-1)) xs ++ every n (drop n xs)

skips :: [a] -> [[a]]
skips xs = map (`every` xs) [1..(length xs)]


localMaxima :: [Integer] -> [Integer]
localMaxima l
    | length l < 3 = []
localMaxima (x:y:z:xs)
    | y > x && y > z = y : localMaxima (y:z:xs)
    | otherwise      = localMaxima (y:z:xs)


histogram :: [Int] -> String
histogram xs = bars r ++ "==========\n0123456789\n"
    where r = map (\xs -> (head xs, length xs)) $ group $ sort xs

bars :: [(Int, Int)] -> String
bars [] = []
bars ps = bars (decrement ps) ++ formatLine ps ++ "\n"

formatLine :: [(Int, Int)] -> String
formatLine [] = ""
formatLine ps = map (\x -> case lookup x ps of
    Nothing -> ' '
    Just _  -> '*') [0..9]

decrement :: [(Int, Int)] -> [(Int, Int)]
decrement []       = []
decrement ((a,b):xs)
    | b > 1     = (a, b-1) : decrement xs
    | otherwise = decrement xs
