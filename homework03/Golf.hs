module Golf where

import Data.List (group, sort, transpose)
import Data.Maybe (fromMaybe)

-- Take 1 element from the list every n, starting at the first
every :: Int -> [a] -> [a]
every _ [] = []
every n xs = (take 1 . drop (n-1)) xs ++ every n (drop n xs)

skips :: [a] -> [[a]]
skips xs = map (`every` xs) [1..(length xs)]


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | y > x && y > z = y : localMaxima (y:z:xs)
    | otherwise      = localMaxima (y:z:xs)
localMaxima _ = []


histogram :: [Int] -> String
histogram xs = unlines $ reverse $ transpose $ toBars r
    where r = map (\l -> (head l, length l)) $ group $ sort xs

toBars :: [(Int,Int)] -> [String]
toBars ps = map (`toBar` ps) [0..9]

toBar :: Int -> [(Int,Int)] -> String
toBar n ps = show n ++ "=" ++ replicate b '*' ++ replicate (10-b) ' ' 
    where b = fromMaybe 0 (lookup n ps)
