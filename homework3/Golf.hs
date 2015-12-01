module Golf where

every :: Int -> [a] -> [a]
every _ [] = []
every n xs = (take 1 . drop (n-1)) xs ++ every n (drop n xs)

-- Notes:
-- [1..0] = []
-- (`every` xs) :: Int -> [a]
skips :: [a] -> [[a]]
skips xs = map (`every` xs) [1..(length xs)]
