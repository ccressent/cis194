module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = fibs2' 0 1
    where fibs2' a b = a : fibs2' b (a + b)


data Stream a = Element a (Stream a)

instance Show a => Show (Stream a) where
    show s = (show . take 20 . streamToList) s ++ "..."

streamToList :: Stream a -> [a]
streamToList (Element x rest) = x : streamToList rest

streamRepeat :: a -> Stream a
streamRepeat x = Element x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Element x rest) = Element (f x) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Element seed (streamFromSeed f (f seed))
