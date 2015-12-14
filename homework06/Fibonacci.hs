{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Fibonacci where

import Data.List (transpose)

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

instance Num (Stream Integer) where
    fromInteger n = Element n (streamRepeat 0)
    negate = streamMap negate
    (Element x s1)  + (Element y s2)    = Element (x+y) (s1 + s2)
    (Element a0 a') * b@(Element b0 b') = Element (a0*b0) (streamMap (*a0) b' + (a' * b))

instance Fractional (Stream Integer) where
    a@(Element a0 a') / b@(Element b0 b') = Element (a0 `div` b0) (a' - q*b')
        where q = a/b

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

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Given my definition of ruler below, it's necessary to define interleaveStreams
-- without a dependency on the first element of the second stream.
-- That is, a definition like
-- interleaveStreams (E x s1) (E y s2) = E x (E y (interleaveStreams s1 s2))
-- would lead to y being evaluated, which in turn would lead to ruler never
-- terminating, trying to evaluate the first element of the next interleaved
-- stream over and over instead of letting laziness work its magic
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Element x s1) s2 = Element x (interleaveStreams s2 s1)

-- Notice that:
-- ruler is made of a stream S1 interleaved with a stream of 0s
-- S1    is made of a stream S2 interleaved with a stream of 1s
-- S2    is made of a stream S3 interleaved with a stream of 2s
-- S3    is made of a stream S4 interleaved with a stream of 3s
-- and so on...
ruler :: Stream Integer
ruler = ruler' 0
    where ruler' n = interleaveStreams (streamRepeat n) (ruler' (n+1))

x :: Stream Integer
x = Element 0 (Element 1 (streamRepeat 0))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)


data Matrix = Matrix [[Integer]]
    deriving Show

instance Num Matrix where
    Matrix a * Matrix b = Matrix [ [ sum $ zipWith (*) r c | c <- transpose b ] | r <- a ]

fibs4 :: Integer -> Integer
fibs4 0 = 1
fibs4 n = extractFibN $ Matrix [[1,1], [1,0]] ^ n
    where extractFibN (Matrix a) = (!! 1) . head $ a
