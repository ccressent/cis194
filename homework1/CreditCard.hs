module CreditCard where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0 = map (toInteger . digitToInt) (show n)
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse 
    where doubleEveryOther' []       = []
          doubleEveryOther' [x]      = [x]
          doubleEveryOther' (x:y:ys) = x : 2*y : doubleEveryOther' ys

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `rem` 10 == 0
