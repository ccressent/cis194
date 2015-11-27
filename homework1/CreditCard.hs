module CreditCard where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0 = map (toInteger . digitToInt) (show n)
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
