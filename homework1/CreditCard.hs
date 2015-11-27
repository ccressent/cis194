module CreditCard where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0 = map (toInteger . digitToInt) (show n)
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther [x] = [x]
doubleEveryOther xs  = doubleEveryOther rest ++ [doubled] ++ [last xs]
    where rest    = init (init xs)
          doubled = 2 * last (init xs)
