{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char (toLower)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score x) = x

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

score :: Char -> Score
score c
  | toLower c `elem` "aeioulnstr" = Score 1
  | toLower c `elem` "dg"         = Score 2
  | toLower c `elem` "bcmp"       = Score 3
  | toLower c `elem` "fhvwy"      = Score 4
  | toLower c `elem` "k"          = Score 5
  | toLower c `elem` "jx"         = Score 8
  | toLower c `elem` "qz"         = Score 10
  | otherwise                     = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score
