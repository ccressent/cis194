module Main where

import Sized
import Scrabble
import JoinList
import Buffer
import Editor

main :: IO ()
main = runEditor editor (mconcat $ map fromString
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String)
