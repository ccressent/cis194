{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case msgType of
    "I" -> LogMessage Info              intField1 msgString
    "W" -> LogMessage Warning           intField1 msgString
    "E" -> LogMessage (Error intField1) intField2 errString
    _   -> Unknown s

    where msgType    = (head . words) s
          intField1  = read $ words s !! 1
          intField2  = read $ words s !! 2
          msgString  = (unwords . drop 2 . words) s
          errString  = (unwords . drop 3 . words) s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)
