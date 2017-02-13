{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- e1
parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  "I" : (t : xs)      -> LogMessage Info (read t) (unwords xs)
  "W" : (t : xs)      -> LogMessage Warning (read t) (unwords xs)
  "E" : (s : (t: xs)) -> LogMessage (Error (read s)) (read t) (unwords xs)
  xs                  -> Unknown (unwords xs)

parse :: String -> [LogMessage]
parse s = parseLines (lines s)

parseLines []       = []
parseLines (x : xs) = parseMessage x : parseLines xs
