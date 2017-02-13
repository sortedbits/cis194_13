{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- e1
parseMessage :: String -> LogMessage
parseMessage m = case (words m) of
  "I" : (t : xs)      -> LogMessage Info (read t) (unwords xs)
  "W" : (t : xs)      -> LogMessage Warning (read t) (unwords xs)
  "E" : (s : (t: xs)) -> LogMessage (Error (read s)) (read t) (unwords xs)
  xs                  -> Unknown (unwords xs)

parse :: String -> [LogMessage]
parse txt = parseLines (lines txt)

parseLines :: [String] -> [LogMessage]
parseLines []       = []
parseLines (x : xs) = parseMessage x : parseLines xs

-- testParse parse 10 "error.log"

-- e2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree        = tree
insert x Leaf                  = Node Leaf x Leaf
insert x0 (Node left x1 right)
  | timestamp x0 <= timestamp x1 = Node (insert x0 left) x1 right
  | otherwise                    = Node left x1 (insert x0 right)

timestamp :: LogMessage -> Int
timestamp (LogMessage (Error _) t _) = t
timestamp (LogMessage _ t _)         = t
timestamp _                          = error "timestamp of Unknown"

-- e3
build :: [LogMessage] -> MessageTree
build []       = Leaf
build (x : xs) = insert x (build xs)

-- e4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                = []
inOrder (Node left x right) = (inOrder left) ++ [x] ++ (inOrder right)

-- e5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []       = []
whatWentWrong (x : xs) = case x of
  (LogMessage (Error s) _ m) -> if s >= 50 then (m : whatWentWrong xs) else whatWentWrong xs 
  _                          -> whatWentWrong xs
    
-- testWhatWentWrong parse whatWentWrong "error.log"
