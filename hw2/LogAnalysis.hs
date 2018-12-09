{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- ex1 --
parseHelper :: [String] -> LogMessage
parseHelper wordList = case wordList of
    ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
    ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
    ("E":level:ts:msg) -> LogMessage (Error (read level)) (read ts) (unwords msg)
    _ -> Unknown (unwords wordList)

parseMessage :: String -> LogMessage
parseMessage = parseHelper . words

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- ex2 --
insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree = case msg of
    Unknown _ -> tree
    LogMessage _ t _ -> case tree of
                            Leaf -> Node Leaf msg Leaf
                            Node lt mid@(LogMessage _ nt _) rt -> if t <= nt
                                                            then Node (insert msg lt) mid rt
                                                            else Node lt mid (insert msg rt)
                            Node lt mid rt -> Node lt mid rt

-- ex3 --
build :: [LogMessage] -> MessageTree
build lst = case lst of
    [] -> Leaf
    (x:xs) -> insert x (build xs)


-- ex4 --
inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
    Leaf -> []
    Node l msg r -> (inOrder l) ++ [msg] ++ (inOrder r)

-- ex5 --
wrongFilter :: LogMessage -> Bool
wrongFilter msg = case msg of
    LogMessage (Error level) _ _ -> if level > 50 then True else False
    _ -> False
wrongMap :: LogMessage -> String
wrongMap (LogMessage _ _ msg) = msg
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map wrongMap) . (filter wrongFilter) . inOrder . build

