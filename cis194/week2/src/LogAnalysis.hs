{-# OPTIONS_GHC -Wall #-}
module LogAnalysis (parseMessage, parse, whatWentWrong) where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case head sWords of
                     "I" -> LogMessage Info firstNum rest
                     "W" -> LogMessage Warning firstNum rest
                     "E" -> LogMessage (Error firstNum) (read $ sWords !! 2) (unwords $ drop 3 sWords)
                     _   -> Unknown s
                    where sWords = words s
                          firstNum = read $ sWords !! 1
                          rest = unwords $ drop 2 sWords

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf        = Node Leaf lm Leaf
insert lm1@(LogMessage _ ts1 _) (Node l lm2@(LogMessage _ ts2 _) r)
    | ts1 <= ts2      = Node (insert lm1 l) lm2 r
    | otherwise       = Node l lm2 (insert lm1 r)

build :: [LogMessage] -> MessageTree
build xs = foldr insert Leaf xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf          = []
inOrder (Node l lm r) = (inOrder l) ++ [lm] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong l = case inOrder $ build l of
                         [] -> []
                         ((LogMessage (Error sev) _ msg):xs) -> if sev >= 50
                                                                    then msg : whatWentWrong xs
                                                                    else whatWentWrong xs
                         (_:xs) -> whatWentWrong xs

