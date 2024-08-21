{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log (LogMessage (..), MessageType (..), testParse)

-- 提取某个位置上的单词 pos 从1开始
takeWords :: String -> Int -> String
takeWords s pos
  | pos > 1 = takeWords (unwords (drop 1 (words s))) (pos - 1)
  | otherwise = unwords (take 1 (words s))

-- 提取某个位置后剩下的所有单词
dropWords :: String -> Int -> String
dropWords s pos = unwords (drop pos (words s))

parseMessage :: String -> LogMessage
parseMessage s =
  case takeWords s 1 of
    "I" -> LogMessage Info (read (takeWords s 2)) (dropWords s 2)
    "W" -> LogMessage Warning (read (takeWords s 2)) (dropWords s 2)
    "E" -> LogMessage (Error (read (takeWords s 2))) (read (takeWords s 3)) (dropWords s 3)
    _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines