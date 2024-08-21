{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log (LogMessage (..), MessageTree (..), MessageType (..), testParse, testWhatWentWrong)

-- exercise 1
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

-- exercise2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert _ (Node _ (Unknown _) _) = Leaf
insert lm@(LogMessage _ t _) (Node mt1 logMsg@(LogMessage _ t1 _) mt2)
  -- 必须生成新的 才行 之前直接使用insert lm mt1 就是错误的
  | t1 > t = Node (insert lm mt1) logMsg mt2
  | otherwise = Node mt1 logMsg (insert lm mt2)

-- exercise3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x : xs) = insert x (build xs)

-- exercise4

inOrder :: MessageTree -> [LogMessage]
-- 处理空
inOrder Leaf = []
-- 这段代码的问题是用了多余的模式匹配,另外 最后处理 node node 情况的时候没有处理lm的数据导致漏掉了一些数据
-- inOrder
--   (Node mtl lm mtr) =
--     case (mtl, mtr) of
--       (Leaf, Leaf) -> [lm]
--       (Node {}, Leaf) -> inOrder mtl ++ [lm]
--       (Leaf, Node {}) -> lm : inOrder mtr
--       (Node {}, Node {}) -> inOrder mtl ++ inOrder mtr
-- 修改后
inOrder
  (Node mtl lm mtr) = inOrder mtl ++ [lm] ++ inOrder mtr

-- exercise5
errMoreThan50 :: LogMessage -> Bool
errMoreThan50 l = case l of
  Unknown _ -> False
  LogMessage m _ _ -> case m of
    Error i -> i > 50
    _ -> False

getMsg :: LogMessage -> String
getMsg l = case l of
  Unknown _ -> ""
  LogMessage _ _ s -> s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong [Unknown _] = []
whatWentWrong s = map getMsg (filter errMoreThan50 (inOrder (build s)))
