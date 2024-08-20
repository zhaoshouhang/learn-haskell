{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log (LogMessage (..), MessageType (..))

takeOneWord :: String -> String
takeOneWord s = unwords (take 1 (words s))

parseMessage :: String -> LogMessage
-- todo:
parseMessage s =
  case takeOneWord s of
    "I" -> LogMessage Info 123 "abc"
    _ -> Unknown s