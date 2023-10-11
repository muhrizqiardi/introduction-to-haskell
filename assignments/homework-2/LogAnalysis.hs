module LogAnalysis where

import Log

split :: Char -> String -> [String]
split _ "" = []
split at x = let (l, r) = break (at ==) x in (l : split at (drop 1 r))

join :: String -> [String] -> String
join _ [] = ""
join _ [""] = ""
join c strList =
  if length strList > 1
    then head strList ++ c ++ join c (drop 1 strList)
    else head strList

parseMessage :: String -> LogMessage
parseMessage msgStr =
  case msgStr of
    "" -> Unknown msgStr
    ('E' : s) -> let msgList = split ' ' msgStr in LogMessage (Error (read (msgList !! 1) :: Int)) (read (msgList !! 2) :: Int) (join " " (take 3 msgList))
    ('I' : s) -> let msgList = split ' ' msgStr in LogMessage Info (read (msgList !! 1) :: Int) (join " " (take 3 msgList))
    ('W' : s) -> let msgList = split ' ' msgStr in LogMessage Warning (read (msgList !! 1) :: Int) (join " " (take 3 msgList))

parse :: String -> [LogMessage]
parse "" = []
parse logs =
  [parseMessage log | log <- split '\n' logs]
