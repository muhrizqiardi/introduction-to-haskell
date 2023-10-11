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
    ('E' : s) ->
      let msgList = split ' ' msgStr
       in LogMessage (Error (read (msgList !! 1) :: Int)) (read (msgList !! 2) :: Int) (join " " (drop 3 msgList))
    ('I' : s) ->
      let msgList = split ' ' msgStr
       in LogMessage Info (read (msgList !! 1) :: Int) (join " " (drop 2 msgList))
    ('W' : s) ->
      let msgList = split ' ' msgStr
       in LogMessage Warning (read (msgList !! 1) :: Int) (join " " (drop 2 msgList))

parse :: String -> [LogMessage]
parse "" = []
parse logs =
  [parseMessage log | log <- split '\n' logs]

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ timestamp _) = timestamp

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log Leaf = Node Leaf log Leaf
insert log tree =
  let treeTimeStamp = getTimeStamp ((\(Node _ log _) -> log) tree)
      logTimeStamp = getTimeStamp log
   in if logTimeStamp > treeTimeStamp
        then Node Leaf log tree
        else Node tree log Leaf

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x : xs) = insert x (build xs)
