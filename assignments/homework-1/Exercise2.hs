doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x =
  [n*2 | n <- x]

main :: IO ()
main = do
  print(doubleEveryOther [8,7,6,5])