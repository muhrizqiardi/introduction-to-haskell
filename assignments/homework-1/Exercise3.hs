sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = head x + sumDigits(tail x)

main :: IO ()
main = do
  print(sumDigits [1,2,3])

