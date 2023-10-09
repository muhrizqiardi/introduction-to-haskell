toDigits :: Integer -> [Integer]
toDigits n = reverse [((n `mod` 10 ^ x) - (n `mod` 10 ^ (x - 1))) `div` 10 ^ (x - 1) | x <- [1 .. n], 10 ^ (x - 1) <= n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = [((n `mod` 10 ^ x) - (n `mod` 10 ^ (x - 1))) `div` 10 ^ (x - 1) | x <- [1 .. n], 10 ^ (x - 1) <= n]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x =
  [n*2 | n <- x]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = head x + sumDigits(tail x)

validate :: Integer -> Bool
validate x = 
  sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0 

main = do
  print(validate 4012888888881881)
