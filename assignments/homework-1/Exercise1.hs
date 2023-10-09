toDigits :: Integer -> [Integer]
toDigits n = reverse [((n `mod` 10 ^ x) - (n `mod` 10 ^ (x - 1))) `div` 10 ^ (x - 1) | x <- [1 .. n], 10 ^ (x - 1) <= n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = [((n `mod` 10 ^ x) - (n `mod` 10 ^ (x - 1))) `div` 10 ^ (x - 1) | x <- [1 .. n], 10 ^ (x - 1) <= n]

main :: IO ()
main = do
    let n = 1234 
    putStrLn "Result of toDigits:"
    print (toDigits n)

    putStrLn "Result of toDigitsRev:"
    print (toDigitsRev n)

