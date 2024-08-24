toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x : xs) = 1 + intListLength xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [s] = [s]
doubleEveryOther (x : (y : zs))
  | intListLength (zs) `mod` 2 == 0 = 2 * x : y : doubleEveryOther zs
  | otherwise = x : 2 * y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x >= 10 = 1 + mod x 10
  | otherwise = x
sumDigits (x : xs) = sumDigits [x] + sumDigits xs

-- all above from exercise1 to exercise3
validate :: Integer -> Bool
validate x
  | mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0 = True
  | otherwise = False
