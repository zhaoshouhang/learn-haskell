sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x >= 10 = 1 + (mod x 10)
  | otherwise = x
sumDigits (x : xs) = sumDigits [x] + sumDigits xs