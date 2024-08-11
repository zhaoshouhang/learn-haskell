intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x : xs) = 1 + intListLength xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [s] = [s]
doubleEveryOther (x : (y : zs))
  | intListLength (zs) `mod` 2 == 0 = 2 * x : y : doubleEveryOther zs
  | otherwise = x : 2 * y : doubleEveryOther zs