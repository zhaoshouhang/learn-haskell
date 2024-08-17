type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =
  hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

-- Main function
main :: IO ()
main = do
  let moves = hanoi 3 "a" "b" "c" -- Calculate moves for 3 disks
  print moves -- Print the list of moves