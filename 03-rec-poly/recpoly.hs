data IntList = Empty | Cons Int IntList
  deriving (Show)

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1

square x = x * x

-- 实际就是map
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList func (Cons x xs) = Cons (func x) (mapIntList func xs)

result = mapIntList addOne exampleList

result1 = mapIntList abs exampleList

result2 = mapIntList square exampleList

-- 接下来是filter
