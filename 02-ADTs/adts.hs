-- Enumeration types
data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
-- _ 代表剩下的不是上面的类型的都是True
isSmall2 _ = True

-- Beyond enumerations

data FailableDouble
  = Failure
  | OK Double
  deriving (Show)

ex01 :: FailableDouble
ex01 = Failure

ex02 :: FailableDouble
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- Store a person's name, age, and favourite Thing.
data Person = Person String Int Thing
  deriving (Show)

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- 通用的定义形式
-- data AlgDataType
--   = Constr1 Type11 Type12
--   | Constr2 Type21
--   | Constr3 Type31 Type32 Type33
--   | Constr4

-- 模式匹配..
-- foo (Constr1 a b)   = ...
-- foo (Constr2 a)     = ...
-- foo (Constr3 a b c) = ...
-- foo Constr4         = ...

baz :: Person -> String
-- 这里的p就代表后面的对象了
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _) = n ++ ", your favorite thing is lame."

-- 通配的方式有下面的
-- pat ::= _
--      |  var
--      |  var @ ( pat )
--      |  ( Constructor pat1 pat2 ... patn )

-- Case expressions

-- case exp of
--   pat1 -> exp1
--   pat2 -> exp2
--   ...

ex03 = case "Hello" of
  [] -> 3
  ('H' : s) -> length s
  _ -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d -> d

-- Recursive data types

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

data Tree
  = Leaf Char
  | Node Tree Int Tree
  deriving (Show)

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))