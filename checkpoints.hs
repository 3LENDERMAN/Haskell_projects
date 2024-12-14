import Data.Char
-- 1. lecture checkpoint
-- Function to check if the number is proper divisor or not:
isDivisior :: Integer -> Integer -> Bool 
isDivisior x y = if (x `div` y) * y == x then True else False

-- 2. lecture checkpoint
-- return second element otherwise return error:
retSec :: [b] -> b
retSec [] = error "Empty list!"
retSec [x] = error "Only one input!"
retSec (x:y:z) = y

-- 3. Lecture checkpoint
-- double first element:
doubleFst :: [a] -> [a]
doubleFst x = (head x) : x 
-- duplicate the list and add first el at the end:
doubleAddFst :: [a] -> [a]
doubleAddFst x = x ++ x ++ take 1 x 
-- function switches first two elements inside the list:
changeTwo :: [a] -> [a]
changeTwo x = head (tail x) : head x : tail (tail x)
-- function switches first element with the last one:
changeFsLs :: [a] -> [a]
changeFsLs x = last x : init (tail x ) ++ (head x : [])
-- length function alternative: 
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
-- is plaindrome predicate
isPalindrom :: Eq a => [a] -> Bool
isPalindrom x = x == (reverse x)
-- is product smaller than additon of elements inside the list: 
isLarger :: (Ord a, Num a) => [a] -> Bool
isLarger x = if (sumIt x) > (productIt x) then True else False

productIt :: Num a => [a] -> a
productIt [] = 1
productIt (x:xs) = x * productIt xs

sumIt :: Num a => [a] -> a
sumIt [] = 0
sumIt (x:xs) = x + sumIt xs
-- all elements in the list are even
isEven :: Integral a => [a] -> Bool
isEven [] = True
isEven (x:xs) = if x `mod` 2 == 0 then isEven xs else False
-- switch all tuples inside list:
changePair :: [(a,b)] -> [(b,a)]
changePair pairs = map swap pairs  
    where 
      swap :: (a,b) -> (b,a)
      swap (a,b) = (b,a)
-- list is ascending:
fn :: Ord a => [a] -> Bool 
fn s = and (map f (zip s (head s:s)))
    where f (a,b) = a >= b

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs 
-- takes element
takeit :: Int -> [a] -> [a]
takeit _ [] = []
takeit n (x:xs) = if (n>0) then x : take (n-1) xs else []

-- Checkpoint 3 : 
-- element times first parameter, then return only divisible by second par: 
listFun :: Integral a => a -> a -> [a] -> [a]
listFun divider prod list = filter (fun divider) (map (*prod) list)
    where   
      fun :: Integral a => a -> a -> Bool
      fun z y = y `mod` z == 0 

-- list contains at least two strings of length >4
minList :: [String] -> Int -> Int -> Bool
minList x y z = 
    if (length x >= y) && (filter (>=z) (map length x) == (map length x)) 
    then True 
    else False

-- Checkpoint 4 : 
-- lambda abstraction:
-- left section
--leftSection :: (b -> c) -> (a -> b -> d) -> a -> c
-- right section
--rightSection :: (a -> b -> c) -> b -> (a -> c)

data TreTree a = Node (a,a) (TreTree a, TreTree a, TreTree a) | Leaf a

mone :: Int -> TreTree Int -> Bool
mone n (Leaf x) = (x == n)
mone n (Node _ (x,y,z)) = (mone n x) && (mone n y) && (mone n z)

addone :: TreTree Int -> TreTree Int 
addone (Leaf x) = Leaf (x + 1)
addone (Node (a,b) (x,y,z)) = Node (a,b) (addone x, addone y, addone z)

myTake :: Int -> [a] -> [a]
myTake n [] = []
myTake n (x:xs) = if n > 0 then x : myTake (n-1) xs else []

myMap :: (a->b) -> [a] -> [b] 
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

mapp fun list = [fun x | x <- list]
lengg list = sum [x | x <- list, let x = 1]

lengthh :: [a] -> Int
lengthh [] = 0
lengthh (x:xs) = 1 + lengthh xs

filta :: (a->Bool)->[a]->[a]
filta predicate [] = []
filta predicate (x:xs) = if (predicate x) then x : filta predicate xs 
                         else filta predicate xs

intFil predicate list = [x | x <- list, predicate x]