list :: [Integer]
list = [10^10^10, 2, 3, let inf = inf in inf, 4, 5]

--list !! 1
--list !! 0 X
--elem 4 list X
--list !! 4 
--elem 1 list

--fn :: [Int] -> [Int]
--fn list = zipWith (\x y -> sum (drop x list) : y) $ [0..] list

-- Sum of elements inside the list:
fn list = [sum (drop x list) | x <- [1..(length list)]]
-- via lambda function:
sumList list = map (\x -> sum (drop x list)) list 


funList = map fst (zip (replicate 3 ":") ["A","B","C"])
-- zip will do (:: [a] -> [b] -> [(a,b)]) ->> tj. [(":","A"),(":","B"),(":","C")]

-- local definition of length function::
myLength list = sum (map setit list) where setit x = 1

-- or integrals on input:
onlyInts list = sum (map (\x->x`div`x)list) 

-- take with recursion:
myTake :: Int -> [a] -> [a]
myTake n [] = []
myTake n (x:xs) = if (n>1) then x : myTake (n-1) xs else x:[]
-- drop with recursion:
myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop n (x:xs) = if (n>0) then myDrop (n-1) xs else x : myDrop n xs
-- concat function:
myConcat ::  [[a]] -> [a] 
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs
-- filter function:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x then x : myFilter f xs else myFilter f xs
-- replicate function: 
myReplicate :: Int -> a -> [a]
myReplicate 0 x = [] 
myReplicate n x = x : myReplicate (n-1) x
-- takeWhile function: 
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f (x:xs) = if f x then x : myTakeWhile f xs else []
-- dropWhile function:
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f [] = []
myDropWhile f (x:xs) = if f x then myDropWhile f xs else x : xs
-- map function:
myMap :: (a->b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
-- zip function:
myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (,) x y : myZip xs ys
-- unzip function:
myUnZip :: [(a,b)] -> ([a],[b])
myUnZip [] = ([],[])
myUnZip ((x,y):ys) = (x : fst f,y : snd f) where f = unzip ys   
-- return elements of the list which are on odd indexes
oddMembers :: [a] -> [a]
oddMembers [] = []
oddMembers [x] = [x]
oddMembers (x:y:ys) = x : oddMembers ys
-- Binary Trees:
data BinTree a = Empty | Node a (BinTree a) (BinTree a)
treeOne :: BinTree Int
treeOne = Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty)

treeFunction :: Num a => BinTree a -> BinTree a
treeFunction Empty = Empty
treeFunction (Node x left right) = Node (x+1) (treeFunction left) (treeFunction right)

treezipwith :: (a -> b -> c) -> BinTree a -> BinTree b -> BinTree c
treezipwith op (Node v1 l1 r1) (Node v2 l2 r2) = Node (v1 `op` v2) (treezipwith op l1 l2) (treezipwith op r1 r2)
treezipwith _ _ _ = Empty
-- acumulation functions:
-- foldl, foldr
-- Reverse function made by using recursion
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
-- replicate function
intRep :: Int -> a -> [a]
intRep n x = [ x | y <- [1..n]]
-- head function
myHead :: [a] -> a
myHead [] = error "Empty list!"
myHead (x:_) = x
-- gsd function
myGcd :: Integer -> Integer -> Integer
myGcd x 0 = x
myGcd 0 y = y
myGcd x y = myGcd y (x `mod` y)
-- sum of all members (recursion): 
recSum :: [Int] -> [Int]
recSum [] = []
recSum (x:xs) = sum xs : recSum xs
-- function oddMembers
oddMembers2 :: [a] -> [a]
oddMembers2 [] = []
oddMembers2 [x] = [x]
oddMembers2 (x:y:ys) = x : oddMembers2 ys
-- factorial function
myFactorial :: Integer -> Integer
myFactorial 0 = 1
myFactorial x = x * myFactorial (x-1)
-- take function
myTake2 :: Int -> [a] -> [a]
myTake2 0 _ = []
myTake2 n (x:xs) = 
    if n > 0 
    then x : myTake2 (n-1) xs 
    else myTake2 0 xs

data TreTree a = Nodes (a,a) (TreTree a, TreTree a, TreTree a) | Leaf a
fstTree :: TreTree Int
fstTree = (Nodes (5,6) (Leaf 5, (Nodes (7,9) (Leaf 8, Leaf 4, Leaf 1)), Leaf 2))

sndTree :: TreTree Char
sndTree = (Nodes ('a','b') (Leaf 'f', Leaf 'c', Leaf 't'))

monoLeaf :: Int -> TreTree Int -> Bool
monoLeaf x (Leaf y) = x == y
monoLeaf x (Nodes _ (left, middle, right)) = monoLeaf x left && monoLeaf x middle && monoLeaf x right

-- sum of all lists:
sumLeaf :: TreTree Int -> Int
sumLeaf (Leaf x) = x
sumLeaf (Nodes (a,b) (l,m,r)) = a + b + sumLeaf l + sumLeaf m + sumLeaf r
-- sort all tree values to list:
leafToList :: TreTree a -> [a]
leafToList (Leaf x) = [x]
leafToList (Nodes (a,b) (c,d,e)) = a : b : [] ++ leafToList c ++ leafToList d ++ leafToList e  
-- calculate number of nodes in the tree:
numOfNodes :: TreTree a -> Int
numOfNodes (Leaf x) = 0
numOfNodes (Nodes y (l,m,r)) = 1 + numOfNodes l + numOfNodes m + numOfNodes r
-- double values in leaves:
doubleLeaf :: TreTree Int -> TreTree Int
doubleLeaf (Leaf x) = Leaf (2 * x)
doubleLeaf (Nodes pair (l,m,r)) = (Nodes pair) (doubleLeaf l, doubleLeaf m, doubleLeaf r) 
-- return if x is divisible by y:
isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy x y = if y > 0 then (x `mod` y) == 0 else False
-- 
isEven :: Integer -> Bool
isEven 0 = True
isEven 1 = False
isEven x = isEven (x-2)

-- last function
getLast :: [a] -> a
getLast [x] = x
getLast (x:xs) = getLast xs

nth :: Int -> [a] -> a
nth n (x:xs) = if n == 0 then x else nth (n-1) xs

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) el = if (x == el) then True else contains xs el  

fnRec :: [Int] -> [Int]
fnRec [] = []
fnRec (x:xs) = sum xs : fnRec xs
