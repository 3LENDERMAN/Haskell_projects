import Data.List (
        minimum
    )

data BinaryTree = 
        Empty 
    |     Leaf Integer 
    |     Node Integer (BinaryTree) (BinaryTree)
    deriving Show

--tested
insert :: BinaryTree -> Integer -> BinaryTree
insert Empty n = Leaf n
insert (Leaf val) n = 
    if n < val 
        then Node val (Leaf n) Empty
        else Node n (Leaf val) Empty
insert (Node val left right) n
    | n < val = Node val (insert left n) right
    | n > val = Node val left (insert right n) 
    | otherwise = Node val left right


--tested
emptyTree :: BinaryTree
emptyTree = Empty


--tested
remove :: BinaryTree -> Integer -> BinaryTree
remove Empty _ = Empty
remove (Leaf val) n
    | n == val = Empty
    | otherwise = Leaf val
remove (Node val left right) n
    | n < val = Node val (remove left n) right
    | n > val = Node val left (remove right n)
    | n == val = removeNode (Node val left right)

removeNode :: BinaryTree -> BinaryTree
removeNode (Node _ Empty right) = right
removeNode (Node _ left Empty) = left
removeNode (Node val left right) = 
    Node (checkRightNode tree) left (getNewRightNode tree)
    where tree = Node val left right

checkRightNode :: BinaryTree -> Integer
checkRightNode (Leaf val) = val
checkRightNode (Node val left right) = val

getNewRightNode :: BinaryTree -> BinaryTree
getNewRightNode (Leaf val) = Empty
getNewRightNode (Node val left right) =
    case (left, right) of
        (_, Empty) -> left
        (Empty, _) -> right
        otherwise -> Node (checkRightNode tree) left (getNewRightNode tree)
        where tree = Node val left right


--tested
containsElement :: BinaryTree -> Integer -> Bool
containsElement Empty _ = False
containsElement (Leaf val) n = val == n
containsElement (Node val left right) n =
    if n < val then
        containsElement left n
    else if n > val then
        containsElement right n
    else True


--tested
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE tree value =
    case list of
        [] -> error "Fuck"
        _ -> minimum list
    where list = listGE tree [] value

listGE :: BinaryTree -> [Integer] -> Integer -> [Integer]
listGE Empty list _ = list
listGE (Leaf val) list spec =
    if val >= spec 
        then (addValToList list val) 
        else list
listGE (Node val left right) list spec =
    if val  >= spec
        then listGE left (listGE right (addValToList list val) spec) spec
        else listGE left (listGE right list spec) spec

addValToList :: [Integer] -> Integer -> [Integer]
addValToList list val = list ++ if (containsInList list val) then [] else [val]

containsInList :: [Integer] -> Integer -> Bool
containsInList list val =
    case list of
        [] -> False
        (head:tail) -> 
            case (head: tail) of
                (_: []) -> False
                _ -> if head == val then True else containsInList tail val


--tested
treeFromList :: [Integer] -> BinaryTree
treeFromList = treeFromList' Empty

treeFromList' :: BinaryTree -> [Integer] -> BinaryTree
treeFromList' tree [] = tree
treeFromList' tree (head : tail) = treeFromList' (insert tree head) tail


--tested
listFromTree :: BinaryTree -> [Integer]
listFromTree = listFromTree' []

listFromTree' :: [Integer] -> BinaryTree -> [Integer]
listFromTree' list Empty  = list
listFromTree' list (Leaf val) = list ++ [val]
listFromTree' list (Node val left right) = [val] ++ listFromTree' (listFromTree' list left) right


main :: IO()
main = interact (\ s -> show $ listFromTree $ emptyTree `insert` 10 `insert` 20 `insert` 30 `remove` 30)
main = interact (\ s -> show $  insert (insert (insert (insert emptyTree 10) 20) 30) 40)
main = interact (\ s -> show $ treeFromList [1, 2, 3, 4, 5])

main = interact (\ s -> show $ treeFromList [2,3,6,54,33,19] `nearestGE` 100)
main = interact (\ s -> show $ listFromTree $ treeFromList [2,3,6,54,33,19] `remove` 33 `remove` 54 `remove` 19 `remove` 2)
main = interact (\ s -> show $ listFromTree (emptyTree `insert` 1 `insert` 2 `insert` 3))