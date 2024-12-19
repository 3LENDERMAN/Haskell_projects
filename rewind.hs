myTake :: Int -> [a] -> [a]
myTake n [] = []
myTake n (x:xs) = if (n > 0) then x : myTake (n-1) xs else []

intTake n xs = [x | (x, i) <- zip xs [0..n-1]]

mydrop :: Int -> [a] -> [a]
mydrop n [] = []
mydrop n (x:xs) = if n > 0 then mydrop (n-1) xs else x:xs

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a->Bool) -> [a] -> [a]
myfilter p [] = []
myfilter p (x:xs) = if p x then x:myfilter p xs else myfilter p xs

intfilter p list = [x | x<-list,p x]

